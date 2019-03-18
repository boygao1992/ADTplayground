[Pure-functional transformations of cyclic graphs and the Credit Card Transform](http://okmij.org/ftp/Haskell/AlgorithmsH.html)

Transformations of cyclic graphs and the Credit Card Transform

Cycles certainly make it difficult to transform graphs in a pure
non-strict language. Cycles in a source graph require us to devise a
way to mark traversed nodes -- however we cannot mutate nodes and
cannot even compare nodes with a generic ('derived') equality
operator. Cycles in a destination graph require us to keep track of
the already constructed nodes so we can complete a cycle. An obvious
solution is to use a state monad and IORefs. There is also a
monad-less solution, which is less obvious: seemingly we cannot add a
node to the dictionary of already constructed nodes until we have
built the node. This fact means that we cannot use the updated
dictionary when building the descendants of the node -- which need the
updated dictionary to link back. The problem can be overcome however
with a _credit card transform_ (a.k.a. "buy now, pay later"
transform). To avoid hitting the bottom, we just have to "pay" by the
"due date".

For illustration, we will consider the problem of printing out a
non-deterministic finite automaton (NFA) and transforming it into a
deterministic finite automaton (DFA). Both NFA and DFA are represented
as cyclic graphs. The problem has been discussed on the
Haskell/Haskell-Cafe mailing lists. The automata in question were to
recognize strings over a binary alphabet.

> module CCardFA where

> import Data.List


A state of an automaton over a binary alphabet is a data structure:

> data FaState l = FaState
>   { label :: l
>   , acceptQ :: Bool
>   , trans0:: [FaState l]
>   , trans1:: [FaState l]
>   }

whose fields have the obvious meaning. Label is used for printing out
and comparing states. The flag acceptQ tells if the state is
final. Since an FaState can generally represent a non-deterministic
automaton, transitions are the _lists_ of states.

An automaton is then a list of starting states.

> type FinAu l = [FaState l]

For example, an automaton equivalent to the regular expression
"0*(0(0+1)*)*" could be defined as:

> dom18 = [one]
>    where one = FaState 1 True [one,two] []
>	   two = FaState 2 True [two,one] [one,two]

using the straightforward translation from a regular expression to an NFA.

We would like to compare and print automata and their states:

> instance (Eq l) => Eq (FaState l) where
>     (FaState l1 _ _ _) == (FaState l2 _ _ _) = l1 == l2

Printing a FaState however poses a slight problem. For example, the
state labeled '1' in the automaton dom18 refers to itself. If we
blindly 'follow the links', we will loop forever. Therefore, we must
keep track of the already printed states. We need a data structure for
such an occurrence check, with the following obvious operations:

> class OCC occ where
>     empty:: occ a
>     seenp:: (Eq a) => a -> occ a -> Bool -- occurrence check predicate
>     put::  a -> occ a -> occ a           -- add an item

In this article, we realize such a data structure as a list. In the
future, we can pull in something fancier from the Edison collection:

> instance OCC [] where
>     empty = []
>     seenp = elem
>     put = (:)

We are now ready to print an automaton. To be more precise, we
traverse the corresponding graph depth-first, pre-order, and keep
track of the already printed states. A 'states_seen' datum accumulates
the shown states, so we can be sure we print each state only once and
thus avoid the looping.

> instance (Eq l,Show l) => Show (FaState l) where
>     show state = "{@" ++ showstates [state] (empty::[FaState l]) "@}"
>          where
>            -- showstates worklist seen_state
>          showstates [] states_seen suffix = suffix
> 	   showstates (st:rest) states_seen suffix
> 	       | st `seenp` states_seen = showstates rest states_seen suffix
> 	   showstates (st@(FaState l accept t0 t1):rest) states_seen suffix =
> 	       showstate st
> 	             $ showstates (t0++t1++rest) (st `put` states_seen) suffix
>          showstate (FaState l accept t0 t1) suffix
> 	             = "{State " ++ (show l) ++
>                    " " ++ (show accept) ++ " " ++ (show $ map label t0) ++
> 	             " " ++ (show $ map label t1) ++ "}" ++ suffix

Now,

CCardFA> print dom18 -- prints as
CCardFA> [{@{State 1 True [1,2] []}{State 2 True [2,1] [1,2]}@}]


The acceptance function for our automata can be written as
follows. The function takes the list of starting states and the string
over the boolean alphabet. The function returns True if the string is
accepted.

> finAuAcceptStringQ start_states str =
>         foldr (\l seed -> acceptP l str || seed) False start_states
>   where acceptP (FaState _ acceptQ _ _) [] = acceptQ
> 	  acceptP (FaState _ _ t0 t1) (s:rest) =
> 	          finAuAcceptStringQ (if s then t1 else t0) rest

To test the automata, we can try

> test1= finAuAcceptStringQ dom18 $ map (>0) [0,1,0,1]
> test2= finAuAcceptStringQ dom18 $ map (>0) [1,1,0,1]
> test3= finAuAcceptStringQ dom18 [True]
> test4= finAuAcceptStringQ dom18 [False]

We are now ready to write the NFA->DFA conversion, a determinization
of an NFA. We implement the textbook algorithm of tracing set of NFA
states. A state in the resulting DFA corresponds to a list of the NFA
states. A DFA is generally a cyclic graph, often with cycles of length
1 (self-referenced nodes). To be able to "link back" as we build DFA
states, we have to remember the already constructed states. We need a
data structure, a dictionary of states:

> class StateDict sd where
>     emptyd :: sd (l,FaState l)
>     locate :: (Eq l) => l -> sd (l,FaState l) -> Maybe (FaState l)
>     putd   :: (l,FaState l) -> sd (l,FaState l) -> sd (l,FaState l)

For now, we realize this dictionary as an associative list. If performance
matters, we can use a fancier dictionary from the Edison

> instance StateDict [] where
>     emptyd = []
>     locate = lookup
>     putd   = (:)

The work of the NFA->DFA conversion is done by the following function
determinize_cc. The function takes a list of NFA states, the dictionary
of the already built states, and returns a pair ([dfa_state],
updated_dictionary) where [dfa_state] is a singleton list.

> -- [nfa_state] -> dictionary_of_seen_states ->
> --                ([dfa_state],updated_dictionary)
> -- [dfa_state] is a singleton list
> determinize_cc states converted_states =
> 	-- first, check the cache to see if the state has been built already
>       case  dfa_label `locate` converted_states of
> 	Nothing -> build_state
> 	Just dfa_state -> ([dfa_state],converted_states)
>   where
>       -- [NFA_labels] -> DFA_labels
>       det_labels = sort . nub . (map label)
>       dfa_label  = det_labels states

>       -- find out NFA-followers for [nfa_state] upon ingestion of 0 and 1
>       (t0_followers,t1_followers) =
> 		foldr (\st (f0,f1) -> (trans0 st ++ f0, trans1 st ++ f1))
> 		      ([],[]) states
>       acceptQ'    = or (map acceptQ states)

>       -- really build the dfa state and return ([dfa_state],updated_cache)
>       build_state = let
> 	   -- note, the dfa_state is computed _below_
> 	   converted_states1 = (dfa_label,dfa_state) `putd` converted_states
>          (t0', converted_states2) =
> 	           (determinize_cc t0_followers converted_states1)
> 	   (t1', converted_states3) =
> 	           (determinize_cc t1_followers converted_states2)
>          dfa_state =
> 	        (FaState dfa_label acceptQ' t0' t1')
> 	   in ([dfa_state],converted_states3)

The front end of the NFA->DFA transformer:

> finAuDeterminize states = fst $ determinize_cc states []

At the heart of the credit card transform is the phrase from the above code:

   converted_states1 = (dfa_label,dfa_state) `putd` converted_states

The phrase expresses the addition to the dictionary of the
'converted_states' of a 'dfa_state' that we haven't built yet. The
computation of the 'dfa_state' is written 4 lines below the phrase in
question. Because (,) is non-strict in its arguments and `locate` is
non-strict in its result, we can get away with a mere promise to
"pay". Note that the computation of the dfa_state needs t0' and t1',
which in turn rely on 'converted_states1'. This fact shows that we can
tie the knot by making a promise to compute a state, add this promise
to the dictionary of the built states, and use the updated dictionary
to build the descendants. Because Haskell is a non-strict language, we
don't need to do anything special to make the promise. Every
computation is Haskell is by default a promise.

We can print the DFA for dom18 to see what we've got:

CCardFA> finAuDeterminize dom18
CCardFA>-- which shows
CCardFA> [{@{State [1]   True  [[1,2]] [[]]   }
CCardFA>    {State [1,2] True  [[1,2]] [[1,2]]}
CCardFA>    {State []    False [[]]    [[]]   }@}]

which is indeed a DFA (which happens to be minimal)
recognizing (0+1)* - 1(0+1)*

We can run the determinized FA using the same function finAuAcceptStringQ:

> test1' = finAuAcceptStringQ (finAuDeterminize dom18) $ map (>0) [0,1,0,1]
> test2' = finAuAcceptStringQ (finAuDeterminize dom18) $ map (>0) [1,1,0,1]

Another example:

> dom19 = [one,two]
>     where one = FaState 1 True [two] []
> 	    two = FaState 2 True [one] [one]

CCardFA> finAuDeterminize dom19
CCardFA> -- shows
CCardFA> [{@{State [1,2] True  [[1,2]] [[1]] }
CCardFA>    {State [1]   True  [[2]]   [[]]  }
CCardFA>    {State [2]   True  [[1]]   [[1]] }
CCardFA>    {State []    False [[]]    [[]]  }@}]

which recognizes (0+1)* - (0+1)*11(0+1)*

Finally, here's an example with a 'diamond' cycle

> dom20 = [zero]
>   where zero = FaState 0 False [one]   [two]
>         one  = FaState 1 False [three] [four]
>         two  = FaState 2 False [four,five] [four]
>         three= FaState 3 True  []  []
>         four = FaState 4 True  [two]  [two]
>         five = FaState 5 True  [] []

CCardFA> print dom20
CCardFA> print $ finAuDeterminize dom20

Another example of tying a knot in the case of _forward links_, by
using a fixed-point combinator, is discussed in
	http://www.mail-archive.com/haskell@haskell.org/msg10687.html
