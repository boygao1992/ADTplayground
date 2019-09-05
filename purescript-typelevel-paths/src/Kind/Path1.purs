module Kind.Path1 where

{- NOTE design choice
filtering conditions on relations attached to fields of target entity
-}

import Type.Data.Boolean as B

foreign import kind Constant
foreign import data CString :: Symbol -> Constant
foreign import data CInt :: Symbol {- IsInt -} -> Constant
foreign import data CBool :: B.Boolean -> Constant

foreign import kind Lit
foreign import data Var :: Symbol -> Lit
foreign import data Constant :: Constant -> Lit

foreign import kind Pred
foreign import data Term :: Symbol -> Pred
foreign import data Eq :: Lit -> Pred
foreign import data Gt :: Lit -> Pred
foreign import data Lt :: Lit -> Pred
foreign import data Not :: Pred -> Pred
foreign import data And :: Pred -> Pred -> Pred
foreign import data Or  :: Pred -> Pred -> Pred

foreign import kind FieldName
foreign import data FieldName :: Symbol -> FieldName

foreign import kind FieldList
foreign import data FieldListNil  :: FieldList
foreign import data FieldListCons :: FieldName -> Field -> FieldList -> FieldList

foreign import kind NodeAccess
foreign import data Fetch  :: NodeAccess
foreign import data Ignore :: NodeAccess

foreign import kind FieldTraversal
foreign import data Branch :: FieldList -> FieldTraversal
foreign import data Node   :: NodeAccess -> FieldTraversal

foreign import kind FieldFilter
foreign import data Filtered :: Pred -> FieldTraversal -> FieldFilter

foreign import kind Field
foreign import data Let            :: Symbol -> LetArg -> Field
foreign import data FieldFilter    :: FieldFilter -> Field
foreign import data FieldTraversal :: FieldTraversal -> Field

foreign import kind LetArg
foreign import data LetFilter :: FieldFilter -> LetArg
foreign import data LetTraversal :: FieldTraversal -> LetArg

{- NOTE name list of all elder friends of people under 14 with a "w" in their names

{ "Person" ::
  { name :: Filtered (Term "w") Ignore
  , age :: Let "age" (Filtered (Lt (CInt "14")) Ignore)
  , friend ::
    { name :: Fetch
    , age :: Filtered (Gt (Var "age")) Ignore
    }
  }
}

Array
{ friend :: Array
  { name :: String
  }
}

-}
type SampleAST =
    FieldListCons
    ( FieldName "name")
    ( FieldFilter
      ( Filtered
        ( Term "w")
        ( Node Ignore)
      )
    )
  ( FieldListCons
    ( FieldName "friend")
    ( FieldTraversal
      ( Branch
        ( FieldListCons
          ( FieldName "age")
          ( FieldFilter
            ( Filtered
              ( Gt (Var "age"))
              ( Node Ignore)
            )
          )
        ( FieldListCons
          ( FieldName "name")
          ( FieldTraversal
            ( Node Fetch)
          )
          FieldListNil
        ))
      )
    )
  ( FieldListCons
    ( FieldName "age")
    ( Let "age"
      ( LetFilter
        ( Filtered
          ( Lt (Constant (CInt "14")))
          ( Node Ignore)
        )
      )
    )
    FieldListNil
  )
  )

