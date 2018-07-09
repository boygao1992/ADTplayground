# Understanding of Decoder

`Decoder` is a `Reader` and `Result`. (latter is obvious, by definition)
(`Reader` implements `Functor`, `Apply`, `Applicative`, `Monad`)
The environment which all these chained `Decoder`s take in is the JSON string to be parsed.
These `Decoder`s are chained sequentially (as `Applicative` into a "bigger" `Decoder`) but function independently. (a typical scenario to have parallel computing and use an `Applicative` to coordinate the end result)

It can be modeled this way because parsing of the JSON string is not part of the computation of each Decoder.
It's parsed only once using native `JSON.parse()` into native JS Object which is a dictionary in its C++ runtime.
(
`./elm-stuff/packages/elm-lang/core/5.1.1/src/Json/Decode.elm`
``` purescript
decodeString : Decoder a -> String -> Result String a
decodeString =
  Native.Json.runOnString
```

`./elm-stuff/packages/elm-lang/core/5.1.1/src/Native/Json.js`

``` javascript
function runOnString(decoder, string)
{
	var json;
	try
	{
		json = JSON.parse(string);
	}
	catch (e)
	{
		return _elm_lang$core$Result$Err('Given an invalid JSON: ' + e.message);
	}
	return run(decoder, json);
}

function run(decoder, value)
{
	var result = runHelp(decoder, value);
	return (result.tag === 'ok')
		? _elm_lang$core$Result$Ok(result.value)
		: _elm_lang$core$Result$Err(badToString(result));
}
```
)

And then each `Decoder` is just a lense/query on this dictionary to grab the required field, which is efficient (O(1)).

``` elm
decodeString :: Decoder a -> String -> Result String a
```
`decodeString = flip(runReader)`
``` haskell
runReader    :: e -> Reader e a  -> a
```

in this case, `e` is `String`
Since `Decoder` also encapsulates `Result`, so the type signature slightly different from generic `Reader`

`Result` is to handle potential errors during the parsing.
If one of the `Decoder`s fails, then the entire parsing fails with an `Err` message.
