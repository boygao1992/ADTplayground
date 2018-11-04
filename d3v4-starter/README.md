## `scaleLinear`

```haskell
d3.scaleLinear :: Tuple Number Number -> Tuple Number Number -> Number -> Number
d3.scaleLinear domain range x = y
```

```javascript
const linearScale = d3.scaleLinear()
  .domain( [ 0, 100 ] )
  .range( [ 0, 1 ] )
```

is equivalent to

```haskell
linearScale :: Number -> Number
linearScale x = x / 100.0
```

examples

```javascript
linearScale(-1) = -0.01
linearScale(0) = 0
linearScale(100) = 1
```

edge cases

1. `String` will be converted to `Number` if possible

   ```javascript
   linearScale("-1") = -0.01
   linearScale("A") = NaN // invalid input
   ```

2. `range` can be in decreasing order

   ```javascript
   // [0, 100] -> [1, 0]
   linearScale(1) = 0.99
   ```

3. if size of `range`  is 0, any valid input will be mapped to the left boundary value of `domain`

   ```javascript
   // [0, 100] -> [0, 0]
   linearScale(1) = 0
   linearScale("-1") = 0
   linearScale("A") = NaN // invalid input
   ```



## `scaleTime`

```haskell
d3.scaleTime :: Tuple Date Date -> Tuple Number Number -> Date -> Number
d3.scaleTime domain range date = scale
```



## `scaleQuantize`

```haskell
d3.scaleQuantize :: forall a. Tuple Number Number -> Array a -> Number -> a
d3.scaleQuantize domain range x = a
```

from a continuous domain to a discrete range

## `scaleOrdinal`

```haskell
d3.scaleOrdinal :: forall a b. Array a -> Array b -> a -> b
d3.scaleOrdinal domain range a = b
```

from a discrete domain to a discrete range

edge cases:

1. length of (Array a) is equal to the length of (Array b)
  - any input not in (Array a) will be mapped to the 1st element of (Array b)
2. length of (Array a), n, is larger than the length of (Array b), m
  - any input not in (Array a) will be mapped to the (n+1)th element of (Array b)
3. length of (Array a), n, is less than the length of (Array b), m
  - (i)th element of (Array a) will be mapped to the (i % m)th element of (Array b)

  - any input not in (Array a) will be mapped to the last element of (Array b)
    e.g.

    ```javascript
    // [ "A", "B", "C", "D", "E" ] -> [ 4, 3 ]
    scaleOrdinal("E") = 4 // i = 4, m = 2, i % m = 0
    ```