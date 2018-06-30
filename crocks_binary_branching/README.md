1. imperative way: intermediate results held by explicit variables
2. imperative with built-in `reduce`
3. partially point-free: curry and partial application, dependency graph with depth 4, one explicit variable
4. fully point-free: `Pair` ADT, `branch`/`fanout` -> `merge`, dependency graph with depth 4, branching logic captured by Pair
5. fully point-free: Ramda `converge`, "infinite" numbers of branching by providing an Array of functions on the same type
6. fully point-free: `List` (Traversable) and `Reader` (Applicative) ADTs,
 `traverse` a `List` of `Reader`s that returns a `Number` = a `Reader` that returns a `List** of `Number`s
 
In [Fantasy-land specification](https://github.com/fantasyland/fantasy-land#foldable), `reduceRight` takes `Array` or `[]` by default, which is a Monoid with `concat` implemented.
 

![sketch](./pic/sketch.png "sketch")
