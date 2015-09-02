# IfCxt

This package introduces the `ifCxt` function.
It has the following type signature:
```
ifCxt :: IfCxt cxt => proxy cxt -> (cxt => a) -> a -> a
```
This function acts like an `if` statement where the `proxy cxt` parameter is the condition.
`cxt` has kind `Constraint`.
If the type checker can satisfy the `cxt` constraint, then the second argument `cxt => a` is returned;
otherwise, the third argument `a` is returned.

Before seeing more details about how this works,
let's look at three examples on why you might want this functionality.

### Example 1: show every type

The `cxtShow` function below is polymorphic over the type `a`.
If `a` is an instance of `Show`, then `cxtShow a` evaluates to `show a`;
but if `a` is not an instance of `Show`, `cxtShow a` evaluates to `<<unshowable>>`.

```
cxtShow :: forall a. IfCxt (Show a) => a -> String
cxtShow a = ifCxt (Proxy::Proxy (Show a))
    (show a)
    "<<unshowable>>"
```
In ghci:

```
ghci> cxtShow (1 :: Int)
"1"
```
```
ghci> cxtShow (id :: a -> a)
"<<unshowable>>"
```

### Example 2: make your code maximally efficient

The `nub` function removes duplicate elements from lists.
It can be defined as:

```
nub :: Eq => [a] -> [a]
nub []     =  []
nub (x:xs) =  x : nub (filter (x/=) xs)
```
This function takes time O(n^2).
But if we also have an `Ord` constraint, we can define a much more efficient version that takes time O(n log n):

```
nubOrd :: Ord a => [a] -> [a]
nubOrd = go . sort
    where
        go (x1:x2:xs)
            | x1==x2    =      go (x2:xs)
            | otherwise = x1 : go (x2:xs)
        go [x] = [x]
        go []  = []
```
Now, we can use the `ifCxt` function to define a version of `nub` that will automatically select the most efficient implementation for whatever type we happen to run it on:

```
cxtNub :: forall a. (Eq a, IfCxt (Ord a)) => [a] -> [a]
cxtNub = ifCxt (Proxy::Proxy (Ord a)) nubOrd nub
```

### Example 3: make your code numerically stable


## How it works

The magic of the technique is in the `IfCxt` class:
```
class IfCxt cxt where
    ifCxt :: proxy cxt -> (cxt => a) -> a -> a
```

There is a "global" instance defined as:
```
instance {-# OVERLAPPABLE #-} IfCxt cxt where ifCxt _ t f = f
```
What this says is that if no more specific instance is available, then the "global" `ifCxt` function will be used, which always returns the `f` (false) parameter.

Then for every instance of every other class, we need to define an overlapping `IfCxt` instance that always returns the `t` (true) parameter.
For example, for `Show Int`, we define:
```
instance {-# OVERLAPPS #-} IfCxt cxt where ifCxt _ t f = t
```

This is a lot of boilerplate, so the template haskell function `mkIfCxtInstances` can be used to define these instances automatically.
Unfortunately, due to a [bug in template haskell](https://ghc.haskell.org/trac/ghc/ticket/9699) we cannot enumerate all the classes currently in scope.
So you must manually call `mkIfCxtInstances` on each class you want `ifCxt` to work with.
