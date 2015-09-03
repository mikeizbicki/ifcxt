# IfCxt

This package introduces the function:
```
ifCxt :: IfCxt cxt => proxy cxt -> (cxt => a) -> a -> a
```
This function acts like an `if` statement where the `proxy cxt` parameter is the condition.
If the type checker can satisfy the `cxt` constraint, then the second argument `cxt => a` is returned;
otherwise, the third argument `a` is returned.

Before seeing more details about how `ifCxt` is implemented,
let's look at three examples of how to use it.

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

### Example 2: make your code asymptotically efficient

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

The simplest way to sum a list of numbers is:
```
sumSimple :: Num a => [a] -> a
sumSimple = foldl' (+) 0
```
This method has numerical stability issues on floating point representations.
[Kahan summation](https://en.wikipedia.org/wiki/Kahan_summation_algorithm) is a more accurate technique shown below:
```
sumKahan :: Num a => [a] -> a
sumKahan = snd . foldl' go (0,0)
    where
        go (c,t) i = ((t'-t)-y,t')
            where
                y = i-c
                t' = t+y
```
Because Kahan summation does a lot more work than simple summation, we would prefer not to run it on non-floating point types.
The `sumCxt` function below accomplishes this:
```
cxtSum :: forall a. (Num a, IfCxt (Floating a)) => [a] -> a
cxtSum = ifCxt (Proxy::Proxy (Floating a)) sumKahan sumSimple
```
Notice that the `ifCxt` function is conditioning on the `Floating a` constraint,
which isn't actually *used* by the `sumKahan` function.

## How it works

The magic of the technique is in the `IfCxt` class:
```
class IfCxt (cxt :: Constraint) where
    ifCxt :: proxy cxt -> (cxt => a) -> a -> a
```
(Notice that making a constraint an instance of a class requires the`ConstraintKinds` extension,
and the higher order `(cxt => a)` parameter requires the `RankNTypes` extension.)

There is a "global" instance defined as:
```
instance {-# OVERLAPPABLE #-} IfCxt cxt where ifCxt _ t f = f
```
What this says is that if no more specific instance is available, then the "global" `ifCxt` function will be used, which always returns the `f` (false) parameter.

Then for every instance of every other class, we need to define an overlapping `IfCxt` instance that always returns the `t` (true) parameter.
For example, for `Show Int`, we define:
```
instance {-# OVERLAPS #-} IfCxt cxt where ifCxt _ t f = t
```

This is a lot of boilerplate, so the template haskell function `mkIfCxtInstances` can be used to define these instances automatically.
Unfortunately, due to a [bug in template haskell](https://ghc.haskell.org/trac/ghc/ticket/9699) we cannot enumerate all the classes currently in scope.
So you must manually call `mkIfCxtInstances` on each class you want `ifCxt` to work with.
