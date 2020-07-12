# Un-optimal beta reduction in GHC?

Update: added a fourth expirement which simply sums the big integer `n ^ m`
replicated k times.

Some code and analysis from Aaron Stump's blog post ["Show me the (optimal) beta"][1].

[1]: https://queuea9.wordpress.com/2020/07/10/show-me-the-optimal-beta/

`Example1.hs` -- the initial example from the blog post. We figured out that
turning on optimisations in GHC (`-O2`) made the un-optimal behaviour go away.

`Example2.hs` -- an updated example that may exhibit un-optimal behaviour. There
are 2 changes compared to the first example:

1. Uses a longer list
2. Summing the list instead of filtering for even

Since we are using Haskell's `Integer` type I suspect that the time is actually
due to computing the large sum, adding larger Integers is more expensive than
checking if they are even.

To check this I've tweaked the examples in 3 ways (all included timings were
taken with GHC 8.8.3):

1. `Example1Sum` -- the first example but summing instead of filtering even
    numbers. This exhibits a similar slower behaviour as example 2.

    This is some evidence that changing to use summation is the cause of the
    time difference in example 2.

    ```
    ./timeit.sh Example1Sum

    [1 of 1] Compiling Main             ( Example1Sum.hs, Example1Sum.o )
    Linking Example1Sum ...

    time to compute just m ^ n
    even

    real	0m0.012s
    user	0m0.008s
    sys	0m0.004s

    time to compute 1 ^ 1 many (100000) times
    odd

    real	0m0.004s
    user	0m0.000s
    sys	0m0.004s

    time to compute m ^ n many (100000) times
    odd

    real	0m1.644s
    user	0m1.632s
    sys	0m0.012s
    ```

2. `Example2FilterEven` -- using the check from the first example instead of
   summing the list. The relative timings look the same as in example 1.


   ```
   ./timeit.sh Example2FilterEven

    [1 of 1] Compiling Main             ( Example2FilterEven.hs, Example2FilterEven.o )
    Linking Example2FilterEven ...

    time to compute just m ^ n
    0

    real	0m0.012s
    user	0m0.008s
    sys	0m0.004s

    time to compute 1 ^ 1 many (100000) times
    0

    real	0m0.003s
    user	0m0.000s
    sys	0m0.003s

    time to compute m ^ n many (100000) times
    0

    real	0m0.014s
    user	0m0.010s
    sys	0m0.004s
    ```

3. `Example2Ints` -- keep the summation, but swap in `Int` instead of
    `Integer`, this might overflow the Int but since we're not interested in
    the actual number that seems fine?

    The time now seems to be dominated by traversing the list and doing many
    additions. So perhaps this is not too interesting...

    ```
    # I've bumped the size of the list otherwise it's just too quick to tell anything.
    ./timeit.sh Example2Ints 100000000

    [1 of 1] Compiling Main             ( Example2Ints.hs, Example2Ints.o )
    Linking Example2Ints ...

    time to compute just m ^ n
    even

    real	0m0.002s
    user	0m0.001s
    sys	0m0.000s

    time to compute 1 ^ 1 many (100000000) times
    odd

    real	0m0.035s
    user	0m0.035s
    sys	0m0.000s

    time to compute m ^ n many (100000000) times
    odd

    real	0m0.026s
    user	0m0.025s
    sys	0m0.000s
    ```

I think it's hard to say anything conclusive without looking at the compiled
code, or one of GHC's many intermediary languages, but it's some evidence to
say that GHC is still doing OK?

---

A fourth experiment: This simply sums a list of length `k` of the result of `m ^ n`.
It is clear that the second example is simply measuring the time taken
for summation.

```
./timeit.sh Example2Const

[1 of 1] Compiling Main             ( Example2Const.hs, Example2Const.o )
Linking Example2Const ...

time to compute just m ^ n
odd

real	0m0.013s
user	0m0.012s
sys	0m0.000s

time to compute 1 ^ 1 many (100000) times
even

real	0m0.003s
user	0m0.000s
sys	0m0.003s

time to compute m ^ n many (100000) times
even

real	0m1.632s
user	0m1.627s
sys	0m0.004s
```
