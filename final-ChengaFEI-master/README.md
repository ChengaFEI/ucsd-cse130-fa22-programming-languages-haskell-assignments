# Fall 2022 Final (Pt 1) [50 points]

**Released:** Wednesday, December 7, 9am PDT

**Due:** Thursday, December 8, 9am PDT

- You **may** consult any course material (lecture notes, assignments, past exams, etc)
- You **may** ask for clarifications on Piazza via a private message to instructors
- You **may not** communicate with other students or ask for anyone's help
- You **may not** search help forums (StackOverflow) and the Internet for a solution

## Overview

The exam is in the files:

- [BHeap.hs](./src/BHeap.hs)
- [tests/Test.hs](./tests/Test.hs)

As before `Test.hs` has some sample tests, and testing code that
you can use to check your solution before submitting.

### Testing and Evaluation

Most of the points, will be awarded automatically, by
**evaluating your functions against a given test suite**.

[Tests.hs](./tests/Test.hs) contains a very small suite
of tests which gives you a flavor of of these tests.
When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
```

**or**

```
K out of N tests failed
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

### Submission Instructions

Submit your code via the `final` assignment on Gradescope.
You must submit a single zip file containing a single directory with your repository inside.
A simple way to create this zip file is:

- Run `git push` to push your local changes to your private fork of the assignment repository
- Navigate to your private fork on github and download source code as a zip

Please *do not* include the `.stack-work` or the `_MACOSX` folder into the submission.

Upon submission to Gradescope, the auto-grader will only test your code on the public test suite,
so you can get no more than 48 points.
After the deadline, we will re-test your code on the private test suite.

### Q1: Binary Heaps (50pts)

Recall that as part of the midterm we implemented the `insert` function
that inserts a new key into a binary heap.
While traversing the tree, this function must check for each node and its left child,
whether they are *full* 
(i.e. whether all of their levels are completely filled).
For this purpose we used a recursive function `isFull`,
which, again, traverses the whole tree.
This makes our insertion function inefficient.

To eliminate the inefficiency, we will modify the `BHeap` datatype slightly:
in each internal node, we will store a boolean flag
that indicates whether this subtree is full:

```haskell
data BHeap 
  = Leaf                       -- ^ Empty tree
  | Node Int Bool BHeap BHeap  -- ^ Node with a value, whether this tree is full,
                               -- ^ and left and right subtree
```

Now we can implement `isFull` in constant time like so:

```haskell
-- | Is this binary tree full according to the cached value?
isFull :: BHeap -> Bool
isFull Leaf              = True
isFull (Node _ full _ _) = full
```

We can use this version of `isFull` to implement efficient insertion. 
But the price to pay is that insertion has to properly adjust the fullness flags
of the nodes it creates.

The [midterm](https://drive.google.com/file/d/18gX__xGuQVKS9JUxYMIBgByhJiN-egAa/view?usp=sharing) 
contains definitions of the various properties that define a well-formed binary heap.

### 1.1. Efficient insertion [15 pts]

Fill in the definition of `insert` that inserts a new key into a binary heap.
You can *assume* that the input heap is valid,
i.e. satisfies the shape property, the value property, 
and all of its fullness flags are accurate.
You must *guarantee* that the output heap is also valid.

Your implementation must satisfy the following test cases:

```haskell
λ> insert 11 Leaf  
Node 11 True Leaf Leaf

λ> insert 5 (Node 11 True (Node 3 True Leaf Leaf) (Node 8 True Leaf Leaf))
Node 11 False (Node 5 False (Node 3 True Leaf Leaf) Leaf) (Node 8 True Leaf Leaf) 
```

Feel free to refer to the midterm [master solution](https://drive.google.com/file/d/1taxKiDYZYPL0OxNy9jH7h2rj83PnetXq/view?usp=sharing) for the implementation of `insert` without fullness flags.


### 1.2. Build with HOF [5 pts]

Fill in the definition of `build` that converts the input `[Int]`
into a `BHeap` by inserting all list elements into an empty heap.
**You may not** use recursion; **you must** use higher-order functions instead. 
The order in which you add elements is irrelevant.

### Property-based testing

Now we want to use QuickCheck to test whether we implemented `insert` and `build` correctly.
In particular, we want to test the following properties:

1. `prop_full_correct`: if we create a heap with `build`, it has its fullness flag set correctly
2. `prop_is_heap_shape`: if we create a heap with `build`, it satisfies the shape property
3. `prop_is_heap_value`: if we create a heap with `build`, it satisfies the value property
4. `prop_contains_elts`: a heap created with `build xs` contains all the elements of `xs`

We have implemented property 1 for you,
so if you have implemented `insert` and `build` correctly,
you should see the following output in `ghci` after loading `BHeap`:

```haskell
λ> quickCheck prop_full_correct
+++ OK, passed 100 tests.
```

The other three properties require you to implement 
some helper functions.

Again, the [midterm](https://drive.google.com/file/d/18gX__xGuQVKS9JUxYMIBgByhJiN-egAa/view?usp=sharing) 
contains definitions of the various properties that we will validate via QuickCheck.

### 1.3. Checking the shape property [10 pts]

Fill in the definition of `isHeapShape` 
that takes as input a binary tree
and determines whether it satisfies the heap shape property.
(Note that the input tree might not be a valid binary heap,
and in particular, its fullness flags might be invalid.)

Your implementation must satisfy the following test cases:

```haskell
λ> isHeapShape (Node 11 True (Node 3 True Leaf Leaf) (Node 8 True Leaf Leaf))
True

λ> isHeapShape (Node 11 True Leaf (Node 8 True Leaf Leaf))
False
```

After this step, you should see the following output:

```haskell
λ> quickCheck prop_is_heap_shape
+++ OK, passed 100 tests.
```

### 1.4. Checking the value property [10 pts]

Fill in the definition of `isHeapValue` 
that takes as input a binary tree
and determines whether it satisfies the heap value property.
(Note that the input tree might not be a valid binary heap,
and in particular, its fullness flags might be invalid.)

Your implementation must satisfy the following test cases:

```haskell
λ> isHeapValue (Node 11 True (Node 3 True Leaf Leaf) (Node 8 True Leaf Leaf))
True

λ> isHeapValue (Node 11 True (Node 15 True Leaf Leaf) (Node 8 True Leaf Leaf))
False
```

After this step, you should see the following output:

```haskell
λ> quickCheck prop_is_heap_value
+++ OK, passed 100 tests.
```

### 1.5. Checking the elements property with HOF [10 pts]

Fill in the definition of `containsAll`
that takes as input a list and a (valid) binary heap
and determines whether all list elements are present in the heap.
**You may not** use recursion; **you must** use higher-order functions instead. 
However, you should use the provided function `contains`
to check the presence of individual values.

Your implementation must satisfy the following test cases:

```haskell
λ> containsAll [3, 8, 11] (Node 11 True (Node 3 True Leaf Leaf) (Node 8 True Leaf Leaf))
True

λ> containsAll [3, 5, 11] (Node 11 True (Node 3 True Leaf Leaf) (Node 8 True Leaf Leaf))
False
``` 

After this step, you should see the following output:

```haskell
λ> quickCheck prop_contains_elts
+++ OK, passed 100 tests.
```
