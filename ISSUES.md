The list-of-successes algorithm we use is nice, but currently quite
inefficient. Large paragraphs cause a combinatorial explosion of
partial solutions. Lowering the threshold parameter (i.e. pruning
more partial solutions) helps, but the quality of the solution
degrades.

We can do at least two things:

1. Shrink the search space. Prune more solutions sooner and avoid
   known dead-ends.

2. Use a more space-efficient representation for solutions.

The handling of the last line of a paragraph is poor. To ensure
that the algorithm terminates, any length is acceptable for the
last line of a paragraph. Most solutions with overlong last lines
are pruned by `optimal-fit`, but they can sneak through when a
solution has few demerits outside those accruing to the last line.
`best-fit` is also likely to pick one of these solutions.

Obvious bug: Extra newlines cause an error.
