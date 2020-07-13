Solutions to Okasaki's 'Purely Functional Data Structures'
==========================================================

Goals
-----

This project is an attempt to solve all of the coding parts to the exercises from Okasaki's "Purely Functional Data Structures".
The theoretical parts can be found on the [accompanying website](https://www.briancallander.com/categories/pfds.html).
There should be extensive testing for all solutions.

This is currently under construction and will likely benefit from significant editing for consistency.

Related
-------

There are several other attempts to do this:

*   [kgeorgiy](https://github.com/kgeorgiy/okasaki)
*   [aistrate](https://github.com/aistrate/Okasaki)
*   [qnikst](https://github.com/qnikst/okasaki)

TODO
----

*   Better tests for the typeclasses: set, heap, queue, deque.

    There are already some basic tests but they could be extended and improved upon with some thought.

*   Consistent method for making instances of typeclasses for data structures.

    For example, there are many tree-like structures: sometimes a binary tree is newtyped to create a new heap instance; sometimes a new binary tree data type is created to do the same.

