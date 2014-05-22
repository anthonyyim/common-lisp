**eight_puzzle_solver.lisp**

An [eight puzzle solver](http://en.wikipedia.org/wiki/15_puzzle).

Usage: `(solve 'misplaced-tiles)` or `(solve 'manhattan-dist)`

The program takes the heuristic function as its
argument. Then, then program creates a solvable 8-puzzle by first
generating a solved puzzle and randomly moving the blank tile
around. The program then proceeds to call general serach, which calls
graph search and passes graph search the fringe, the closed list, the
successor function, the goal test, the same state test, heuristic
functiona and finally the enqueue function. Graph search then pops the
first node off the fringe and checks to see if the goal has been
reached. If not, it checks if the node has been visited before. If the
node hasn't been visited before, it expands it by calling the
successor function and then enqueuing the successor nodes into the
fringe according to their evaluation function. This process of popping
the fringe and expanding is then repated until the goal state is
reached.

Example output:
```lisp
>(solve 'misplaced-tiles)
>(2 3 5 7 1 8 0 6 4) is the initial state. Nodes expanded is 444. Below is the sequence of moves by blank tile:
(UP RIGHT RIGHT DOWN LEFT UP RIGHT UP LEFT LEFT DOWN RIGHT RIGHT DOWN)

>(solve 'manhattan-dist '(2 3 6 1 7 8 5 4 0))
is the initial state. Nodes expanded is 85. Below is the sequence of moves by blank tile:
>(UP UP LEFT LEFT DOWN RIGHT DOWN LEFT UP RIGHT DOWN RIGHT)

-----
**kb.lisp**

A first order logic inference engine

Usage: `(atp '(-insert knowledge base here-) '(-insert negated query here-))`

`-insert knowledge base here-` and `-insert query here-` should be your knowledge
base and negated query respectively in first order logic. A successful resolution
returns `NEGATED_QUERY_IS_FALSE` and a failure returns `FAIL`.

Furthermore, the `!` symbol represents "not" and the `$` designates a
variable (E.g. $x would be the variable x). Skolem functions and
constants are also supported. For example, a Skolem function in a literal would be
represented like the following: (American $x (F $y)). I.e., American(x F(x)).

Example Input:
```lisp
(atp '(((! American $x) (! Weapon $y) (! Sells $x $y $z) (! Hostile $z) (Criminal $x))
((Owns Nono M1))
((Missile M1))
((! Missile $x) (! Owns Nono $x) (Sells West $x Nono))
((! Missile $x) (Weapon $x))
((! Enemy $x America) (Hostile $x))
((American West))
((Enemy Nono America)))
'(((! Criminal West))))
```

Where the knowledge base is:
```lisp
'(((! American $x) (! Weapon $y) (! Sells $x $y $z) (! Hostile $z) (Criminal $x))
((Owns Nono M1))
((Missile M1))
((! Missile $x) (! Owns Nono $x) (Sells West $x Nono))
((! Missile $x) (Weapon $x))
((! Enemy $x America) (Hostile $x))
((American West))
((Enemy Nono America)))
```

and the negated query is:
```lisp
'(((! Criminal West)))
```

-----

**matcher.lisp**

