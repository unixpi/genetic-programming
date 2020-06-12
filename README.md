# genetic-programming

Genetic programming in OCaml.


## Selection ()

better individuals are more likely to have more child programs than inferior
individuals. The most commonly employed method for selecting individuals
in GP is tournament selection

In tournament selection a number of individuals are chosen at random
from the population. These are compared with each other and the best of
them is chosen to be the parent. When doing crossover, two parents are
needed and, so, two selection tournaments are made

Note that tournament
selection only looks at which program is better than another. It does
not need to know how much better. This effectively automatically rescales
fitness, so that the selection pressure4 on the population remains constant.
Thus, a single extraordinarily good program cannot immediately swamp the
next generation with its children; if it did, this would lead to a rapid loss
of diversity with potentially disastrous consequences for a run. Conversely,
tournament selection amplifies small differences in fitness to prefer the better
program even if it is only marginally superior to the other individuals in
a tournament.

An element of noise is inherent in tournament selection due to the random
selection of candidates for tournaments. So, while preferring the best,
tournament selection does ensure that even average-quality programs have
some chance of having children. Since tournament selection is easy to implement
and provides automatic fitness rescaling, it is commonly used in GP.

Considering that selection has been described many times in the evolutionary
algorithms literature, we will not provide details of the numerous
other mechanisms that have been proposed. (Goldberg, 1989), for example,
describes fitness-proportionate selection, stochastic universal sampling and
several others.

## Crossover and mutation ()

GP departs significantly from other evolutionary algorithms in the implementation
of the operators of crossover and mutation.

The most commonly
used form of crossover is subtree crossover. Given two parents, subtree
crossover randomly (and independently) selects a crossover point (a node)
in each parent tree. Then, it creates the offspring by replacing the subtree
rooted at the crossover point in a copy of the first parent with a copy of
the subtree rooted at the crossover point in the second parent, as illustrated
in Figure 2.5. Copies are used to avoid disrupting the original individuals.
This way, if selected multiple times, they can take part in the creation of
multiple offspring programs.

Note that it is also possible to define a version
of crossover that returns two offspring, but this is not commonly used.
Often crossover points are not selected with uniform probability
as this leads to crossover operations frequently exchanging only very small amounts
of genetic material (i.e, small subtrees); many crossovers may in fact, reduce to simply
swapping two leaves

To counter this, Koza suggested the widely used approach of choosing functions 90% of the time
and leaves 10% of the time. many other types of crossover and mutation of GP trees are
possible

The most commonly used form of mutation in GP (which we will call
subtree mutation) randomly selects a mutation point in a tree and substitutes
the subtree rooted there with a randomly generated subtree.

Subtree mutation is sometimes implemented as
crossover between a program and a newly generated random program; this
operation is also known as “headless chicken” crossover

Another common form of mutation is point mutation, In point mutation, a random node is selected and the primitive
stored there is replaced with a different random primitive of the same arity
taken from the primitive set. If no other primitives with that arity exist,
nothing happens to that node (but other nodes may still be mutated).

When subtree mutation is applied, this involves the modification of exactly
one subtree. Point mutation, on the other hand, is typically applied on a
per-node basis. That is, each node is considered in turn and, with a certain
probability.

The choice of which of the operators described above should be used
to create an offspring is probabilistic. Operators in GP are normally mutually
exclusive (unlike other evolutionary algorithms where offspring are
sometimes obtained via a composition of operators). Their probability of
application are called operator rates. Typically, crossover is applied with the
highest probability, the crossover rate often being 90% or higher. On the
contrary, the mutation rate is much smaller, typically being in the region of
1%.
When the rates of crossover and mutation add up to a value p which is
less than 100%, an operator called reproduction is also used, with a rate of
1 − p. Reproduction simply involves the selection of an individual based on
fitness and the insertion of a copy of it in the next generation.

## Closure - property required of GP programs ()

The first component of closure is type consistency
Type consistency is required because subtree crossover can mix and join nodes arbitrarily.
Type conversion mechanisms can
introduce unexpected biases into the search process, so they should be used
with care

The type consistency requirement can seem quite limiting but often simple
restructuring of the functions can resolve apparent problems. For example,
an if function is often defined as taking three arguments: the test, the
value to return if the test evaluates to true and the value to return if the
test evaluates to false. The first of these three arguments is clearly Boolean,
which would suggest that if can’t be used with numeric functions like +.

This, however, can easily be worked around by providing a mechanism to
convert a numeric value into a Boolean (+ves are true, -ves are false).
Alternatively, one can replace the 3-input if with a function of four (numeric)
arguments a, b, c, d. The 4-input if implements “If a < b then return
value c otherwise return value d”.

An alternative to requiring type consistency is to extend the GP system.
Crossover and mutation might explicitly make use of type information
so that the children they produce do not contain illegal type mismatches.
When mutating a legal program, for example, mutation might be required
to generate a subtree which returns the same type as the subtree it has just
deleted.

The other component of closure is evaluation safety. Evaluation safety
is required because many commonly used functions can fail at run time. An
evolved expression might, for example, divide by 0, or call MOVE FORWARD
when facing a wall or precipice. This is typically dealt with by modifying
the normal behaviour of primitives. It is common to use protected versions
of numeric functions that can otherwise throw exceptions, such as division,
logarithm, exponential and square root. The protected version of a function
first tests for potential problems with its input(s) before executing the corresponding
instruction; if a problem is spotted then some default value is
returned.

Protected division (often notated with %) checks to see if its second
argument is 0. If so, % typically returns the value 1 (regardless of the value
of the first argument)

Similarly, in a robotic application a MOVE AHEAD
instruction can be modified to do nothing if a forward move is illegal or if
moving the robot might damage it.

An alternative to protected functions is to trap run-time exceptions and
strongly reduce the fitness of programs that generate such errors. However,
if the likelihood of generating invalid expressions is very high, this can lead
to too many individuals in the population having nearly the same (very
poor) fitness. This makes it hard for selection to choose which individuals
might make good parents.

One type of run-time error that is more difficult to check for is numeric
overflow. If the underlying implementation system throws some sort of exception,
then this can be handled either by protection or by penalising as
discussed above. However, it is common for implementation languages to
ignore integer overflow quietly and simply wrap around. If this is unacceptable,
then the GP implementation must include appropriate checks to catch
and handle such overflows.



## Sufficiency ()

There is one more property that primitives sets should have: sufficiency.
Sufficiency means it is possible to express a solution to the problem at hand
using the elements of the primitive set.

Unfortunately, sufficiency can be
guaranteed only for those problems where theory, or experience with other
methods, tells us that a solution can be obtained by combining the elements
of the primitive sets

When a primitive set is insufficient, GP
can only develop programs that approximate the desired one. However, in
many cases such an approximation can be very close and good enough for
the user’s purpose. Adding a few unnecessary primitives in an attempt to
ensure sufficiency does not tend to slow down GP overmuch, although there
are cases where it can bias the system in unexpected ways.


Evolving Structures other than Programs()

There are many problems where solutions cannot be directly cast as computer
programs. For example, in many design problems the solution is an
artifact of some type: a bridge, a circuit, an antenna, a lens, etc. GP has
been applied to problems of this kind by using a trick: the primitive set is set
up so that the evolved programs construct solutions to the problem. This is
analogous to the process by which an egg grows into a chicken. For example,
if the goal is the automatic creation of an electronic controller for a plant,
the function set might include common components such as integrator,
differentiator, lead, lag, and gain, and the terminal set might contain
reference, signal, and plant output. Each of these primitives, when
executed, inserts the corresponding device into the controller being built.
If, on the other hand, the goal is to synthesise analogue electrical circuits,
the function set might include components such as transistors, capacitors,
resistors, etc.


## Fitness Function()

The first two preparatory steps define the primitive set for GP, and therefore
indirectly define the search space GP will explore. This includes all the
programs that can be constructed by composing the primitives in all possible
ways.

the fitness function allows us to know which regions of the search space include
programs that solve, or approximately solve, the problem. This is the task
of the fitness measure, which is our primary (and often sole) mechanism
for giving a high-level statement of the problem’s requirements to the GP
system. For example, suppose the goal is to get GP to synthesise an amplifier
automatically. Then the fitness function is the mechanism which tells GP
to synthesise a circuit that amplifies an incoming signal. (As opposed to
evolving a circuit that suppresses the low frequencies of an incoming signal,
or computes its square root, etc. etc.)

Fitness can be measured in many ways. For example, in terms of: the
amount of error between its output and the desired output; the amount
of time (fuel, money, etc.) required to bring a system to a desired target
state; the accuracy of the program in recognising patterns or classifying
objects; the payoff that a game-playing program produces; the compliance
of a structure with user-specified design criteria

There is something unusual about the fitness functions used in GP that
differentiates them from those used in most other evolutionary algorithms.
Because the structures being evolved in GP are computer programs, fitness
evaluation normally requires executing all the programs in the population,
typically multiple times. While one can compile the GP programs that make
up the population, the overhead of building a compiler is usually substantial,
so it is much more common to use an interpreter to evaluate the evolved
programs.

Interpreting a program tree means executing the nodes in the tree in
an order that guarantees that nodes are not executed before the value of
their arguments (if any) is known. This is usually done by traversing the
tree recursively starting from the root node, and postponing the evaluation
of each node until the values of its children (arguments) are known. Other
orders, such as going from the leaves to the root, are possible. If none
of the primitives have side effects, the two orders are equivalent
