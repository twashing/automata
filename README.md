# Automata

A simple automata library. A good breakdown of automata theory is on [Wikipedia](https://en.wikipedia.org/wiki/Automata_theory). Particularly, I'm interested in finite state machines.

![A simple finite state machine](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/DFAexample.svg/274px-DFAexample.svg.png)

_This automaton consists of states (represented in the figure by circles) and transitions (represented by arrows). As the automaton sees a symbol of input, it makes a transition (or jump) to another state, according to its transition function, which takes the current state and the recent symbol as its inputs._


## Usage

```
:a/begin
:a/end
a/any

[:a :b :c :d]
[:a (a/+ :b) :c :d]
[:a (a/* :b) :c :d]
[:a (a/? :b) :c :d]
[:a (a/bound 2 4 :b) :c :d]
[:a (a/and :b :c) :d]
[:a (a/or :b :c) :d]
[:a (a/not :b) :c :d]
[1 (a/range 2 10)11]
```

## TODO

- tests
- cycle between a > b > a
- OR condition  a > b > c  |  a > c
- AND condition  a > b > c  |  a > d > c
- NOT condition  a > !b > c 
- range
- bound


## License

Copyright © 2018 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
