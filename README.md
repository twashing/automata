# Automata

A simple automata library. A good breakdown of automata theory is on [Wikipedia](https://en.wikipedia.org/wiki/Automata_theory). Particularly, I'm interested in finite state machines.

![A simple finite state machine](https://upload.wikimedia.org/wikipedia/commons/thumb/9/9d/DFAexample.svg/274px-DFAexample.svg.png)

_This automaton consists of states (represented in the figure by circles) and transitions (represented by arrows). As the automaton sees a symbol of input, it makes a transition (or jump) to another state, according to its transition function, which takes the current state and the recent symbol as its inputs._


## Usage

You can create automaton like so.

```
(require '[automata.core :refer [automaton]]')

(automaton [:a :b :c :d])
(automaton [:a (a/+ :b) :c :d])
(automaton [:a (a/* :b) :c :d])
(automaton [:a (a/? :b) :c :d])
(automaton [:a (a/bound 2 4 :b) :c :d])
(automaton [:a (a/and :b :c) :d])
(automaton [:a (a/or :b :c) :d])
(automaton [:a (a/not :b) :c :d])
(automaton [1 (a/range 2 10)11])
```

Advancing through the states is done like so. Starting With regular scalars.
```
=> (def a (automaton [:a :b :c :d]))

=> (advance a :a)
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :b} {:matcher :c} {:matcher :d})
    :state {:matcher :a}
    :history (nil {:state {:matcher :a} :input :a :transition :automata.core/match})}

=> (-> a (advance :a) (advance :b))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :c} {:matcher :d})
    :state {:matcher :b}
    :history (nil
                {:state {:matcher :a} :input :a :transition :automata.core/match}
                {:state {:matcher :b} :input :b :transition :automata.core/match})}

=> (advance a :b)
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :state nil
    :history [nil]
    :error {:type :invalid-trasition :input :b :matcher {:matcher :a}}}


=> (-> a (advance :a) (advance :a))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :b} {:matcher :c} {:matcher :d})
    :state {:matcher :a}
    :history (nil {:state {:matcher :a} :input :a :transition :automata.core/match})
    :error {:type :invalid-trasition :input :a :matcher {:matcher :b}}}
```

Here, scalars mied with stars.
```
=> (def b (automaton [(* :a) :b :c :d]))

=> (-> b (advance :a) (advance :a))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :b} {:matcher :c} {:matcher :d})
    :state {:matcher :a}
    :history
    (nil
     {:state {:matcher :a} :input :a :transition :automata.core/match}
     {:state {:matcher :a} :input :a :transition :automata.core/match})}


=> (-> b (advance :a) (advance :b))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :c} {:matcher :d})
    :state {:matcher :b}
    :history
    (nil
     {:state {:matcher :a} :input :a :transition :automata.core/match}
     {:state {:matcher :b} :input :b :transition :automata.core/match})}
```

Star in the midde.
```
=> (def c (automaton [:a (* :b) :c :d]))

=> (-> c (advance :a) (advance :c))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :d})
    :state {:matcher :c}
    :history
    (nil
     {:state {:matcher :a} :input :a :transition :automata.core/match}
     {:state {:matcher :b} :input :c :transition :automata.core/noop}
     {:state {:matcher :c} :input :c :transition :automata.core/match})}
```

Multiple stars.
```
=> (def d (automaton [(* :a) (* :b) :c :d]))

=> (-> d (advance :b) (advance :b) (advance :c))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :d})
    :state {:matcher :c}
    :history
    (nil
     {:state {:matcher :a} :input :b :transition :automata.core/noop}
     {:state {:matcher :b} :input :b :transition :automata.core/match}
     {:state {:matcher :b} :input :b :transition :automata.core/match}
     {:state {:matcher :c} :input :c :transition :automata.core/match})}
  

=> (-> d (advance :c))
=> {:states ({:matcher :a} {:matcher :b} {:matcher :c} {:matcher :d})
    :run ({:matcher :d})
    :state {:matcher :c}
    :history
    (nil
     {:state {:matcher :a} :input :c :transition :automata.core/noop}
     {:state {:matcher :b} :input :c :transition :automata.core/noop}
     {:state {:matcher :c} :input :c :transition :automata.core/match})}
```


## TODO

- remaining combinators
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
