(ns automata.refactor
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.core.match :as m]
            [clojure.tools.trace :refer [trace]]
            [com.rpl.specter :refer :all]))


(declare automata?)
(declare parser-combinator?)
(declare complete?)

(defn position->state [a position]
  (-> a :matcher (nth position)))

(defn ->state [a]
  (-> a
      (position->state (:position a))
      :matcher))

(defn peek-next-state [a]
  (-> a
      (position->state (inc (:position a)))
      :matcher))

(defn peek-nth-state [a n]
  (-> a
      (position->state (clojure.core/+ n (:position a)))
      :matcher))

(defn ->matcher [a]
  (position->state a (:position a)))

(defn peek-next-matcher [a]
  (position->state a (inc (:position a))))

(defn peek-nth-matcher [a n]
  (position->state a (clojure.core/+ n (:position a))))

(defn complete? [input]
  (if ((comp clojure.core/not automata?) input)
    true
    (->> input
         :matcher
         (map :matcher)
         (keep-indexed
           #(if (= %2 :END) %1))
         first
         dec
         (= (:position input)))))

(defn identity? [automaton node]
  (= (->state automaton)
     node))



(defmulti transition-common (fn [dispatch _ _] dispatch))

(defmethod transition-common :this [_ automaton input]
  (let [history (:history automaton)
        last-index (-> history count (- 1))
        l (last history)]

    (if-not (empty? l)

      (assoc automaton :history (as-> automaton a
                                  (:history a)
                                  (into [] a)
                                  (update a last-index #(concat % [input]))))
      (assoc automaton :history (-> automaton :history (concat [[input]]))))))

(defmethod transition-common :next [_ automaton input]
  (assoc automaton
         :history (-> automaton :history (concat [[input]]))
         :position (-> automaton :position inc)))

(defmethod transition-common :increment-position [_ automaton input]
  (assoc automaton :position (-> automaton :position inc)))

(defmethod transition-common :skip [_ automaton input]
  (assoc automaton
         :history (-> automaton :history (concat [[input]]))
         :position (-> automaton :position (clojure.core/+ 2))))



(defmulti transition-error (fn [error-type _ _] error-type))

(defmethod transition-error :invalid-transition
  [error-type automaton input]
  (assoc automaton
         :error {:type error-type
                 :input input}))

(defmethod transition-error :input-out-of-bounds
  [error-type automaton input]
  (as-> automaton a
    (assoc a :error {:type error-type
                     :input input})
    (transition-common :this a input)
    (transition-common :increment-position a input)))



(defmulti transition-local (fn [dispatch _ _] dispatch))

(defmethod transition-local :plus [_ automaton-error-free input]

  (cond

    (= input (peek-next-state automaton-error-free))
    (transition-common :next automaton-error-free input)

    (= input (->state automaton-error-free))
    (transition-common :this automaton-error-free input)

    :else (transition-error :invalid-transition automaton-error-free input)))

(defmethod transition-local :star [_ automaton-error-free input]

  (cond

    (= input (->state automaton-error-free))
    (transition-common :this automaton-error-free input)

    (= input (peek-next-state automaton-error-free))
    (transition-common :next automaton-error-free input)

    (= input (peek-nth-state automaton-error-free 2))
    (transition-common :skip automaton-error-free input)

    :else (transition-error :invalid-transition automaton-error-free input)))

(defmethod transition-local :bound [_ {:keys [x y] :as automaton-error-free} input]

  (let [transition-proposition (if (= (peek-next-state automaton-error-free) input)
                                 (transition-common :next automaton-error-free input)
                                 (transition-common :this automaton-error-free input))
        {:keys [position history]} transition-proposition
        history-at-position (if (empty? history)
                              []
                              (last history))
        size-at-position (count history-at-position)]

    (m/match [transition-proposition size-at-position]

             ;; BEFORE BOUNDS
             [(_ :guard #(= input (->state %)))
              (_ :guard #(< % x))]
             (transition-error :input-out-of-bounds automaton-error-free input)

             ;; WITHIN BOUNDS
             [(_ :guard #(= input (->state %)))
              (_ :guard #(clojure.core/and
                           (>= % x)
                           (<= % y)))]
             transition-proposition

             ;; LOOKAHEAD
             [(_ :guard #(= (peek-nth-state % (clojure.core/+ 2 position)) input)) _]
             (transition-common :skip automaton-error-free input)

             :else (transition-error :invalid-transition automaton-error-free input))))



(defn conditionally-append-to-history [node automaton-updated]
  (if (complete? node)
    (transition-common :this automaton-updated automaton-updated)
    automaton-updated))

(defn conditionally-increment-position [node automaton-updated]
  (if (clojure.core/and
        (complete? node)
        (clojure.core/not (identity? automaton-updated node)))
    (transition-common
      :increment-position automaton-updated automaton-updated)
    automaton-updated))

(defn conditionally-reset-matcher [node position automaton-updated]
  (if (complete? node)
    (as-> node n
      (assoc n
             :position 0
             :history [])
      (transform [:matcher (nthpath position) :matcher]
                 (constantly n)
                 automaton-updated))
    automaton-updated))

(defn automaton->with-updated-node [position node automaton-error-free]
  (->> (transform [:matcher (nthpath position) :matcher]
                  (constantly node)
                  automaton-error-free)
       (conditionally-append-to-history node)
       (conditionally-increment-position node)
       (conditionally-reset-matcher node position)))



(defprotocol ParserCombinator
  (transition [_ automaton input]))

(defrecord Scalar [matcher]
  ParserCombinator
  (transition [_ automaton input]
    (let [automaton-error-free (dissoc automaton :error)]
      (if (= input (peek-next-state automaton-error-free))
        (transition-common :next automaton-error-free input)
        (transition-error :invalid-transition automaton-error-free input)))))

(defn extract-coordinates [automaton]

  (let [next-position (-> automaton :position inc)
        next-state (peek-next-state automaton)

        this-position (-> automaton :position)
        this-state (->state automaton)

        automata-this? (automata? this-state)
        complete-this? (complete? this-state)
        automata-next? (automata? next-state)
        complete-next? (complete? next-state)]

    (cond
      automata-this? {:isautomata? automata-this?
                      :iscomplete? complete-this?
                      :state this-state
                      :position this-position
                      :location :this}
      automata-next? {:isautomata? automata-next?
                      :iscomplete? complete-next?
                      :state next-state
                      :position next-position
                      :location :next}
      :else {:isautomata? false
             :iscomplete? complete-this?
             :state nil
             :position nil
             :location nil})))

(defrecord Plus [matcher]

  ParserCombinator

  (transition [this automaton input]

    (let [automaton-error-free (dissoc automaton :error)

          {:keys [isautomata? iscomplete?
                  state position location]} (extract-coordinates automaton-error-free)

          peeked-node-equals? (= input (peek-next-state automaton-error-free))

          lhs (-> automaton :matcher
                  (nth (:position automaton))
                  (#(when (automata? %)
                      (-> % :matcher (dissoc :position :history :error)))))
          rhs (-> automaton :history last last :matcher
                  (nth (:position automaton))
                  (#(when (automata? %)
                      (-> % :matcher (dissoc :position :history :error)))))

          processed-at-least-once? (= lhs rhs)]

      ;; (trace [location [peeked-node-equals? processed-at-least-once?] isautomata? iscomplete?])
      (m/match [location
                (clojure.core/and peeked-node-equals? processed-at-least-once?)
                isautomata?
                iscomplete?]

               [_ false true false] (as-> input i
                                      (transition state state i)
                                      (automaton->with-updated-node position i automaton-error-free))

               [_ _ _ _] (transition-local :plus automaton-error-free input)))))

(defrecord Star [matcher]

  ParserCombinator

  (transition [this automaton input]

    (let [automaton-error-free (dissoc automaton :error)
          next-position (-> automaton :position inc)
          next-state (peek-next-state automaton-error-free)

          this-position (-> automaton :position)
          this-state (->state automaton-error-free)

          automata-this? (automata? this-state)
          complete-this? (complete? this-state)
          automata-next? (automata? next-state)
          complete-next? (complete? next-state)

          peeked-node-equals? (= input (peek-next-state automaton-error-free))]

      (m/match [peeked-node-equals? automata-this? complete-this? automata-next? complete-next?]

               [true _ _ _ true] (transition-local :star automaton-error-free input)

               [_ true false _ _] (as-> input i
                                  (transition this-state this-state i)
                                  (automaton->with-updated-node this-position i automaton-error-free))

               [_ _ _ true false] (as-> input i
                                  (transition next-state next-state i)
                                  (automaton->with-updated-node next-position i automaton-error-free))))))

(defrecord Bound [matcher x y]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)

          {:keys [isautomata? iscomplete?
                  state position location]} (extract-coordinates automaton-error-free)

          ;; next-position (-> automaton :position inc)
          ;; next-state (peek-next-state automaton-error-free)
          ;;
          ;; this-position (-> automaton :position)
          ;; this-state (->state automaton-error-free)
          ;;
          ;; automata-this? (automata? this-state)
          ;; complete-this? (complete? this-state)
          ;; automata-next? (automata? next-state)
          ;; complete-next? (complete? next-state)
          ]

      (trace [isautomata? iscomplete?])
      (m/match [isautomata? iscomplete?]

               [true false] (as-> input i
                                (transition state state i)
                                (automaton->with-updated-node position i automaton-error-free))

               [_ true] (transition-local :bound (assoc automaton-error-free :x x :y y) input)))))

(defrecord Or [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)

          {a true b false} (->> automaton-error-free peek-next-state (group-by automata?))

          next-position (-> automaton-error-free :position inc)

          automata-input-values (->> a
                                     (map :matcher)
                                     (map (fn [b]
                                            (remove #(some #{:START :END}
                                                           #{(:matcher %)})
                                                    b)))             ;; automata-input-sets
                                     trace (map (fn [a] (map :matcher a)))  ;; automata-input-scalars
                                     (map #(some (into #{} %) #{input})))

          an-automata-has-input? (->> automata-input-values
                                      (some identity)
                                      ((comp clojure.core/not nil?)))

          a-scalar-has-input? ((comp clojure.core/not nil?)
                               (some (->> b
                                          (into #{})
                                          (map :matcher)
                                          (into #{}))
                                     [input]))

          foobar (fn [a input]
                   (let [{position :position :as fst} (first a)
                         {error :error :as result} (transition fst fst input)]

                     (if (nil? error)
                       (automaton->with-updated-node position result fst) ;; ** TODO dig deeper
                       (recur (rest a) input))))]

      ;; (trace [an-automata-has-input? a-scalar-has-input? [a next-position input]])
      (m/match [an-automata-has-input? a-scalar-has-input?]

               [true _] (foobar a input)
               [false true] (transition-common :next automaton-error-free input)
               [false false] (transition-error :invalid-transition automaton-error-free input)))))

(comment ;; NESTING OR

  (or [:a :b] [:a :c])
  (def three (automata [(or [:a :b] [:x :y] :c) :d]))

  (advance three :c)
  )

(defrecord Automata [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (let [parser-combinator? (->> (->matcher automaton)
                                  (instance? automata.refactor.ParserCombinator))
          automaton-state (->state automaton)
          identity-guard (fn [inp]
                           (= inp
                              (->> automaton :matcher rest butlast (map :matcher))))]

      (m/match [parser-combinator? automaton-state input]

               ;; START / END states
               [_ :START (_ :guard identity-guard)] (identity automaton)
               [_ :START _] (transition (peek-next-matcher automaton) automaton input)
               [_ :END _] (identity automaton)

               ;; ParserCombinator states
               [true _ _] (transition (->matcher automaton) automaton input)

               [_ _ _] :WTF))))


(def automata? (partial instance? automata.refactor.Automata))
(def parser-combinator? (partial instance? automata.refactor.ParserCombinator))
(def plus? (partial instance? automata.refactor.Plus))

;; ParserCombinator
;;   disambiguate between i. the matcher and ii. current state
;;
;; Star
;;   cannot have same stars next to each other
;;
;; Plus
;;   cannot have same stars next to each other

;; :state nil - means start state
;; :state (<states>) - means end state

;; :state START
;; :state END

(defn + [a] (->Plus a))
(defn * [a] (->Star a))
(defn scalar [a] (->Scalar a))
(defn bound [a x y] (->Bound a x y))
(defn ? [a] (->Bound a 0 1))
(defn or [& branches] (->Or (into #{} branches)))
(defmacro automata [states]
  {:pre [(seqable? states)
         ((comp clojure.core/not parser-combinator?) (eval states))]}

  (let [walker-vector-list-scalar (fn [v]
                                    (cond
                                      (vector? v) (as-> v a
                                                    (concat (list :START) a (list :END))
                                                    (map #(if-not (clojure.core/and
                                                                    (seqable? %)
                                                                    (parser-combinator? %))
                                                            (scalar %) %) a)
                                                    (into [] a)
                                                    (->Automata a)
                                                    (assoc a
                                                           :position 0
                                                           :history []))
                                      (list? v) (eval v)
                                      :else v))
        walker-set (fn [a]
                     (if (set? a)
                       (->> a
                            (map #(if ((comp clojure.core/not automata?) %)
                                    (scalar %)
                                    %))
                            (into #{}))
                       a))]

    (->> states
         (clojure.walk/postwalk walker-vector-list-scalar)
         (clojure.walk/postwalk walker-set))))

(defn advance [automaton input]
  (transition automaton automaton input))


(comment ;; SCALAR, STAR, PLUS

  (automata (* (or :a :b :c))) ;; FAIL we have to start with an automata


  ;; A
  (def a (automata [:a :b :c :d]))
  ;; (match a '(:a :b :c :d) [:a :b :c :d])
  ;; (match a :a :a)
  ;; (match a '(:a :b :c :d) :a) ;; false

  (advance a :a)
  (advance a :z)
  (advance a [:a :b :c :d])


  ;; C
  (def c (automata [(* :a) :b :c :d]))

  (advance c :a)
  (advance c :b)
  (advance c :c) ;; FAIL

  (-> c
      (advance :a)
      (advance :a))

  (-> c
      (advance :a)
      (advance :b)
      (advance :c))


  ;; D
  (def d (automata [(+ :a) :b :c :d]))

  (advance d :a)
  (advance d :b) ;; FAIL

  (-> d
      (advance :a)
      (advance :a))

  (-> d
      (advance :a)
      (advance :a)
      (advance :b))

  (-> d
      (advance :a)
      (advance :b)
      (advance :c)))

(comment ;; BOUND

  ;; E
  (def e (automata [(bound :a 2 3) :b :c :d]))

  (advance e :a) ;; FAIL

  (-> e
      (advance :a)
      (advance :a))

  (-> e
      (advance :a)
      (advance :a)
      (advance :a))

  ;; FAIL
  (-> e
      (advance :a)
      (advance :a)
      (advance :a)
      (advance :a))


  ;; F
  (def f (automata [(bound :a 1 2) :b :c :d]))

  (advance f :a)

  (-> f
      (advance :a)
      (advance :a))

  (-> f
      (advance :a)
      (advance :a)
      (advance :b))

  ;; FAIL
  (-> f
      (advance :a)
      (advance :a)
      (advance :a))


  ;; FF
  (def ff (automata [(bound :a 0 1) :b :c :d]))

  (advance ff :a)
  (advance ff :b)

  ;; FAIL
  (-> ff
      (advance :a)
      (advance :a))


  ;;; FFF
  (def fff (automata [(? :a) :b :c :d]))

  (advance fff :a)
  (advance fff :b)

  ;; FAIL
  (-> fff
      (advance :a)
      (advance :a)))

(comment ;; OR

  (def g (automata [(or :z :x :c :v) :b :c]))

  (advance g :z)
  (advance g :c)

  (-> g
      (advance :z)
      (advance :z)) ;; FAIL

  (advance g :b) ;; FAIL
  )

(comment ;; NESTING STAR

  (def one (automata [(* [:a :b]) :c])) ;; combinator / automata

  (advance one :a)
  (advance one :c) ;; TODO, should FAIL

  (-> one
      (advance :a)
      (advance :b))

  (-> one
      (advance :a)
      (advance :b)
      (advance :c))

  (-> one
      (advance :a)
      (advance :b)
      (advance :a))

  (-> one
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b))

  (-> one
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)
      (advance :c)))

(comment ;; NESTING PLUS

  (def zero (automata [(+ [:a :b]) :c])) ;; combinator / automata

  (advance zero :a)
  (advance zero :c) ;; should be a nested fail, in case there's a nested :c as well

  (-> zero
      (advance :a)
      (advance :b))

  (-> zero
      (advance :a)
      (advance :b)
      (advance :c))

  (-> zero
      (advance :a)
      (advance :b)
      (advance :a))

  (-> zero
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b))

  (-> zero
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)

      (advance :c)))

(comment ;; NESTING BOUND

  (def two (automata [(bound [:a :b] 2 3) :c :d]))

  (advance two :a)
  (advance two :c)

  (-> two
      (advance :a)
      (advance :b)) ;; TODO NOT YET

  (-> two
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)) ;; GOOD

  ;; TODO
  (-> two
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)

      (advance :a)) ;; TODO FAIL
  )



(comment ;; NESTING

  (do
    (automata [(* (+ [:a :b :c]))])
    (automata [(* (or :a :b :c))])
    (automata [(* [:a :b])])
    (automata [:a :b :c :d])
    (automata [(bound :a 2 3) :b :c :d]))

  (do

    ;; A
    (def a (automata [:a :b :c :d]))


    ;; C
    (def c (automata [(* :a) :b :c :d]))


    ;; D
    (def d (automata [(+ :a) :b :c :d]))


    ;; E
    (def e (automata [(bound :a 2 3) :b :c :d]))


    ;; F
    (def f (automata [(bound :a 1 2) :b :c :d]))


    ;; FF
    (def ff (automata [(bound :a 0 1) :b :c :d]))


  ;;; FFF
    (def fff (automata [(? :a) :b :c :d]))


    ;; G
    (def g (automata [(or :z :x :c :v) :b :c])))


  (def two (automata [(* (or :a :b :c))])) ;; combinator / combinator
  (def three (automata [[:a :b] [:a :c]])) ;; automata > automata


  ;; H
  (def h (automata [(* (or :a :b :c)) :z :x]))


  (advance h :a)
  (advance h :b)
  (advance h :c)


  (+ (or :a :b :c))
  (bound (or :a :b :c) 2 3)
  (+ (or [:a :b] [:a :c])))


;; (use 'com.rpl.specter)
;;
;; (defrecord Foo [bar])
;;
;; (def a
;;   #automata.refactor.Foo
;;   {:bar #automata.refactor.Foo
;;    {:bar (#automata.refactor.Foo{:bar :a :x 1 :y 2} :b :c :d)}})
;;
;; (def LEAF-WALKER
;;   (recursive-path [] p
;;                   (cond-path
;;                     #(instance? Foo %) [:bar p]
;;                     seqable? [ALL #(instance? Foo %) p]
;;                     STAY STAY)))
;;
;; (def FOO-WALKER
;;   (recursive-path [] p
;;                   (cond-path
;;                     #(instance? Foo %) [:bar p]
;;                     seqable? [ALL #(instance? Foo %) p]
;;                     STAY STAY)))
;;
;; (select LEAF-WALKER a)

;; automata.refactor.Automata
;; :matcher
;; [#automata.refactor.Scalar{:matcher :START}
;;  #automata.refactor.Star
;;  {:matcher #automata.refactor.Automata
;;   {:matcher
;;    [#automata.refactor.Scalar{:matcher :START}
;;     #automata.refactor.Scalar{:matcher :a}
;;     #automata.refactor.Scalar{:matcher :b}
;;     #automata.refactor.Scalar{:matcher :END}],
;;    :position 0,
;;    :history []}}
;;  #automata.refactor.Scalar{:matcher :c}
;;  #automata.refactor.Scalar{:matcher :END}],
;; :position 0,
;; :history []}

;; Within 10 tick increments
(+ (or :a :b :c))



(comment

  ;; TODO these guys can all nest each other
  ;; Star, Plus, Bound, Or, Automata

  ;; - Star, Plus -> Bound special cases

  ;; - check does input match THIS node
  ;; - check does input match NEXT node
  ;; - check does input match N 'th node (only for Star)

  ;; - traverse down to element (if incomplete)
  ;; - present results to parent (complete or not)

  (def checka (automata [(or :a :b :c) (or :d :e :f)]))

  (-> checka
      (advance :b)
      (advance :e))

  (-> checka
      (advance :b)
      (advance :c)) ;; FAIL




  (def buy-signals
    (automata [:exponential-ma-has-crossed-below
               :macd-troughs
               (bound (or :rsi :bollinger-band-squeeze :bollinger-band-price-outside) 2 3)]))




  ;; OK
  (-> buy-signals
      (advance :exponential-ma-has-crossed-below)
      (advance :macd-troughs))

  ;; ... TODO
  (def a (-> buy-signals
             (advance :exponential-ma-has-crossed-below)
             (advance :macd-troughs)))

  (advance a :rsi)

  (-> buy-signals
      (advance :exponential-ma-has-crossed-below)
      (advance :macd-troughs)
      (advance :rsi))

  (-> buy-signals
      (advance :exponential-ma-has-crossed-below)
      (advance :macd-troughs)
      (advance :rsi)
      (advance :bollinger-band-squeeze))

  (-> buy-signals
      (advance :exponential-ma-has-crossed-below)
      (advance :macd-troughs)
      (advance :rsi)
      (advance :bollinger-band-squeeze)
      (advance :bollinger-band-price-outside)

      ;; should FAIL here
      (advance :rsi)))
