(ns automata.refactor
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.core.match :as m]
            [clojure.tools.trace :refer [trace]]
            [com.rpl.specter :refer :all]))


(declare parser-combinator?)
(declare automata?)
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

#_(defn start-state? [automaton]
  (-> automaton :state :START))


(defprotocol ParserCombinator
  (transition [_ automaton input]))

(defrecord Scalar [matcher]
  ParserCombinator
  (transition [_ automaton input]
    (let [automaton-error-free (dissoc automaton :error)]
      (if (= input (peek-next-state automaton-error-free))
        (transition-common :next automaton-error-free input)
        (transition-error :invalid-transition automaton-error-free input)))))

(defrecord Plus [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)]

      (cond

        (= input (peek-next-state automaton-error-free))
        (transition-common :next automaton-error-free input)

        (= input (->state automaton-error-free))
        (transition-common :this automaton-error-free input)

        :else (transition-error :invalid-transition automaton-error-free input)))))

(defn complete? [input]
  (if ((comp clojure.core/not automata?) input)
    true
    (->> input
         trace
         :matcher
         trace
         (map :matcher)
         trace
         (keep-indexed
           #(if (= %2 :END) %1))
         trace
         first
         dec
         (= (:position input)))))

(defmulti transition-local (fn [dispatch _ _] dispatch))

(defmethod transition-local :star [_ automaton-error-free input]

  (cond

    (= input (->state automaton-error-free))
    (transition-common :this automaton-error-free input)

    (= input (peek-next-state automaton-error-free))
    (transition-common :next automaton-error-free input)

    (= input (peek-nth-state automaton-error-free 2))
    (transition-common :skip automaton-error-free input)

    :else (transition-error :invalid-transition automaton-error-free input)))

(defrecord Star [matcher]

  ParserCombinator

  (transition [this automaton input]

    (let [automaton-error-free (dissoc automaton :error)
          next-position (-> automaton :position inc)
          next-state (peek-next-state automaton-error-free)
          automaton->with-updated-node (fn [next-position input automaton-error-free]
                                         (transform [:matcher (nthpath next-position) :matcher]
                                                    (constantly input)
                                                    automaton-error-free))]
      ;; (trace automaton)
      ;; (trace this)
      ;; (trace next-state)

      '[[is-automata incomplete] / nest

        ;; when complete
        ;;   add entire automata -> history
        ;;   advance position by 1

        [is-automata complete] / check-repeat [repeat | next]
        [not-automata _] / goto B]

      (m/match [(automata? next-state) (complete? next-state)]

               [true false] (as-> input i
                              (transition next-state next-state i)
                              (automaton->with-updated-node next-position i automaton-error-free))
               [_ true] (transition-local :star automaton-error-free input)))))


(comment ;; FOO

  (def one (automata [(* [:a :b]) :c])) ;; combinator / automata

  ;; one
  (advance one :a)

  (-> one
      (advance :a)
      (advance :b))

  (-> one
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)))

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

  (automata (* (or :a :b :c))) ;; FAIL we have to start with an automata

  (def one (automata [(* [:a :b]) :c])) ;; combinator / automata
  (def two (automata [(* (or :a :b :c))])) ;; combinator / combinator
  (def three (automata [[:a :b] [:a :c]])) ;; automata > automata


  ;; one
  (advance one :a)

  (-> one
      (advance :a)
      (advance :b))

  (-> one
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b))

  (-> one
      (advance :a)
      (advance :b)
      (advance :c))

  (-> one
      (advance :a)
      (advance :b)
      (advance :a)
      (advance :b)
      (advance :c))

  (advance one :c)

  (-> one
      (advance :a)
      (advance :b)
      (advance :c)) ;; FAIL

  ;; H
  (def h (automata [(* (or :a :b :c)) :z :x]))


  (advance h :a)
  (advance h :b)
  (advance h :c)


  (+ (or :a :b :c))
  (bound (or :a :b :c) 2 3)
  (+ (or [:a :b] [:a :c])))

(defrecord Bound [matcher x y]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)
          transition-proposition (if (= (peek-next-state automaton-error-free) input)
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

               :else (transition-error :invalid-transition automaton-error-free input)))))

(defrecord Or [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)
          next-state (peek-next-state automaton-error-free)]

      (if (clojure.core/and
            (set? next-state)
            (some (->> next-state
                       (map :matcher)
                       (into #{}))
                  [input]))

        (transition-common :next automaton-error-free input)
        (transition-error :invalid-transition automaton-error-free input)))))

(defrecord Automata [matcher]

  ParserCombinator

  #_(transition [_ automaton input]

    (println "Here")
    (trace [automaton input])
    (trace [(peek-next-matcher automaton) (peek-next-state automaton)])

    (let [parser-combinator? (->> (->matcher automaton)
                                  (instance? automata.refactor.ParserCombinator))
          automaton-state (->state automaton)
          identity-guard (fn [inp]
                           (= inp
                              (->> automaton :matcher rest butlast (map :matcher))))]

      (trace [parser-combinator? automaton-state])))

  (transition [_ automaton input]

    (println "Here")
    (let [parser-combinator? (->> (->matcher automaton)
                                  (instance? automata.refactor.ParserCombinator))
          automaton-state (->state automaton)
          ;; _ (trace [parser-combinator? automaton-state])

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


(def parser-combinator? (partial instance? automata.refactor.ParserCombinator))
(def automata? (partial instance? automata.refactor.Automata))

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
         ((comp clojure.core/not parser-combinator?) states)]}

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
        walker-set #(if (set? %)
                      (->> (map scalar %)
                           (into #{}))
                      %)]

    (->> states
         (clojure.walk/postwalk walker-vector-list-scalar)
         (clojure.walk/postwalk walker-set))))

(defn advance [automaton input]

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

             [_ _ _] :WTF)))


;; TODO implement navigation for nesting

(comment ;; SCALAR, STAR, PLUS

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
      (advance :c))

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
        (advance :a))))

(comment ;; OR

  (def g (automata [(or :z :x :c :v) :b :c]))

  (advance g :z)
  (advance g :c)

  (-> g
      (advance :z)
      (advance :z)) ;; FAIL

  (advance g :b) ;; FAIL
  )




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
