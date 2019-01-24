(ns automata.refactor
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.core.match :as m]
            [clojure.tools.trace :refer [trace]]
            [com.rpl.specter :refer :all]))


(defn position->state [a position]
  (-> a :states (nth position)))

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
    (transition-common :this a input)))

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

(defrecord Star [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)]

      (cond

        (= input (->state automaton-error-free))
        (transition-common :this automaton-error-free input)

        (= input (peek-next-state automaton-error-free))
        (transition-common :next automaton-error-free input)

        (= input (peek-nth-state automaton-error-free 2))
        (transition-common :skip automaton-error-free input)

        :else (transition-error :invalid-transition automaton-error-free input)))))

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
            (some next-state [input]))

        (transition-common :next automaton-error-free input)
        (transition-error :invalid-transition automaton-error-free input)))))

(defrecord Automata [matcher]

  ParserCombinator

  (transition [_ automaton input]

    ))


(def parser-combinator? (partial instance? automata.refactor.ParserCombinator))


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
(defn automata [states]
  {:pre [(seqable? states)
         ((comp clojure.core/not parser-combinator?) states)]}

  (println states)
  (let [decorate-fn #(cond
                       (instance? automata.refactor.ParserCombinator %) %
                       (seqable? %) (recur %)
                       :else (scalar %))
        states-decorated (as-> states s
                           (map decorate-fn s)
                           (concat (list (scalar :START)) s (list (scalar :END))))]

    (-> (->Automata states-decorated)
        (assoc :position 0
               :history []))))

(defmacro foo [states]

  (println (seqable? states))
  (println (map #(instance? automata.refactor.ParserCombinator %) states)))

(defn advance [automaton input]

  (let [parser-combinator? (->> (->matcher automaton)
                                (instance? automata.refactor.ParserCombinator))
        automaton-state (->state automaton)
        identity-guard (fn [inp]
                         (= inp
                            (->> automaton :states rest butlast (map :matcher))))]

    (m/match [parser-combinator? automaton-state input]


             ;; START / END states
             [_ :START (_ :guard identity-guard)] (identity automaton)
             [_ :START _] (transition (peek-next-matcher automaton) automaton input)
             [_ :END _] (identity automaton)


             ;; ParserCombinator states
             [true _ _] (transition (->matcher automaton) automaton input)

             [_ _ _] :WTF)))

#_(defn automaton [states]
  (automata states))

(comment ;; SCALAR, STAR, PLUS

  ;; A
  (def a (automaton [:a :b :c :d]))
  ;; (match a '(:a :b :c :d) [:a :b :c :d])
  ;; (match a :a :a)
  ;; (match a '(:a :b :c :d) :a) ;; false

  (advance a :a)
  (advance a :z)
  (advance a [:a :b :c :d])


  ;; C
  (def c (automaton [(* :a) :b :c :d]))

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
  (def d (automaton [(+ :a) :b :c :d]))

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
  (def e (automaton [(bound :a 2 3) :b :c :d]))

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
  (def f (automaton [(bound :a 1 2) :b :c :d]))

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
  (def ff (automaton [(bound :a 0 1) :b :c :d]))

  (advance ff :a)
  (advance ff :b)

  ;; FAIL
  (-> ff
      (advance :a)
      (advance :a))


  ;;; FFF
  (def fff (automaton [(? :a) :b :c :d]))

  (advance fff :a)
  (advance fff :b)

  ;; FAIL
  (-> fff
      (advance :a)
      (advance :a))

  )

(comment ;; OR

  (def g (automaton [(or :z :x :c :v) :b :c]))

  (advance g :z)
  (advance g :c)

  (-> g
      (advance :z)
      (advance :z)) ;; FAIL

  (advance g :b) ;; FAIL
  )

(comment ;; NESTING

  ;; (or [:a :b] [:a :c])


  (foo [(* (or :a :b :c))])
  (automata [(* (or :a :b :c))])
  (automata [(* [:a :b])])

  (def TREE-VALUES
	  (recursive-path [] p
	                  (cond-path
                      vector? (continue-then-stay ALL p)
                      seqable? [ALL p])))

  (select TREE-VALUES '[(* (+ [:a :b :c]))])
  (transform TREE-VALUES #(into #{} %) '[(* (+ [:a :b :c]))] )

  (do

    ;; A
    (def a (automaton [:a :b :c :d]))


    ;; C
    (def c (automaton [(* :a) :b :c :d]))


    ;; D
    (def d (automaton [(+ :a) :b :c :d]))


    ;; E
    (def e (automaton [(bound :a 2 3) :b :c :d]))


    ;; F
    (def f (automaton [(bound :a 1 2) :b :c :d]))


    ;; FF
    (def ff (automaton [(bound :a 0 1) :b :c :d]))


  ;;; FFF
    (def fff (automaton [(? :a) :b :c :d]))


    ;; G
    (def g (automaton [(or :z :x :c :v) :b :c])))


  (automata (* (or :a :b :c))) ;; FAIL we have to start with an automata

  (def one (automata [(* (or :a :b :c))])) ;; combinator / combinator
  (def two (automata [(* [:a :b])])) ;; combinator / automata
  (def three (automata [[:a :b] [:a :c]])) ;; automata > automata


  ;; H
  (def h (automaton [(* (or :a :b :c)) :z :x]))


  (advance h :a)
  (advance h :b)
  (advance h :c)


  (+ (or :a :b :c))
  (bound (or :a :b :c) 2 3)
  (+ (or [:a :b] [:a :c])))
