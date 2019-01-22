(ns automata.refactor
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.core.match :as m]
            [clojure.tools.trace :refer [trace]]
            [instaparse.core :as insta]))


;; scalar (literal)
;; list (FSM)
;; combinator
;;
;; - track count
;;   ? is within bound

;; - track position
;;   ! at a match, return position

;; - advance
;;   ? are we at start of FSM
;;   ! transition to subsequent matcher

;; ParserCombinator
;;   disambiguate between i. the matcher and ii. current state
;;
;; Star
;;   cannot have same stars next to each other
;;
;; Plus
;;   cannot have same stars next to each other

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
  (assoc automaton :history (-> automaton :history (concat [input]))))

(defmethod transition-common :next [_ automaton input]
  (-> (transition-common :this automaton input)
      (assoc :position (-> automaton :position inc))))

(defmethod transition-common :skip [_ automaton input]
  (assoc automaton :position (-> automaton :position (clojure.core/+ 2))))

(defn transition-error [automaton input]
  (assoc automaton
         :error {:type :invalid-transition
                 :input input}))

(defn start-state? [automaton]
  (-> automaton :state :START))


(defprotocol ParserCombinator
  (transition [_ automaton input])
  (match [_ state input]))

(defrecord Scalar [matcher]
  ParserCombinator
  (transition [_ automaton input]
    (if (= input (peek-next-state automaton))
      (transition-common :next automaton input)
      (transition-error automaton input)))
  (match [_ state input]))

(defrecord Plus [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (cond

      (= input (->state automaton))
      (transition-common :this automaton input)

      (= input (peek-next-state automaton))
      (transition-common :next automaton input)

      :else (transition-error automaton input)))

  (match [_ state input]))

(defrecord Star [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (cond

      (= input (->state automaton))
      (transition-common :this automaton input)

      (= input (peek-next-state automaton))
      (transition-common :next automaton input)

      (= input (peek-nth-state automaton 2))
      (transition-common :skip automaton input)

      :else (transition-error automaton input)))

  (match [_ state input]))

(defrecord Bound [matcher]
  ParserCombinator
  (transition [_ automaton input])
  (match [_ state input]))

(defrecord Or [matcher]
  ParserCombinator
  (transition [_ automaton input])
  (match [_ state input]))


;; :state nil - means start state
;; :state (<states>) - means end state

;; :state START
;; :state END


(defn + [a] (->Plus a))
(defn * [a] (->Star a))
(defn scalar [a] (->Scalar a))
(defn bound [a] (->Bound a))
(defn or [a] (->Or a))

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

(defn automaton [states]
  (let [decorate-fn #(cond
                       (instance? automata.refactor.ParserCombinator %) %
                       (seqable? %) (automaton %)
                       :else (scalar %))
        states-decorated (as-> states s
                           (map decorate-fn s)
                           (concat (list (scalar :START)) s (list (scalar :END))))]

    (-> {:states states-decorated}
        (assoc :position 0 :history []))))

(comment

  ;; A
  (def a (automaton [:a :b :c :d]))
  ;; (match a '(:a :b :c :d) [:a :b :c :d])
  ;; (match a :a :a)
  ;; (match a '(:a :b :c :d) :a) ;; false

  (advance a :a)
  (advance a :z)
  (advance a [:a :b :c :d])


  ;; B TODO
  ;; (def b (automaton [[:a :b] [:c :d]]))
  ;; (match b)
  ;; (advance b :a)


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
  (do
    (def d (automaton [(+ :a) :b :c :d]))
    (def d1 (advance d :a)))

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


  (automaton [(bound :a 2 3)])
  (automaton [(or :z :x :c :v)])


  (or [:a :b] [:a :c])
  (or [:a :b] [:a :c])

  (+ (or :a :b :c))
  (* (or :a :b :c))
  (bound (or :a :b :c) 2 3)

  (+ (or [:a :b] [:a :c]))

  )
