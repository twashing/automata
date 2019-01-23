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

(defn start-state? [automaton]
  (-> automaton :state :START))


(defprotocol ParserCombinator
  (transition [_ automaton input])
  (match [_ state input]))

(defrecord Scalar [matcher]
  ParserCombinator
  (transition [_ automaton input]
    (let [automaton-error-free (dissoc automaton :error)]
      (if (= input (peek-next-state automaton-error-free))
        (transition-common :next automaton-error-free input)
        (transition-error :invalid-transition automaton-error-free input))))
  (match [_ state input]))

(defrecord Plus [matcher]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)]

      (cond

        (= input (peek-next-state automaton-error-free))
        (transition-common :next automaton-error-free input)

        (= input (->state automaton-error-free))
        (transition-common :this automaton-error-free input)

        :else (transition-error :invalid-transition automaton-error-free input))))

  (match [_ state input]))

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

        :else (transition-error :invalid-transition automaton-error-free input))))

  (match [_ state input]))

(defrecord Bound [matcher x y]

  ParserCombinator

  (transition [_ automaton input]

    (let [automaton-error-free (dissoc automaton :error)
          transition-proposition (transition-common :this automaton-error-free input)
          {:keys [position history]} (trace transition-proposition)
          history-at-position (if (empty? history)
                                []
                                (last history))
          size-at-position (count history-at-position)]

      (m/match [transition-proposition size-at-position]

               ;; BEFORE BOUNDS
               [(_ :guard #(= input (peek-next-state %)))
                (_ :guard #(< % x))]
               (as-> automaton-error-free a
                 (transition-error :input-out-of-bounds a input)
                 (transition-common :increment-position a input))

               [(_ :guard #(= input (->state %)))
                (_ :guard #(< % x))]
               (transition-error :input-out-of-bounds automaton-error-free input)

               ;; WITHIN BOUNDS
               [(_ :guard #(= input (->state %)))
                (_ :guard #(clojure.core/and (>= % x)
                                (<= % y)))]
               transition-proposition

               :else (transition-error :invalid-transition automaton-error-free input))))
  (match [_ state input]))

(comment

  (def e (automaton [(bound :a 2 3) :b :c :d]))

  (advance e :a) ;; FAIL

  (-> e
      (advance :a)
      (advance :a))

  (-> e
      (advance :a)
      (advance :a)
      (advance :a))

  (-> e
      (advance :a)
      (advance :a)
      (advance :a)
      (advance :a)) ;; FAIL

  )

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
(defn bound [a x y] (->Bound a x y))
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
      (advance :c))


  ;; E
  (def e (automaton [(bound :a 2 3) :b :c :d]))



  (automaton [(or :z :x :c :v)])


  (or [:a :b] [:a :c])
  (or [:a :b] [:a :c])

  (+ (or :a :b :c))
  (* (or :a :b :c))
  (bound (or :a :b :c) 2 3)

  (+ (or [:a :b] [:a :c]))

  )
