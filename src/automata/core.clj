(ns automata.core
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))


(defprotocol ParserCombinator
  "Represents each possible type of parser combinator"
  (advance2 [this automaton input]))

(defrecord Plus [state]
  ParserCombinator
  (advance2 [_ automaton input]
    :baz))

(defrecord Star [state]
  ParserCombinator
  (advance2 [_ {:keys [states state run history] :as automaton} input]
    (println automaton)
    (println input)))

(def automaton {:states states
                :state :foo
                :run []
                :history [nil]})

;; (advance2 star automaton :foo)


(defrecord Scalar [state]
  ParserCombinator
  (advance2 [_ {:keys [states state run history] :as automaton} input]
    :bar))


(defn + [a] (->Plus a))
(defn * [a] (->Star a))
(defn scalar [a] (->Scalar a))

(def plus (+ :foo))
(def star (* :foo))
(def states [(* :foo) (scalar :bar)])


;; (advance2 star states :foo)
;; What state are we in
;; What are the possible transitions (identity, other)


(defn ? [a])
(defn bound [a])
(defn and [a])
(defn or [a])
(defn not [a])
(defn range [a])
::any
(defn accept-state? [a])


;; (advance a :a)
;; {:states (nil :a :b :c), :run (:a :b :c), :state :a, :history (nil)}


;; ========

(def is-automaton? map?)
(def exists? (comp clojure.core/not nil?))


(defn start-state? [automaton]
  (-> automaton first nil?))

(defn automaton [states]
  (let [run (concat [nil] states)]
    {:states run
     :run run
     :state nil
     :history []}))

(defn transition [{:keys [states state run history] :as automaton}]
  (let [[current nxt] run]
    (assoc automaton
           :state nxt
           :run (rest run)
           :history (concat history [current]))))

(defn advance [{:keys [states state run] :as automaton} input]

  (let [current-state (first run)
        allowed-transitions (-> run second list set)
        allowed? (some allowed-transitions #{input})]

    (if allowed?
      (transition automaton)
      (assoc automaton
             :error {:type :invalid-trasition
                     :input input
                     :allowed-transitions allowed-transitions}))))

(s/fdef advance
  :args (s/cat :automaton is-automaton?
               :input exists?)
  :ret is-automaton?)

;; [:a :b :c :d]
;; [:a (a/+ :b) :c :d]
;; [:a (a/* :b) :c :d]
;; [:a (a/? :b) :c :d]
;; [:a (a/bound 2 4 :b) :c :d]
;; [:a (a/and :b :c) :d]
;; [:a (a/or :b :c) :d]
;; [:a (a/not :b) :c :d]
;; [1 (a/range 2 10) 11]


(st/instrument)


(comment

  (advance a :a)
  {:states (nil :a :b :c), :run (:a :b :c), :state :a, :history (nil)}

  (advance a :b)
  {:states (nil :a :b :c),
   :run (nil :a :b :c),
   :state nil,
   :history [],
   :error {:type :invalid-trasition, :input :b, :allowed-transitions #{:a}}}

  (-> a (advance :a) (advance :a))
  {:states (nil :a :b :c),
   :run (:a :b :c),
   :state :a,
   :history (nil),
   :error {:type :invalid-trasition, :input :a, :allowed-transitions #{:b}}}

  (-> a (advance :a) (advance :b))
  {:states (nil :a :b :c), :run (:b :c), :state :b, :history (nil :a)})
