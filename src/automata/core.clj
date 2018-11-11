(ns automata.core
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))


(defn + [a])
(defn * [a])
(defn ? [a])
(defn bound [a])
(defn and [a])
(defn or [a])
(defn not [a])
(defn range [a])
::any
(defn accept-state? [a])


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
