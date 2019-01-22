(ns automata.core
  (:refer-clojure :exclude [+ * and or not range])
  (:require [clojure.tools.logging :refer [info] :as log]
            [clojure.tools.trace :refer [trace]]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

::empty
::match
::nomatch
::noop

(def is-automaton? map?)
(def exists? (comp clojure.core/not nil?))

(defn start-state? [automaton]
  (-> automaton first nil?))

(defn handle-start-state [run]
  (if (start-state? run)
    (rest run) run))

(defn arrved-at-state? [{matcher :matcher} nxt]
  (= matcher nxt))

(defprotocol ParserCombinator
  "Represents each possible type of parser combinator"

  (get-matcher [_ automaton input])
  (slide-matcher [_ input run])
  (match [_ state input])
  (transition [_ automaton input transition]))

(defrecord Plus [matcher]
  ParserCombinator

  (get-matcher [_ {state+matcher :state run :run} input]
    (if (arrved-at-state? {:matcher matcher} input)
      state+matcher
      (first run)))

  (slide-matcher
    [this input run]

    ;; (info "Plus::arrved-at-state? /" (arrved-at-state? this input)
    ;;       " / state /" this
    ;;       " / next /" input
    ;;       " / run /" run)
    (cond
      (start-state? run) (rest run)
      (arrved-at-state? this input) run
      :else run))

  (match [{this+state :matcher :as tst} {next+state :matcher :as nst} input]
    (let [local-state (if (arrved-at-state? tst input)
                        this+state next+state)]

      ;; (trace (str "local-state / " (pr-str local-state)))
      ;; (trace (str "input / " input))
      ;; (trace (str "(= local-state input) / " (= local-state input)))

      (cond
        (= ::empty input) ::nomatch
        (= local-state input) ::match
        :else ::nomatch)))

  (transition [_ {:keys [states state run history] :as automaton} input transition]
    (let [nxt (first run)
          arrived? (arrved-at-state? state input)
          state' (if arrived? state nxt)
          run' (if arrived? run (rest run))]

      ;; (info "transition::arrved-at-state? /" arrived? " / state /" state " / next /" input)
      (assoc automaton
             :state state'
             :run run'
             :history (concat history [{:state state' :input input :transition transition}])))))

(defrecord Star [matcher]
  ParserCombinator

  (get-matcher [_ {state+matcher :state run :run} input]
    (if (arrved-at-state? {:matcher matcher} input)
      state+matcher
      (first run)))

  (slide-matcher [this input run]

    ;; (info "Star::arrved-at-state? /" (arrved-at-state? this input)
    ;;       " / state /" this
    ;;       " / next /" input
    ;;       " / run /" run)
    (cond
      (start-state? run) (rest run)
      (arrved-at-state? this input) run
      :else run))

  (match [{this+state :matcher :as tst} {next+state :matcher :as nst} input]
    (let [local-state (if (arrved-at-state? tst input)
                        this+state next+state)]

      ;; (trace (str "local-state / " (pr-str local-state)))
      ;; (trace (str "input / " input))
      ;; (trace (str "(= local-state input) / " (= local-state input)))

      (cond
        (= ::empty input) ::noop
        (= local-state input) ::match
        :else ::noop)))

  (transition [_ {:keys [states state run history] :as automaton} input transition]
    (let [nxt (first run)
          arrived? (arrved-at-state? state input)
          state' (if arrived? state nxt)
          run' (if arrived? run (rest run))]

      ;; (info "transition::arrved-at-state? /" arrived? " / state /" state " / next /" input)
      (assoc automaton
             :state state'
             :run run'
             :history (concat history [{:state state' :input input :transition transition}])))))

(defrecord Scalar [matcher]
  ParserCombinator

  (get-matcher [_ {run :run} _]
    (first run))

  (slide-matcher [_ _ run] (rest run))

  (match [{this+state :matcher :as tst} {next+state :matcher :as nst} input]
    (if (= this+state input)
      ::match ::nomatch))

  (transition [_ {:keys [states state run history] :as automaton} input transition]
    (let [nxt (first run)]
      (assoc automaton
             :state nxt
             :run (rest run)
             :history (concat history [{:state nxt :input input :transition transition}])))))


(defn + [a] (->Plus a))
(defn * [a] (->Star a))
(defn scalar [a] (->Scalar a))

(defn bound [a])
(defn or [a])

(defn ? [a])
(defn and [a])
(defn not [a])
(defn range [a])
(defn accept-state? [a])

(defn automaton [states]
  (let [decorate-fn (fn [a]
                      (if (instance? automata.core.ParserCombinator a)
                        a
                        (scalar a)))
        states-decorated (map decorate-fn states)
        run states-decorated]
    {:states run
     :run run
     :state nil
     :history [nil]}))

#_(s/fdef advance
  :args (s/cat :automaton is-automaton?
               :input exists?)
  :ret is-automaton?)

#_(st/instrument)

(defn advance [{states :states
                state+matcher :state
                run :run
                history :history :as automaton} input]

  (let [run' (handle-start-state run)
        state+matcher' (if (nil? state+matcher)
                         (first run)
                         (get-matcher state+matcher automaton input))
        [next-matcher subsequent-matcher] (slide-matcher state+matcher' input run')
        result (match state+matcher' next-matcher input)]

    ;; (println)
    ;; (println "state+matcher' / " state+matcher')
    ;; (println "  > match / " result " > input / " input " > run / " run)
    ;; (println "<>")
    ;; (println "next-matcher / " next-matcher)
    ;; (println "subsequent-matcher / " subsequent-matcher)
    ;; (println)


    (case result
      ::match (transition state+matcher' automaton input ::match)
      ::nomatch (assoc automaton
                       :error {:type :invalid-trasition
                               :input input
                               :matcher state+matcher'})
      (recur (transition subsequent-matcher automaton input ::noop) input))))

(comment


  (def a (automaton [(+ :a) :b :c :d]))


  ;; Should PASS
  (advance a :a)

  (-> a
      (advance :a)
      (advance :b)
      (advance :c))

  (-> a
      (advance :a)
      (advance :a)
      (advance :b))


  ;; Should FAIL
  (advance a :b)
  (advance a :c)


  (def b (automaton [:a (+ :b) :c :d]))


  ;; Should PASS
  (-> b
      (advance :a)
      (advance :b)
      (advance :c))

  (-> b
      (advance :a)
      (advance :b)
      (advance :b)
      (advance :c))


  ;; Should FAIL
  (-> b
      (advance :a)
      (advance :c))


  (def c (automaton [(+ :a) (+ :b) :c :d]))


  ;; Should PASS
  (-> c
      (advance :a)
      (advance :b)
      (advance :c))

  (-> c
      (advance :a)
      (advance :a)
      (advance :b)
      (advance :b)
      (advance :c))


  ;; Should FAIL
  (advance c :c))

(comment


  ;; track count of matches; indicate whether there's currently an FSM match


  ;; implement lists + union
  [[:a :b] [:a :c]]

  ;; accepts i. scalars ii. lists or iii. combinators
  (or :z :x :c :v)
  (or [:a :b] [:a :c])
  (or (* :a) (+ :s))

  ;; accepts a i. scalar ii. list or iii. combinator
  ;;   track amount of times something occurs (refactor existing code)
  ;;   implement nesting
  (bound :a 2 3)
  (bound [:a :c] 2 3)
  (bound (or :z :x :c :v) 2 3)


  (def z (automaton [(bound (or :z :x :c :v) 2 3)]))

  )
