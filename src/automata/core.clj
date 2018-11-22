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

  (slide-matcher [_ input run])
  (match [this state input])
  (transition [this automaton input transition]))

(defrecord Plus [matcher]
  ParserCombinator

  (slide-matcher [_ input run])

  (match [_ state input]
    :baz)

  (transition [_ automaton input transition]))

(defrecord Star [matcher]
  ParserCombinator

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
        :else ::noop
        ;; :else ::nomatch
        )))

  (transition [_ {:keys [states state run history] :as automaton} input transition]
    (let [_ matcher

          nxt (first run)
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

  (slide-matcher [_ _ run] run)

  (match [{this+state :matcher :as tst} {next+state :matcher :as nst} input]
    (if (= next+state input)
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
(defn ? [a])
(defn bound [a])
(defn and [a])
(defn or [a])
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
                history :history :as automaton}
               input]

  (let [real-run (handle-start-state run)
        state+matcher' (cond
                         (nil? state+matcher) (first run)
                         ;; (->> state+matcher :matcher (= input) not) (first run)
                         :else state+matcher)
        [next-matcher subsequent-matcher] (slide-matcher state+matcher' input real-run)

        ;; _ (println)
        ;; _ (println "state+matcher' / " state+matcher')
        ;; _ (println "middle matcher / " (->> state+matcher :matcher (= input)) " / input / " input " / run / " run)
        ;; _ (println ">")
        ;; _ (println "next-matcher / " next-matcher)
        ;; _ (println "subsequent-matcher / " subsequent-matcher)

        result (match state+matcher' next-matcher input)]

    (case result
      ::match (transition state+matcher' automaton input ::match)
      ::nomatch (assoc automaton
                       :error {:type :invalid-trasition
                               :input input
                               :matcher state+matcher'})
      (recur (transition subsequent-matcher automaton input ::noop) input))))


(comment


  (def a (automaton [:a :b :c :d]))

  (advance a :a)
  (advance a :b) ;; error
  (-> a (advance :a) (advance :a)) ;; error
  (-> a (advance :a) (advance :b)))

(comment

  (def b (automaton [(* :a) :b :c :d]))

  (-> b (advance :a))
  (-> b (advance :a) (advance :a))
  (-> b (advance :a) (advance :b))
  (-> b (advance :b)))

(comment

  (def c (automaton [:a (* :b) :c :d]))

  (-> c (advance :a))
  (-> c (advance :a) (advance :b) (advance :b))


  ;; TODO fix
  (-> c (advance :a) (advance :c))
  (-> c (advance :b))
  (-> c (advance :d)))

(comment

  (def d (automaton [(* :a) (* :b) :c :d]))

  (-> d (advance :a))
  (-> d (advance :a) (advance :a))
  (-> d (advance :a) (advance :b))
  (-> d (advance :b) (advance :b))
  (-> d (advance :b) (advance :b) (advance :c))
  (-> d (advance :c))

  ;; TODO fix
  (-> d (advance :d)) ;; error
  )
