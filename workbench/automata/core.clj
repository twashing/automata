(ns automata.core
  (:refer-clojure :exclude [+ * and or not range]))


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
  (-> d (advance :d)) ;; error
  )
