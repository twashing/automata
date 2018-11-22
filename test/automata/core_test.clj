(ns automata.core-test
  (:require [clojure.test :refer :all]
            [automata.core :refer :all]))


(deftest test-scalars

  (let [a (automaton [:a :b :c :d])]

    (testing "basic scalar matches"

      (is (= (advance a :a)
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :a}
              :history '(nil {:state #automata.core.Scalar{:matcher :a} :input :a :transition :automata.core/match})}))

      (is (= (-> a (advance :a) (advance :b))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :b}
              :history '(nil
                          {:state #automata.core.Scalar{:matcher :a} :input :a :transition :automata.core/match}
                          {:state #automata.core.Scalar{:matcher :b} :input :b :transition :automata.core/match})})))

    (testing "basic scalar match errors"

      (is (= (advance a :b)
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state nil
              :history [nil]
              :error {:type :invalid-trasition :input :b :matcher #automata.core.Scalar{:matcher :a}}}))

      (is (= (-> a (advance :a) (advance :a))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :a}
              :history '(nil {:state #automata.core.Scalar{:matcher :a} :input :a :transition :automata.core/match})
              :error {:type :invalid-trasition :input :a :matcher #automata.core.Scalar{:matcher :b}}})))))

(deftest test-star-basic

  (let [b (automaton [(* :a) :b :c :d])]

    (testing "single star"

      (is (= (-> b (advance :a))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :a}
              :history '(nil {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match})})))

    (testing "many stars"

      (is (= (-> b (advance :a) (advance :a))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :a}
              :history
              '(nil
                 {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match}
                 {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match})})))

    (testing "star then scalar"

      (is (= (-> b (advance :a) (advance :b))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :b}
              :history
              '(nil
                 {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match}
                 {:state #automata.core.Scalar{:matcher :b} :input :b :transition :automata.core/match})})))

    (testing "bypass star"

      (is (= (-> b (advance :b))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Scalar{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :b}
              :history
              '(nil
                 {:state #automata.core.Star{:matcher :a} :input :b :transition :automata.core/noop}
                 {:state #automata.core.Scalar{:matcher :b} :input :b :transition :automata.core/match})})))))

(deftest test-star-surrounded

  (let [c (automaton [:a (* :b) :c :d])]

    (testing "scalar then star"

      (is (= (-> c (advance :a))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :a}
              :history '(nil {:state #automata.core.Scalar{:matcher :a} :input :a :transition :automata.core/match})})))

    (testing "scalar then star (many)"

      (is (= (-> c (advance :a) (advance :b) (advance :b))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :b}
              :history '(nil
                          {:state #automata.core.Scalar{:matcher :a} :input :a :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match})})))

    (testing "scalar skip star scalar"
      (is (= (-> c (advance :a) (advance :c))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :c}
              :history '(nil
                          {:state #automata.core.Scalar{:matcher :a} :input :a :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :b} :input :c :transition :automata.core/noop}
                          {:state #automata.core.Scalar{:matcher :c} :input :c :transition :automata.core/match})})))

    (testing "pre-star matching error"

      (is (= (-> c (advance :b))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state nil
              :history [nil]
              :error {:type :invalid-trasition :input :b :matcher #automata.core.Scalar{:matcher :a}}}))

      (is (= (-> c (advance :d))
             {:states '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state nil
              :history [nil]
              :error {:type :invalid-trasition :input :d :matcher #automata.core.Scalar{:matcher :a}}})))))

(deftest test-star-multiple

  (let [d (automaton [(* :a) (* :b) :c :d])]

    (testing "single advance"
      (is (= (-> d (advance :a))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :a}
              :history '(nil {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match})})))

    (testing "multiple advance"
      (is (= (-> d (advance :a) (advance :a))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :a}
              :history '(nil
                          {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match})})))

    (testing "first then second advance"
      (is (= (-> d (advance :a) (advance :b))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :b}
              :history '(nil
                          {:state #automata.core.Star{:matcher :a} :input :a :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match})})))

    (testing "skip first multiple advance second"
      (is (= (-> d (advance :b) (advance :b))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :b}
              :history '(nil
                          {:state #automata.core.Star{:matcher :a} :input :b :transition :automata.core/noop}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match})})))

    (testing "skip first multiple advance second third"
      (is (= (-> d (advance :b) (advance :b) (advance :c))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :c}
              :history '(nil
                          {:state #automata.core.Star{:matcher :a} :input :b :transition :automata.core/noop}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match}
                          {:state #automata.core.Star{:matcher :b} :input :b :transition :automata.core/match}
                          {:state #automata.core.Scalar{:matcher :c} :input :c :transition :automata.core/match})})))

    (testing "skip first + second single advance"
      (is (= (-> d (advance :c))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :d})
              :state #automata.core.Scalar{:matcher :c}
              :history '(nil
                          {:state #automata.core.Star{:matcher :a} :input :c :transition :automata.core/noop}
                          {:state #automata.core.Star{:matcher :b} :input :c :transition :automata.core/noop}
                          {:state #automata.core.Scalar{:matcher :c} :input :c :transition :automata.core/match})})))

    (testing "error match after skipping many stars"
      (is (= (-> d (advance :d))
             {:states '(#automata.core.Star{:matcher :a} #automata.core.Star{:matcher :b} #automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :run '(#automata.core.Scalar{:matcher :c} #automata.core.Scalar{:matcher :d})
              :state #automata.core.Star{:matcher :b}
              :history '(nil
                          {:state #automata.core.Star{:matcher :a} :input :d :transition :automata.core/noop}
                          {:state #automata.core.Star{:matcher :b} :input :d :transition :automata.core/noop})
              :error {:type :invalid-trasition :input :d :matcher #automata.core.Scalar{:matcher :c}}})))))
