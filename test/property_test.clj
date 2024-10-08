(ns property-test
  {:clj-kondo/config '{:lint-as {clojure.test.check.clojure-test/defspec clojure.test/deftest
                                 clojure.test.check.properties/for-all clojure.core/let}}}
  (:require [clojure.test :refer [is run-tests]]
            [avl-dict :refer [avl-balanced? avl-contains?
                              avl-equal? concat-avl
                              delete empty-avl filter-values
                              fold-left fold-right get-value
                              insert keys-avl to-list to-tree
                              visualize]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]])
  (:import (java.util Date)))

(def iteration_num 1000)
(def samples_count 100)

(def gen-key
  gen/small-integer)

(def gen-value
  (gen/one-of [gen/string gen/boolean gen/keyword gen/small-integer]))

(def gen-pair
  (gen/tuple gen-key gen-value))

(def gen-date
  (gen/fmap #(Date. %)
            (gen/choose (- (System/currentTimeMillis) (* 1000 60))
                        (System/currentTimeMillis))))

(def gen-int-pair
  (gen/tuple gen-key gen/small-integer))

(def gen-date-pair
  (gen/not-empty (gen/tuple gen-date gen-date)))

(def gen-poly-pair (gen/one-of [(gen/vector (gen/not-empty gen-pair) samples_count)
                                (gen/vector (gen/not-empty gen-int-pair) samples_count)
                                (gen/vector (gen/not-empty gen-date-pair) samples_count)]))

;; Test to verify that a randomly selected key from a set of inserted key-value pairs exists within the AVL tree
;; and that the AVL tree remains balanced after insertions.
(defspec contains-random-element-test iteration_num
  ;; Verifies that a randomly selected key from inserted pairs exists in the AVL tree and the tree remains balanced.
  (prop/for-all [tuples gen-poly-pair]
                (let [avl (to-tree tuples)
                      [key] (rand-nth tuples)]
                  (is (avl-contains? avl key) "AVL tree should contain the randomly selected key.")
                  (is (avl-balanced? avl) "AVL tree should remain balanced after insertions."))))

(defspec contains-all-elements-test iteration_num
  ;; Verifies that all inserted keys exist in the AVL tree and the tree remains balanced.
  (prop/for-all [tuples gen-poly-pair]
                (let [avl (to-tree tuples)]
                  (is (every? (fn [[k _]] (avl-contains? avl k)) tuples) "AVL tree should contain all inserted keys.")
                  (is (every? (fn [[k _]] (avl-contains? avl k)) tuples) "AVL tree should contain all inserted keys.")
                  (is (avl-balanced? avl) "AVL tree should remain balanced after insertions."))))

(defspec insert-and-retrieve-test iteration_num
  ;; Verifies that an inserted key-value pair can be retrieved and the AVL tree remains balanced.
  (prop/for-all [tuples (gen/vector-distinct (gen/tuple gen-key gen-value))
                 key gen-key
                 value gen-value]
                (try
                  (let [avl (to-tree tuples)
                        new-avl (insert avl key value)]
                    (is (avl-contains? new-avl key) "AVL tree should contain the inserted key.")
                    (is (avl-balanced? new-avl) "AVL tree should remain balanced after insertion.")
                    (is (= value (get-value new-avl key)) "Retrieved value should match the inserted value."))
                  (catch Exception e
                    (println "Error occurred. Visualizing tree:")
                    (println "Error message:" (.getMessage e))
                    (println "Tuples:" tuples)
                    (println "Inserted key:" key)
                    (println "Inserted value:" value)
                    (throw e)))))

(defspec insert-and-retrieve-date-test iteration_num
  ;; Verifies that an inserted key-value pair can be retrieved and the AVL tree remains balanced.
  (prop/for-all [tuples (gen/vector-distinct (gen/tuple gen-date gen-date))
                 key gen-date
                 value gen-date]
                (try
                  (let [avl (to-tree tuples)
                        new-avl (insert avl key value)]
                    (is (avl-contains? new-avl key) "AVL tree should contain the inserted key.")
                    (is (avl-balanced? new-avl) "AVL tree should remain balanced after insertion.")
                    (is (= value (get-value new-avl key)) "Retrieved value should match the inserted value."))
                  (catch Exception e
                    (println "Error occurred. Visualizing tree:")
                    (println "Error message:" (.getMessage e))
                    (println "Inserted key:" key)
                    (println "Inserted value:" value)
                    (throw e)))))

(defspec delete-and-retrieve-test iteration_num
  ;; Verifies that a deleted key does not exist in the AVL tree and the tree remains balanced.
  (prop/for-all [tuples gen-poly-pair]
                (let [avl (to-tree tuples)
                      [key] (rand-nth tuples)
                      new-avl (delete avl key)]
                  (is (not (avl-contains? new-avl key)) "AVL tree should not contain the deleted key.")
                  (is (avl-balanced? new-avl) "AVL tree should remain balanced after deletion."))))

(defspec fold-find-sum iteration_num
  ;; Verifies that the sum of keys in the AVL tree matches the expected sum of unique numeric keys.
  (prop/for-all [tuples (gen/let [keys (gen/fmap (fn [pairs] (vec (map first pairs)))
                                                 (gen/set (gen/tuple gen-key gen-value) {:num-elements samples_count}))
                                  values (gen/vector-distinct gen-value {:num-elements samples_count :max-tries 40})]
                          (map (fn [k v] [k v]) keys values))]
                (let [tree (to-tree tuples)
                      expected-sum (->> tuples
                                        (filter (fn [[k _]] (number? k)))
                                        (map first)
                                        (distinct)
                                        (reduce + 0))
                      sum-left         (fold-left (fn [acc k _] (+ acc k))
                                                  0
                                                  tree)
                      sum-right (fold-right (fn [acc k _] (+ acc k))
                                            0
                                            tree)]
                  (is (= sum-left expected-sum))
                  (is (= sum-right expected-sum)))))

(defspec concat-2-avls-all-elements-exist iteration_num
  ;; Verifies that concatenating two AVL trees results in a balanced AVL tree containing all keys from both input trees.
  (prop/for-all [tuples1 (gen/vector (gen/not-empty gen-pair) samples_count)
                 tuples2 (gen/vector (gen/not-empty gen-pair) samples_count)]
                (let [avl1 (to-tree tuples1)
                      avl2 (to-tree tuples2)
                      concated-avl (concat-avl avl1 avl2)
                      all-keys (set (concat (keys-avl avl1) (keys-avl avl2)))]
                  (is (every? (fn [k] (avl-contains? concated-avl k)) all-keys)
                      "Concatenated AVL should contain all keys from both input AVLs")
                  (is (avl-balanced? concated-avl)
                      "Concatenated AVL should be balanced"))))

(defspec filter-values-test iteration_num
  ; Verifies that filtering the AVL tree based on a predicate results in a balanced AVL tree containing only elements that satisfy the predicate.
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-int-pair) 2)
                 pred (gen/elements [pos? neg? zero? odd? even?])]
                (let [avl (to-tree tuples)
                      filtered-avl (filter-values (fn [_ v] (pred v)) avl)
                      all-satisfy-pred? (every? (fn [[_ v]] (pred v)) (partition 2 (to-list filtered-avl)))
                      is-balanced? (avl-balanced? filtered-avl)]
                  (when (or (not all-satisfy-pred?) (not is-balanced?))
                    (println "Original AVL:")
                    (visualize avl)
                    (println "Filtered AVL:")
                    (visualize filtered-avl))
                  (is all-satisfy-pred?
                      "Filtered AVL should contain only elements that satisfy the predicate.")
                  (is is-balanced?
                      "Filtered AVL should remain balanced."))))
;; Verify Monoid laws
;; 1. Identity element: mappend empty v = v
;; 2. Associativity: mappend (mappend v1 v2) v3 = mappend v1 (mappend v2 v3)
(defspec monoid-first-law-test iteration_num
  ;; Verifies the monoid identity law for AVL trees.
  (prop/for-all [tuples (gen/vector (gen/not-empty gen-pair) samples_count)]
                (let [avl (to-tree tuples)
                      empty-avl (empty-avl)
                      concatenated-avl (concat-avl avl empty-avl)]
                  (is (avl-equal? concatenated-avl avl) "Identity element: mappend empty v = v")
                  (is (avl-balanced? concatenated-avl) "Concatenated AVL should remain balanced"))))

(defspec monoid-second-law-test iteration_num
  ;; Verifies the monoid associativity law for AVL trees.
  (prop/for-all [tuples1 (gen/vector (gen/not-empty gen-pair) samples_count)
                 tuples2 (gen/vector (gen/not-empty gen-pair) samples_count)
                 tuples3 (gen/vector (gen/not-empty gen-pair) samples_count)]
                (let [avl1 (to-tree tuples1)
                      avl2 (to-tree tuples2)
                      avl3 (to-tree tuples3)
                      concatenated-avl1 (concat-avl (concat-avl avl1 avl2) avl3)
                      concatenated-avl2 (concat-avl avl1 (concat-avl avl2 avl3))]
                  (is (avl-equal? concatenated-avl1 concatenated-avl2)
                      "Associativity: mappend (mappend v1 v2) v3 = mappend v1 (mappend v2 v3)")
                  (is (avl-balanced? concatenated-avl1) "Concatenated AVL should remain balanced")
                  (is (avl-balanced? concatenated-avl2) "Concatenated AVL should remain balanced"))))

(run-tests)
