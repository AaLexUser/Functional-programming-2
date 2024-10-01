(ns units-test
  (:require [clojure.test :refer [deftest testing is run-tests]]
            [avl-dict :refer [avl-balanced? balance-factor
                              concat-avl delete
                              empty-avl filter-values
                              fold-left fold-right
                              get-value height
                              insert keys-avl
                              map-avl to-list to-tree]]))

; Unit Tests

(deftest test-empty-avl
  (testing "Empty AVL tree"
    (is (nil? (empty-avl)))
    (is (= 0 (height (empty-avl))))))

(deftest test-insert-and-get
  (testing "Inserting and retrieving values"
    (let [tree (-> (empty-avl)
                   (insert 5 "five")
                   (insert 3 "three")
                   (insert 7 "seven"))]
      (is (= "five" (get-value tree 5)))
      (is (= "three" (get-value tree 3)))
      (is (= "seven" (get-value tree 7)))
      (is (nil? (get-value tree 1)))
      (is (= true (avl-balanced? tree))))))

(deftest test-delete
  (testing "Deleting nodes"
    (let [tree (-> (empty-avl)
                   (insert 5 "five")
                   (insert 3 "three")
                   (insert 7 "seven")
                   (delete 3))]
      (is (nil? (get-value tree 3)))
      (is (= "five" (get-value tree 5)))
      (is (= "seven" (get-value tree 7)))
      (is (= true (avl-balanced? tree))))))

(deftest test-balance
  (testing "Tree balancing"
    (let [tree (reduce (fn [t i] (insert t i (str i)))
                       (empty-avl)
                       (range 1 8))]
      (is (<= (Math/abs (balance-factor tree)) 1))
      (is (<= (height tree) 4))
      (is (= true (avl-balanced? tree))))))

(deftest test-to-list
  (testing "Converting tree to sorted list"
    (let [tree (-> (empty-avl)
                   (insert 5 "five")
                   (insert 3 "three")
                   (insert 7 "seven"))]
      (is (= '(3 "three" 5 "five" 7 "seven")
             (to-list tree)))
      (is (= true (avl-balanced? tree))))))

(deftest test-keys-avl
  (testing "keys-avl function"
    (let [avl (to-tree [[1 "a"] [2 "b"] [3 "c"]])]
      (is (= (keys-avl avl) '(1 2 3)) "Should return all keys in the AVL tree in sorted order"))
    (let [empty-avl (empty-avl)]
      (is (= (keys-avl empty-avl) '()) "Should return an empty list for an empty AVL tree"))))


(deftest test-map-avl
  (testing "Mapping over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 1)
                   (insert 2 2)
                   (insert 3 3))
          mapped-tree (map-avl (fn [_ v] (* v 2)) tree)]
      (is (= 2 (get-value mapped-tree 1)))
      (is (= 4 (get-value mapped-tree 2)))
      (is (= 6 (get-value mapped-tree 3)))
      (is (= true (avl-balanced? mapped-tree))))))

(deftest test-fold-right-sum
  (testing "Folding right over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 1)
                   (insert 2 2)
                   (insert 3 3))
          sum (fold-right (fn [acc _ v] (+ acc v)) 0 tree)]
      (is (= 6 sum))
      (is (= true (avl-balanced? tree))))))

(deftest test-fold-left-sum
  (testing "Folding left over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 1)
                   (insert 2 2)
                   (insert 3 3))
          sum (fold-left (fn [acc _ v] (+ acc v)) 0 tree)]
      (is (= 6 sum))
      (is (= true (avl-balanced? tree))))))



(deftest test-fold-right-string
  (testing "Folding right over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 "a")
                   (insert 2 "b")
                   (insert 3 "c"))
          result (fold-right (fn [acc _ value]
                               (str acc value)) "" tree)]
      (is (= "cba" result))
      (is (= true (avl-balanced? tree))))))

(deftest test-fold-left-string
  (testing "Folding left over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 "a")
                   (insert 2 "b")
                   (insert 3 "c"))
          result (fold-left (fn [acc _ value]
                              (str acc value)) "" tree)]
      (is (= "abc" result))
      (is (= true (avl-balanced? tree))))))


(deftest test-concat-avl
  (testing "Concatenating two AVL trees"
    (let [tree1 (-> (empty-avl)
                    (insert 1 "one")
                    (insert 2 "two"))
          tree2 (-> (empty-avl)
                    (insert 3 "three")
                    (insert 4 "four"))
          combined (concat-avl tree1 tree2)]
      (is (= "one" (get-value combined 1)))
      (is (= "two" (get-value combined 2)))
      (is (= "three" (get-value combined 3)))
      (is (= "four" (get-value combined 4)))
      (is (= true (avl-balanced? combined))))))

(deftest test-filter-values
  (testing "Filtering AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 1)
                   (insert 2 2)
                   (insert 3 3)
                   (insert 4 4))
          filtered (filter-values (fn [_ v] (even? v)) tree)]
      (is (nil? (get-value filtered 1)))
      (is (= 2 (get-value filtered 2)))
      (is (nil? (get-value filtered 3)))
      (is (= 4 (get-value filtered 4)))
      (is (= true (avl-balanced? filtered))))))

(deftest test-monoid
  (testing "Monoid operations on AVL trees"
    (let [empty-tree (empty-avl)
          non-empty-tree (-> (empty-avl)
                             (insert 1 1)
                             (insert 2 2)
                             (insert 3 3)
                             (insert 4 4))]
      (is (= empty-tree (concat-avl empty-tree empty-tree)))
      (is (= non-empty-tree (concat-avl empty-tree non-empty-tree)))
      (is (= non-empty-tree (concat-avl non-empty-tree empty-tree)))
      (is (= non-empty-tree (concat-avl non-empty-tree non-empty-tree))))))


(run-tests)