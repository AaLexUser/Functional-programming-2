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

(deftest test-insert-and-retrieve-date
  (testing "Inserting and retrieving date values"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) "2020-01-01")
                   (insert (java.util.Date. 120 5 15) "2020-06-15")
                   (insert (java.util.Date. 119 11 31) "2019-12-31")
                   (insert (java.util.Date. 121 6 4) "2021-07-04"))]
      (is (= "2020-01-01" (get-value tree (java.util.Date. 120 0 1))))
      (is (= "2020-06-15" (get-value tree (java.util.Date. 120 5 15))))
      (is (= "2019-12-31" (get-value tree (java.util.Date. 119 11 31))))
      (is (= "2021-07-04" (get-value tree (java.util.Date. 121 6 4))))
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

(deftest test-delete-date
  (testing "Deleting date nodes"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) "2020-01-01")
                   (insert (java.util.Date. 120 5 15) "2020-06-15")
                   (insert (java.util.Date. 119 11 31) "2019-12-31")
                   (delete (java.util.Date. 120 0 1)))]
      (is (nil? (get-value tree (java.util.Date. 120 0 1))))
      (is (= "2020-06-15" (get-value tree (java.util.Date. 120 5 15))))
      (is (= "2019-12-31" (get-value tree (java.util.Date. 119 11 31))))
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

(deftest test-to-list-date
  (testing "Converting tree with Date keys and values to sorted list"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 119 11 31) (java.util.Date. 119 11 31))
                   (insert (java.util.Date. 120 0 1) (java.util.Date. 120 0 1))
                   (insert (java.util.Date. 120 5 15) (java.util.Date. 120 5 15))
                   (insert (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)))
          expected [(java.util.Date. 119 11 31) (java.util.Date. 119 11 31)
                    (java.util.Date. 120 0 1) (java.util.Date. 120 0 1)
                    (java.util.Date. 120 5 15) (java.util.Date. 120 5 15)
                    (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)]
          result (to-list tree)]
      (is (= (count expected) (count result)) "List should have correct number of elements")
      (is (= expected result) "List should be sorted correctly")
      (is (every? #(instance? java.util.Date %) result) "All elements should be Date instances")
      (is (apply <= (map #(.getTime %) (take-nth 2 result))) "Keys should be in ascending order"))))

(deftest test-keys-avl
  (testing "keys-avl function"
    (let [avl (to-tree [[1 "a"] [2 "b"] [3 "c"]])]
      (is (= (keys-avl avl) '(1 2 3)) "Should return all keys in the AVL tree in sorted order"))
    (let [empty-avl (empty-avl)]
      (is (= (keys-avl empty-avl) '()) "Should return an empty list for an empty AVL tree"))))

(deftest test-keys-avl-date
  (testing "keys-avl function"
    (let [avl (to-tree [[(java.util.Date. 120 0 1) "2020-01-01"]
                        [(java.util.Date. 120 5 15) "2020-06-15"]
                        [(java.util.Date. 119 11 31) "2019-12-31"]
                        [(java.util.Date. 121 6 4) "2021-07-04"]])]
      (is (= (keys-avl avl)
             [(java.util.Date. 119 11 31)
              (java.util.Date. 120 0 1)
              (java.util.Date. 120 5 15)
              (java.util.Date. 121 6 4)])))))

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

(deftest test-map-avl-date
  (testing "Mapping over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) 1)
                   (insert (java.util.Date. 120 5 15) 2)
                   (insert (java.util.Date. 119 11 31) 3)
                   (insert (java.util.Date. 121 6 4) 4))
          mapped-tree (map-avl (fn [_ v] (* v 2)) tree)]
      (is (= 2 (get-value mapped-tree (java.util.Date. 120 0 1))))
      (is (= 4 (get-value mapped-tree (java.util.Date. 120 5 15))))
      (is (= 6 (get-value mapped-tree (java.util.Date. 119 11 31))))
      (is (= 8 (get-value mapped-tree (java.util.Date. 121 6 4))))
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

(deftest test-fold-right-date
  (testing "Folding right over AVL tree with dates"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) (java.util.Date. 120 0 1))
                   (insert (java.util.Date. 120 5 15) (java.util.Date. 120 5 15))
                   (insert (java.util.Date. 119 11 31) (java.util.Date. 119 11 31))
                   (insert (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)))
          latest-date (fold-right (fn [acc _ v] (if (.after v acc) v acc))
                                  (java.util.Date. 0 0 1)
                                  tree)]
      (is (= (java.util.Date. 121 6 4) latest-date))
      (is (true? (avl-balanced? tree))))))

(deftest test-fold-left-sum
  (testing "Folding left over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert 1 1)
                   (insert 2 2)
                   (insert 3 3))
          sum (fold-left (fn [acc _ v] (+ acc v)) 0 tree)]
      (is (= 6 sum))
      (is (= true (avl-balanced? tree))))))

(deftest test-fold-left-date
  (testing "Folding left over AVL tree"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) (java.util.Date. 120 0 1))
                   (insert (java.util.Date. 120 5 15) (java.util.Date. 120 5 15))
                   (insert (java.util.Date. 119 11 31) (java.util.Date. 119 11 31))
                   (insert (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)))
          max-date (fold-left (fn [acc _ v] (if (.after v acc) v acc)) (java.util.Date. 119 11 31) tree)]
      (is (= 0 (.compareTo max-date (java.util.Date. 121 6 4))))
      (is (true? (avl-balanced? tree))))))

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

(deftest test-concat-avl-date
  (testing "Concatenating two AVL trees with Date keys"
    (let [tree1 (-> (empty-avl)
                    (insert (java.util.Date. 120 0 1) "2020-01-01")
                    (insert (java.util.Date. 120 5 15) "2020-06-15"))
          tree2 (-> (empty-avl)
                    (insert (java.util.Date. 119 11 31) "2019-12-31")
                    (insert (java.util.Date. 121 6 4) "2021-07-04"))
          combined (concat-avl tree1 tree2)]
      (is (= "2020-01-01" (get-value combined (java.util.Date. 120 0 1))))
      (is (= "2020-06-15" (get-value combined (java.util.Date. 120 5 15))))
      (is (= "2019-12-31" (get-value combined (java.util.Date. 119 11 31))))
      (is (= "2021-07-04" (get-value combined (java.util.Date. 121 6 4))))
      (is (avl-balanced? combined))
      (is (= 4 (count (keys-avl combined))))
      (is (= [(java.util.Date. 119 11 31)
              (java.util.Date. 120 0 1)
              (java.util.Date. 120 5 15)
              (java.util.Date. 121 6 4)]
             (keys-avl combined))))))

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

(deftest test-filter-values-date
  (testing "Filtering AVL tree with Date keys"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) "2020-01-01")
                   (insert (java.util.Date. 120 5 15) "2020-06-15")
                   (insert (java.util.Date. 119 11 31) "2019-12-31")
                   (insert (java.util.Date. 121 6 4) "2021-07-04"))
          filtered (filter-values (fn [_ v] (.startsWith v "2020")) tree)]
      (is (= "2020-01-01" (get-value filtered (java.util.Date. 120 0 1))))
      (is (= "2020-06-15" (get-value filtered (java.util.Date. 120 5 15))))
      (is (nil? (get-value filtered (java.util.Date. 119 11 31))))
      (is (nil? (get-value filtered (java.util.Date. 121 6 4))))
      (is (avl-balanced? filtered))
      (is (= 2 (count (keys-avl filtered))))
      (is (= [(java.util.Date. 120 0 1) (java.util.Date. 120 5 15)]
             (keys-avl filtered))))))

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

(deftest test-monoid-date
  (testing "Monoid operations on AVL trees with Date keys"
    (let [empty-tree (empty-avl)
          non-empty-tree (-> (empty-avl)
                             (insert (java.util.Date. 120 0 1) (java.util.Date. 120 0 1))
                             (insert (java.util.Date. 120 5 15) (java.util.Date. 120 5 15))
                             (insert (java.util.Date. 119 11 31) (java.util.Date. 119 11 31))
                             (insert (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)))]
      (is (= empty-tree (concat-avl empty-tree empty-tree)))
      (is (= non-empty-tree (concat-avl empty-tree non-empty-tree)))
      (is (= non-empty-tree (concat-avl non-empty-tree empty-tree)))
      (is (= non-empty-tree (concat-avl non-empty-tree non-empty-tree)))
      (is (avl-balanced? non-empty-tree))
      (is (= 4 (count (keys-avl non-empty-tree))))
      (is (= [(java.util.Date. 119 11 31)
              (java.util.Date. 120 0 1)
              (java.util.Date. 120 5 15)
              (java.util.Date. 121 6 4)]
             (keys-avl non-empty-tree))))))

(deftest test-polymorphic-insert-keys
  (testing "AVL tree with polymorphic keys"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 119 11 31) (java.util.Date. 119 11 31))
                   (insert (java.util.Date. 120 0 1) (java.util.Date. 120 0 1))
                   (insert (java.util.Date. 120 5 15) (java.util.Date. 120 5 15))
                   (insert (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)))]
      (is (= (java.util.Date. 119 11 31) (get-value tree (java.util.Date. 119 11 31))))
      (is (= (java.util.Date. 120 0 1) (get-value tree (java.util.Date. 120 0 1))))
      (is (= (java.util.Date. 120 5 15) (get-value tree (java.util.Date. 120 5 15))))
      (is (= (java.util.Date. 121 6 4) (get-value tree (java.util.Date. 121 6 4))))
      (is (avl-balanced? tree))
      (is (= 4 (count (keys-avl tree))))
      (is (= [(java.util.Date. 119 11 31)
              (java.util.Date. 120 0 1)
              (java.util.Date. 120 5 15)
              (java.util.Date. 121 6 4)]
             (keys-avl tree)))
      (is (every? #(instance? java.util.Date %) (keys-avl tree)))
      (is (every? #(instance? java.util.Date %) (map #(get-value tree %) (keys-avl tree))))
      (is (= (keys-avl tree) (map #(get-value tree %) (keys-avl tree)))))))

(deftest test-polymorphic-delete-keys
  (testing "AVL tree with polymorphic keys"
    (let [tree (-> (empty-avl)
                   (insert (java.util.Date. 120 0 1) (java.util.Date. 120 0 1))
                   (insert (java.util.Date. 120 5 15) (java.util.Date. 120 5 15))
                   (insert (java.util.Date. 119 11 31) (java.util.Date. 119 11 31))
                   (insert (java.util.Date. 121 6 4) (java.util.Date. 121 6 4)))
          data-to-delete (java.util.Date. 120 0 1)
          tree-after-delete (delete tree data-to-delete)]
      (is (nil? (get-value tree-after-delete data-to-delete)))
      (is (avl-balanced? tree-after-delete))
      (is (= 3 (count (keys-avl tree-after-delete))))
      (is (= [(java.util.Date. 119 11 31)
              (java.util.Date. 120 5 15)
              (java.util.Date. 121 6 4)]
             (keys-avl tree-after-delete)))
      (is (every? #(instance? java.util.Date %) (keys-avl tree-after-delete))))))

(run-tests)