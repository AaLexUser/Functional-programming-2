# Лабораторная работа #2

## Дисциплина

Функциональное программирование

## Выполнил

Лапин Алексей Александрович, P34102

## Цель работы

Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

## Требования

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

## Вариант

| AVL-dict |
|----------|

## Ход работы

### Структура проекта

```shell
├── README.md
├── src/
│   └── avl_dict.clj
├── test/
│       ├── units_test.clj
│       └── property_test.clj
│
├── .github/workflows/
│   └── clojure.yml
│
├── .gitingore 
├── test.edn
└── deps.edn


```

### Структура данных

```clojure
(defrecord AVLNode [key value left right height])
```

### AVL-дерево

```clojure
(ns avl-dict
  (:require [clojure.pprint :refer [pprint]]))

;; AVL Node structure
(defrecord AVLNode [key value left right height])

;; Get the height of a node
(defn height [node]
  (if node (:height node) 0))

;; Update the height of a node
(defn update-height [node]
  (let [hl (height (:left node))
        hr (height (:right node))
        new-height (inc (max hl hr))]
    (assoc node :height new-height)))

;; Calculate the balance factor of a node
(defn balance-factor [node]
  (let [hl (height (:left node))
        hr (height (:right node))]
    (- hl hr)))

;; Perform a right rotation
(defn rotate-right [node]
  (let [l (:left node)
        new-node (assoc node :left (:right l))
        new-l (assoc l :right new-node)]
    (-> new-l
        (update :right update-height)
        (update-height))))

;; Perform a left rotation
(defn rotate-left [node]
  (let [r (:right node)
        new-node (assoc node :right (:left r))
        new-r (assoc r :left new-node)]
    (-> new-r
        (update :left update-height)
        (update-height))))

;; Balance the AVL tree 
(defn balance [node]
  (let [bf (balance-factor node)]
    (cond
      (> bf 1)
      (if (>= (balance-factor (:left node)) 0)
        (rotate-right node)
        (-> node
            (assoc :left (rotate-left (:left node)))
            rotate-right))

      (< bf -1)
      (if (<= (balance-factor (:right node)) 0)
        (rotate-left node)
        (-> node
            (assoc :right (rotate-right (:right node)))
            rotate-left))

      :else (update-height node))))

;; Insert a key-value pair into the AVL tree
(defn insert [node key value]
  (if
   (nil? node) (->AVLNode key value nil nil 1)
   (let [cmp (compare key (:key node))]
     (cond
       (zero? cmp) (assoc node :value value)
       (neg? cmp) (balance (assoc node :left (insert (:left node) key value)))
       :else (balance (assoc node :right (insert (:right node) key value)))))))

;; Find the node with the minimum value
(defn min-value-node [node]
  (if (:left node)
    (recur (:left node))
    node))

;; Delete a key from the AVL tree
(defn delete [node key]
  (let [cmp (compare key (:key node))]
  (cond
    (nil? node) nil
    (neg? cmp) (balance (assoc node :left (delete (:left node) key)))
    (pos? cmp) (balance (assoc node :right (delete (:right node) key)))
    :else
    (cond
      (nil? (:left node)) (:right node)
      (nil? (:right node)) (:left node)
      :else
      (let [min-node (min-value-node (:right node))]
        (-> min-node
            (assoc :right (delete (:right node) (:key min-node)))
            (assoc :left (:left node)) ; Correct association
            (balance)))))))

;; Get the value for a given key
(defn get-value [node key]
  (let [cmp (compare key (:key node))]
  (cond
    (nil? node) nil
    (neg? cmp) (recur (:left node) key)
    (pos? cmp) (recur (:right node) key)
    :else (:value node))))

;; Check if the AVL tree contains a key
(defn avl-contains? [node key]
  (not (nil? (get-value node key))))

;; Convert the AVL tree to a list
(defn to-list [node]
  (if (nil? node) '()
      (concat (to-list (:left node))
              [(:key node) (:value node)]
              (to-list (:right node)))))

;; Apply a function to each node in the AVL tree
(defn map-avl [f node]
  (when node
    (let [l (map-avl f (:left node))
          r (map-avl f (:right node))
          new-value (f (:key node) (:value node))]
      (assoc node :value new-value :left l :right r))))

;; Fold the AVL tree from the left
(defn fold-left [f acc node]
  (if (nil? node)
    acc
    (let [acc (fold-left f acc (:left node))
          acc (f acc (:key node) (:value node))
          acc (fold-left f acc (:right node))]
      acc)))

;; Fold the AVL tree from the right
(defn fold-right [f acc node]
  (if node
    (let [acc (fold-right f acc (:right node))
          acc (f acc (:key node) (:value node))
          acc (fold-right f acc (:left node))]
      acc)
    acc))

;; Concatenate two AVL trees
(defn concat-avl [node1 node2]
  (cond
    (nil? node1) node2
    (nil? node2) node1
    :else (fold-left insert node2 node1)))

;; Get the keys of the AVL tree
(defn keys-avl [avl]
  (if (nil? avl) '()
      (map first (partition 2 (to-list avl)))))

;; Filter the values in the AVL tree
(defn filter-values [pred node]
  (when node
    (let [l (filter-values pred (:left node))
          r (filter-values pred (:right node))]
      (if (pred (:key node) (:value node))
        (balance (assoc node :left l :right r))
        (concat-avl l r)))))

;; Create an empty AVL tree
(defn empty-avl []
  nil)

;; Visualize the AVL tree
(defn visualize [node]
  (letfn [(build-tree [node prefix is-left]
            (when node
              (str
               (build-tree (:right node) (str prefix (if is-left "│   " "    ")) false)
               prefix
               (if is-left "└── " "┌── ")
               (str (:key node) ":" (:value node) " (h:" (:height node) ")")
               "\n"
               (build-tree (:left node) (str prefix (if is-left "    " "│   ")) true))))]
    (println (build-tree node "" false))))

;; Convert a sequence to an AVL tree
(defn to-tree [seq]
  (reduce (fn [tree [k v]] (insert tree k v)) nil seq))

;; Generate a sequence of random key-value pairs
(defn gen-seq [n max-val]
  (let [keys (repeatedly n #(rand-int max-val))
        vals (repeatedly n #(rand-int max-val))]
    (zipmap keys vals)))

;; Convert an AVL tree to a map
(defn avl->map [node]
  (when node
    {:key (.key node)
     :value (.value node)
     :left (avl->map (.left node))
     :right (avl->map (.right node))
     :height (.height node)}))

;; Pretty print the AVL tree
(defn pprint-avl [avl]
  (pprint (avl->map avl)))

;; Check if two AVL trees are equal
(defn avl-equal? [avl1 avl2]
  (= (to-list avl1) (to-list avl2)))

;; Check if the AVL tree is balanced
(defn avl-balanced? [avl]
  (if (nil? avl) true
      (let [lh (height (:left avl))
            rh (height (:right avl))]
        (and (<= (Math/abs (- lh rh)) 1)
             (avl-balanced? (:left avl))
             (avl-balanced? (:right avl))))))

(comment
  (def gg (empty-avl))
  (def gg (insert gg "12" "1"))
  (def gg (insert gg "2" "2"))
  (def gg (insert gg 3 3))
  (def gg (insert gg 4 4))
  (def gg (insert gg 5 5))
  ;; visualize 10 elements with timeout 10 seconds 
  (def gg (empty-avl))
  (doall (for [i (range 1 10)]
           (do
             (def gg (insert gg i i))
             (visualize gg))))
  (visualize gg)
  (def seq1 (gen-seq 10 30))
  (def xx (to-tree seq1))
  (visualize xx)
  (println (to-list xx))
  (pprint-avl gg)
  (pprint gg)

  (let [tree (-> (empty-avl)
                 (insert 1 1)
                 (insert 2 2)
                 (insert 3 3)
                 (insert 4 4))
        filtered (filter-values (fn [_ v] (even? v)) tree)]
    (visualize tree)
    (visualize filtered)
    (println (to-list filtered))
    (pprint filtered)
    (println (keys-avl filtered)))

  (let [tree (-> (empty-avl)
                 (insert 5 "five")
                 (insert 3 "three")
                 (insert 7 "seven"))]
    (println (get-value tree 5))
    (println (get-value tree 3))
    (println (get-value tree 7))
    (println (get-value tree 1))
    (println (avl-balanced? tree))))
```

### Тестирование

#### Unit testing

```clojure
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
```

#### Property-based testing

```clojure
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

(def gen-poly-pair (gen/one-of [
             (gen/vector (gen/not-empty gen-pair) samples_count)
             (gen/vector (gen/not-empty gen-int-pair) samples_count)
             (gen/vector (gen/not-empty gen-date-pair) samples_count)
]))

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
                 value gen-value
                 ]
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
```

### Запуск тестов

```shell
clojure -M:test
```

Вывод:



```shell
Testing property-test
{:result true, :num-tests 1000, :seed 1727796752228, :time-elapsed-ms 1182, :test-var "monoid-first-law-test"}
{:result true, :num-tests 1000, :seed 1727796753412, :time-elapsed-ms 921, :test-var "delete-and-retrieve-test"}
{:result true, :num-tests 1000, :seed 1727796754333, :time-elapsed-ms 954, :test-var "contains-all-elements-test"}
{:result true, :num-tests 1000, :seed 1727796755288, :time-elapsed-ms 1757, :test-var "fold-find-sum"}
{:result true, :num-tests 1000, :seed 1727796757045, :time-elapsed-ms 907, :test-var "contains-random-element-test"}
{:result true, :num-tests 1000, :seed 1727796757953, :time-elapsed-ms 940, :test-var "insert-and-retrieve-test"}
{:result true, :num-tests 1000, :seed 1727796758894, :time-elapsed-ms 3005, :test-var "monoid-second-law-test"}
{:result true, :num-tests 1000, :seed 1727796761900, :time-elapsed-ms 76, :test-var "filter-values-test"}
{:result true, :num-tests 1000, :seed 1727796761976, :time-elapsed-ms 1835, :test-var "concat-2-avls-all-elements-exist"}

Ran 9 tests containing 21009 assertions.
0 failures, 0 errors.

Testing units-test

Ran 14 tests containing 44 assertions.
0 failures, 0 errors.
[(............................................)(......................................................................................)]
23 tests, 21053 assertions, 0 failures.
```

## Вывод

Выполняя данную лабораторную работу, я освоил несколько важных приемов функционального программирования и работы с пользовательскими типами данных:

1. Реализация АВЛ-дерева: Я создал полиморфную структуру данных АВЛ-дерево, которая обеспечивает эффективное хранение и поиск ключей. Это позволило мне глубже понять принципы работы сбалансированных деревьев и их реализацию в функциональном стиле.

2. Неизменяемость данных: Все операции с АВЛ-деревом реализованы с соблюдением принципа неизменяемости, что является ключевым аспектом функционального программирования. Это обеспечивает безопасность при параллельном выполнении и упрощает отладку.

3. Функции высшего порядка: Использование функций `map-avl`, `fold-left`, `fold-right` и `filter-values` демонстрирует мощь функций высшего порядка в обработке данных.

4. Моноидальная структура: Реализация АВЛ-дерева как моноида с операцией `concat-avl` и нейтральным элементом `empty-avl` показывает, как абстрактные алгебраические концепции могут быть применены к структурам данных.

5. Тестирование:

   - Unit-тестирование: Я написал набор модульных тестов для проверки корректности работы основных операций АВЛ-дерева.

   - Property-based тестирование: Использование библиотеки test.check позволило мне создать более общие и мощные тесты, проверяющие свойства АВЛ-дерева на большом количестве случайных входных данных.

6. Полиморфизм: АВЛ-дерево реализовано как полиморфная структура, способная хранить значения различных типов.

7. Визуализация: Реализация функции `visualize` помогла в отладке и понимании структуры дерева.

Эта лабораторная работа позволила мне глубже погрузиться в функциональное программирование, работу со сложными структурами данных и различные методы тестирования. 
