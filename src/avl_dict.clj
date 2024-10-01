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
  (cond
    (nil? node) nil
    (< key (:key node)) (balance (assoc node :left (delete (:left node) key)))
    (> key (:key node)) (balance (assoc node :right (delete (:right node) key)))
    :else
    (cond
      (nil? (:left node)) (:right node)
      (nil? (:right node)) (:left node)
      :else
      (let [min-node (min-value-node (:right node))]
        (-> min-node
            (assoc :right (delete (:right node) (:key min-node)))
            (assoc :left (:left node)) ; Correct association
            (balance))))))

;; Get the value for a given key
(defn get-value [node key]
  (cond
    (nil? node) nil
    (< key (:key node)) (recur (:left node) key)
    (> key (:key node)) (recur (:right node) key)
    :else (:value node)))

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