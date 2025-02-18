;; Припустимо, що існує структура для вузла BST:
(defstruct (bst-node 
             (:constructor make-bst-node (key &optional (left nil) (right nil))))
  key
  left
  right)

(defun bst-to-descending-list (tree)
  "Повертає список значень із BST-дерева TREE від більшого до меншого."
  (if (null tree)
      nil
      (append (bst-to-descending-list (bst-node-right tree))
              (list (bst-node-key tree))
              (bst-to-descending-list (bst-node-left tree)))))

;; Приклад використання
(let* ((node1 (make-bst-node 1))
       (node3 (make-bst-node 3))
       (node2 (make-bst-node 2 node1 node3))   ;; дерево:   2
       (node7 (make-bst-node 7))
       (node5 (make-bst-node 5 node2 node7)))  ;; дерево:       5
                                               ;;            2     7
                                               ;;          1   3
  (format t "~%BST: ~A" node5)
  (format t "~%Список спадно: ~A~%" (bst-to-descending-list node5)))
