;; Оголошення структури для вузла дерева з трьома нащадками
(defstruct (tree-node (:constructor make-node (data &key (left nil) (middle nil) (right nil))))
  data
  left
  middle
  right)

;; (a) Функція copy-tree-node
(defun copy-tree-node (node)
  "Повертає копію дерева node, створюючи нові вузли для кожного нащадка."
  (if (null node)
      nil
      (make-node (tree-node-data node)
                 :left   (copy-tree-node (tree-node-left   node))
                 :middle (copy-tree-node (tree-node-middle node))
                 :right  (copy-tree-node (tree-node-right  node)))))

;; (b) Функція find-in-tree
(defun find-in-tree (obj node)
  "Перевіряє, чи містить дерево node значення obj (z точки зору eq)."
  (cond
    ((null node) nil)
    ((eq (tree-node-data node) obj) t)
    (t (or (find-in-tree obj (tree-node-left   node))
           (find-in-tree obj (tree-node-middle node))
           (find-in-tree obj (tree-node-right  node))))))

;; Приклад використання
(let* ((root (make-node 'root))
       (child1 (make-node 'child1))
       (child2 (make-node 'child2))
       (child3 (make-node 'child3)))
  (setf (tree-node-left root)   child1
        (tree-node-middle root) child2
        (tree-node-right root)  child3)

  (format t "~%Оригінал: ~A" root)
  (format t "~%Знайти 'child2'? ~A" (find-in-tree 'child2 root))
  (let ((copied (copy-tree-node root)))
    (format t "~%Копія: ~A" copied)
    (format t "~%(eq оригіналу копії) ~A" (eq root copied))))
