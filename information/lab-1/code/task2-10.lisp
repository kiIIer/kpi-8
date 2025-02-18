;; Повторимо структуру для бінарного дерева пошуку:
(defstruct (bst-node 
             (:constructor make-bst-node (key &optional (left nil) (right nil))))
  key
  left
  right)

(defun bst-adjoin1 (tree obj)
  "Додає OBJ у BST-дерево, якщо його там ще немає."
  (cond
    ((null tree)
     ;; Якщо дерево порожнє, створюємо новий вузол
     (make-bst-node obj))
    ((= obj (bst-node-key tree))
     ;; Якщо ключ уже існує, нічого не змінюємо
     tree)
    ((< obj (bst-node-key tree))
     ;; Йдемо вліво, якщо OBJ менший за ключ вузла
     (setf (bst-node-left tree)
           (bst-adjoin1 (bst-node-left tree) obj))
     tree)
    (t
     ;; В іншому випадку - йдемо вправо
     (setf (bst-node-right tree)
           (bst-adjoin1 (bst-node-right tree) obj))
     tree)))

;; Приклад використання
(let* ((root (make-bst-node 5))
       (node3 (make-bst-node 3))
       (node7 (make-bst-node 7))
       (node2 (make-bst-node 2))
       (node4 (make-bst-node 4)))
  ;; Побудуємо дерево:
  ;;       5
  ;;      / \
  ;;     3   7
  ;;    / \
  ;;   2   4
  (setf (bst-node-left root) node3)
  (setf (bst-node-right root) node7)
  (setf (bst-node-left node3) node2)
  (setf (bst-node-right node3) node4)

  (format t "~%Початкове дерево: ~A" root)
  (format t "~%Спроба додати ключ 3:~%")
  (bst-adjoin1 root 3)  ;; Нічого не змінюється, бо 3 вже існує
  (format t "Дерево: ~A~%" root)

  (format t "~%Спроба додати ключ 6:~%")
  (bst-adjoin1 root 6)  ;; Вставляємо 6 між 5 і 7
  (format t "Дерево: ~A~%" root))
