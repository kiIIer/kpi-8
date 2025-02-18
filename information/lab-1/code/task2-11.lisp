;; (a) Створення хеш-таблиці з асоціативного списку
(defun alist->hash-table (alist &key (test 'eql))
  "Повертає нову хеш-таблицю, створену з асоціативного списку ALIST."
  (let ((ht (make-hash-table :test test)))
    (dolist (pair alist ht)
      ;; (car pair) - ключ, (cdr pair) - значення
      (setf (gethash (car pair) ht) (cdr pair)))))

;; (b) Створення асоціативного списку з хеш-таблиці
(defun hash-table->alist (ht)
  "Повертає асоціативний список, що відповідає хеш-таблиці HT."
  (let ((result nil))
    (maphash (lambda (key value)
               (push (cons key value) result))
             ht)
    (nreverse result)))

;; Приклад використання
(let* ((alist '((a . 1) (b . 2) (c . 3)))
       (ht (alist->hash-table alist))
       (alist-from-ht (hash-table->alist ht)))
  (format t "~%Асоціативний список: ~A" alist)
  (format t "~%Хеш-таблиця (через gethash): ")
  (format t "~A " (gethash 'a ht))  ;; => 1
  (format t "~A " (gethash 'b ht))  ;; => 2
  (format t "~A " (gethash 'c ht))  ;; => 3
  (format t "~%Асоціативний список з хеш-таблиці: ~A~%" alist-from-ht))
