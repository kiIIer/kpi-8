(defun occurrences (lst)
  (let ((table nil))
    ;; Підрахунок входжень
    (dolist (el lst)
      (let ((entry (assoc el table :test #'eq)))
        (if entry
            (incf (cdr entry)) ;; Збільшуємо лічильник
            (push (cons el 1) table)))) ;; Додаємо новий запис

    ;; Сортування за спаданням кількості входжень
    (sort table #'> :key #'cdr)))

;; Тестування:
(print (occurrences '(a b a d a c d c a)))  ;; Результат: ((A . 4) (C . 2) (D . 2) (B . 1))
(print (occurrences '(x y x x z y y y)))    ;; Результат: ((Y . 4) (X . 3) (Z . 1))
(print (occurrences '(1 2 2 3 3 3 1 1)))    ;; Результат: ((1 . 3) (3 . 3) (2 . 2))
(print (occurrences '()))                   ;; Результат: NIL
