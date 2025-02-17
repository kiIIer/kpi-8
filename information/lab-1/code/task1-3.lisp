(defun fourth-element (lst)
  (car (cdr (cdr (cdr lst)))))

;; Тестування:
(print (fourth-element '(a b c d e)))  ;; Результат: D
(print (fourth-element '(1 2 3 4 5 6)))  ;; Результат: 4