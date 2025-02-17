;; (a) Реалізація через рекурсію
(defun pos+-rec (lst &optional (index 0))
  (if (null lst)
      nil
      (cons (+ (car lst) index)
            (pos+-rec (cdr lst) (+ index 1)))))

;; (b) Реалізація через ітерацію
(defun pos+-iter (lst)
  (let ((result nil)
        (index 0))
    (dolist (el lst (nreverse result)) ;; Реверсуємо, оскільки `push` додає на початок
      (push (+ el index) result)
      (incf index))))

;; (c) Реалізація через `mapcar`
(defun pos+-mapcar (lst)
  (let ((index -1))
    (mapcar (lambda (el) 
              (incf index) 
              (+ el index))
            lst)))

;; Тестування:
(print (pos+-rec '(7 5 1 4)))   ;; Результат: (7 6 3 7)
(print (pos+-iter '(7 5 1 4)))  ;; Результат: (7 6 3 7)
(print (pos+-mapcar '(7 5 1 4))) ;; Результат: (7 6 3 7)
