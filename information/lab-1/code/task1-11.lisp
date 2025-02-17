(defun new-union (list1 list2)
  (let ((result nil))
    (dolist (x list1)
      (unless (member x result)
        (push x result)))
    (dolist (x list2)
      (unless (member x result)
        (push x result)))
    (nreverse result))) ;; Перевертаємо список, щоб зберегти правильний порядок

;; Тестування:
(print (new-union '(a b c) '(b a d)))  ;; Результат: (A B C D)
(print (new-union '(1 2 3) '(3 4 5)))  ;; Результат: (1 2 3 4 5)
(print (new-union '(x y) '(y z x)))    ;; Результат: (X Y Z)
(print (new-union '() '(a b c)))       ;; Результат: (A B C)
(print (new-union '(a b c) '()))       ;; Результат: (A B C)
