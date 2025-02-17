(defun enigma (x)
  (and (not (null x)) 
       (or (null (car x)) 
           (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y) 
      nil
      (if (eql (car y) x) 
          0
          (let ((z (mystery x (cdr y)))) 
            (and z (+ z 1))))))
  
;; Тестування:
(print (enigma '(1 2 nil 4)))  ;; Результат: T (є nil у списку)
(print (enigma '(1 2 3 4)))    ;; Результат: NIL (немає nil)

(print (mystery 2 '(1 2 3 2 4 2)))  ;; Результат: 1 (перша поява 2 на індексі 1)
(print (mystery 5 '(1 2 3 4)))      ;; Результат: NIL (елемента немає)
(print (mystery 3 '(1 2 3 4 3 5)))  ;; Результат: 2 (перша поява 3 на індексі 2)
