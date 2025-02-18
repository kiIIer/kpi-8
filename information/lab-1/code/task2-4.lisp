(defun showdots (lst)
  (cond
    ;; Якщо порожній список
    ((null lst)
     (format t "NIL"))
    ;; Якщо атом (не cons‐пара)
    ((atom lst)
     (format t "~A" lst))
    ;; Інакше це cons‐пара
    (t
     (format t "(")                ; Відкриваємо дужку
     (showdots (car lst))         ; Виводимо car-частину
     (format t " . ")             ; Виводимо " . "
     (showdots (cdr lst))         ; Виводимо cdr-частину
     (format t ")"))))            ; Закриваємо дужку

;; Приклади викликів:
(showdots '(a b c))   ;; (A . (B . (C . NIL)))
(format t "~%")
(showdots '())        ;; NIL
(format t "~%")
(showdots '(1 (2 3) 4))  ;; (1 . ((2 . (3 . NIL)) . (4 . NIL)))
(format t "~%")
(showdots '(x . y))   ;; (X . Y)
(format t "~%")
(showdots '(x (y . z))) ;; (X . ((Y . Z) . NIL))
(format t "~%")
