(defun copy-list-reduce (lst)
  "Створює копію списку lst, використовуючи reduce."
  (reduce (lambda (acc x)
            (append acc (list x)))
          lst
          :initial-value '()))


(defun reverse-reduce (lst)
  "Повертає список lst у зворотному порядку, використовуючи reduce."
  (reduce (lambda (acc x)
            (cons x acc))
          lst
          :initial-value '()))

(print (copy-list-reduce '(1 2 3)))
;; => (1 2 3)  ; новий список, відмінний від вихідного

(print (reverse-reduce '(1 2 3)))
;; => (3 2 1)
