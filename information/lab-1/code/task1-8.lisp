;; (a) Друкує кількість точок

;; Рекурсивний підхід
(defun print-dots-rec (n)
  (when (> n 0)
    (format t ".")
    (print-dots-rec (- n 1))))

;; Ітеративний підхід
(defun print-dots-iter (n)
  (dotimes (i n)
    (format t ".")))

;; (b) Підраховує кількість символів `a` у списку

;; Рекурсивний підхід
(defun count-a-rec (lst)
  (if (null lst)
      0
      (+ (if (eql (car lst) 'a) 1 0)
         (count-a-rec (cdr lst)))))

;; Ітеративний підхід
(defun count-a-iter (lst)
  (let ((count 0))
    (dolist (x lst count)
      (when (eql x 'a)
        (incf count)))))

;; Тестування:
(print-dots-rec 5)  ;; Результат: .....
(format t "~%")
(print-dots-iter 5)  ;; Результат: .....

(print (count-a-rec '(a b c a d a)))  ;; Результат: 3
(print (count-a-iter '(a b c a d a))) ;; Результат: 3
