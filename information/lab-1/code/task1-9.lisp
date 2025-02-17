(defun summit (lst)
  (if (null lst)
      0  ;; Якщо список порожній, повертаємо 0
      (+ (if (null (car lst)) 0 (car lst))  ;; Ігноруємо nil, додаємо інші елементи
         (summit (cdr lst)))))  ;; Рекурсивно додаємо інші елементи

;; Тестування:
(print (summit '(1 2 3 nil 4)))  ;; Результат: 10
(print (summit '(nil nil 5 5)))  ;; Результат: 10
(print (summit '()))             ;; Результат: 0
(print (summit '(nil nil nil)))  ;; Результат: 0
