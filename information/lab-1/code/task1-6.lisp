;; Варіант (a)
(print (car (car (cdr '(a (b c) d)))))  ;; Результат: B

;; Варіант (b)
(print ((lambda (a b) a) 13 (ignore-errors (/ 1 0))))  ;; Результат: 13

;; Варіант (c)
(print (apply #'list* '(1 nil)))  ;; Результат: (1)
