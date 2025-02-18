(defun quarter-turn (matrix)
  "Повертає копію квадратного масиву MATRIX, повернутого на 90 градусів."
  (let* ((dims (array-dimensions matrix))
         (n (first dims))
         ;; Створюємо новий масив з тими ж розмірами
         (result (make-array dims)))
    ;; Якщо масив не квадратний, опрацьовуємо помилку чи повертаємо NIL
    (unless (= (first dims) (second dims))
      (error "Матриця не квадратна."))
    ;; Заповнюємо новий масив, міняючи місцями індекси
    (dotimes (i n)
      (dotimes (j n)
        ;; У повернутій матриці:
        ;; - рядок => j
        ;; - стовпчик => n - i - 1
        (setf (aref result j (- n i 1))
              (aref matrix i j))))
    result))

;; Приклад використання
(let ((mat #2A((a b)
               (c d))))
  (format t "~%Original: ~A~%" mat)
  (format t "Rotated: ~A~%" (quarter-turn mat)))
