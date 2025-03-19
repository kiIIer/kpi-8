;;; =============================================================================
;;;  Фреймова "мова представлення знань" (виправлена версія).
;;; =============================================================================

(defvar *FRAMES* nil
  "Глобальний список (або хеш-таблиця) для зберігання створених фреймів.")

(defun slots->internal (slots)
  "Перетворює список слотів у внутрішню структуру.
Кожен елемент має вигляд: (SLOT-NAME (ASPECT ...) (ASPECT ...))."
  (mapcar #'identity slots))

(defun merge-slots (old-slots new-slots)
  "Об'єднує старі слоти з новими, оновлюючи чи додаючи слоти."
  (let ((result old-slots))
    (dolist (slot new-slots)
      (let ((existing (assoc (car slot) result)))
        (if existing
            (setf (cdr existing) (cdr slot)) ;; Оновити слот
          (push slot result))))
    result))

;;; ---------------------------
;;; Створення / оновлення фреймів
;;; ---------------------------
(defmacro deframeq (frame-name &rest slots)
  "Оголошує (або оновлює) фрейм FRAME-NAME, додаючи SLOT definitions (as data)."
  `(deframeq-internal ',frame-name ',slots))

(defun deframeq-internal (frame-name slots)
  "Реалізує логіку створення/оновлення фрейму."
  (let ((old-frame (assoc frame-name *FRAMES*)))
    (if old-frame
        (setf (cdr old-frame)
              (merge-slots (cdr old-frame) (slots->internal slots)))
      (push (cons frame-name (slots->internal slots)) *FRAMES*)))
  frame-name)

(defun fassertq (frame-name &rest slots-forms)
  "Додає або змінює слот(и) у фреймі FRAME-NAME.
Кожен елемент – список слотів типу '((SLOTNAME (ASPECT (DATA...)))...)"
  (dolist (slotlist slots-forms)
    (deframeq-internal frame-name slotlist)))


;;; ---------------------------
;;; Отримання даних зі слота фрейма
;;; ---------------------------
(defun fget (frame-name slot-name &optional (aspect '$value))
  "Повертає список значень з аспекта ASPECT у слоті SLOT-NAME фрейма FRAME-NAME.
Наприклад, якщо (COPIES ($VALUE 5)), (fget 'FRAME 'COPIES) => (5)."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if (null frame)
        nil
      (let ((slot (assoc slot-name (cdr frame))))
        (if (null slot)
            nil
          (let ((asp (assoc aspect (cdr slot))))
            (if asp
                (cdr asp)  ;; Повертає (DATA1 DATA2 ...)
              nil)))))))

;;; ---------------------------
;;; Демони ($IF-ADDED), що викликаються при додаванні значень
;;; ---------------------------
(defun maybe-run-if-added (frame-name slot-name aspect value)
  "Якщо в слоті є ( $IF-ADDED (#'<lambda> ...)), викликати ці процедури."
  (let ((demon (fget frame-name slot-name '$if-added)))
    (when demon
      (dolist (proc demon)
        (funcall proc frame-name slot-name aspect value)))))

(defun fput (frame-name slot-name aspect value)
  "Додає VALUE до аспекту ASPECT слота SLOT-NAME у фреймі FRAME-NAME.
Головна зміна: тепер зберігаємо (,aspect ,value), а не (,aspect (,value)).
Це уникає подвійних дужок."
  (fassertq frame-name
    `((,slot-name
       (,aspect ,value))))
  (maybe-run-if-added frame-name slot-name aspect value)
  value)

;;; =========================
;;; Функції для бібліотечної системи
;;; =========================

(defun borrow-book (reader book)
  "Видає книгу читачеві, зменшуючи кількість копій.
(fget book 'COPIES) => (5). (car ...) => 5. Потім зберігаємо (4) (одна пара дужок)."
  (let* ((copies-raw (fget book 'COPIES))  ;; => (5)
         (copies (car copies-raw)))        ;; => 5
    (if (and copies (> copies 0))
        (progn
          (fput book 'COPIES '$value (1- copies))  ;; тепер (4) замість ((4))
          (fput reader 'BORROWED '$value book)     ;; (LIB_BOOK) замість ((LIB_BOOK))
          (format t "~%Книга ~A видана читачеві ~A~%" book reader))
      (format t "~%Немає вільних примірників книги ~A~%" book))))

(defun return-book (reader book)
  "Читач повертає книгу.
Якщо (fget book 'COPIES) => (4), беремо (car ...) => 4, зберігаємо (5)."
  (let* ((borrowed (fget reader 'BORROWED))   ;; => (LIB_BOOK)
         (copies-raw (fget book 'COPIES))     ;; => (4)
         (copies (car copies-raw)))           ;; => 4
    (if (member book borrowed)
        (progn
          (fput book 'COPIES '$value (1+ copies))   ;; => (5)
          (fput reader 'BORROWED '$value (remove book borrowed))
          (format t "~%Книга ~A повернута читачем ~A~%" book reader))
      (format t "~%Читач ~A не має книги ~A~%" reader book))))
