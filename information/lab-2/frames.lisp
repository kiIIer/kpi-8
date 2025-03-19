;;; ==============================
;;; Фреймова "мова представлення знань"
;;; ==============================

(defvar *FRAMES* nil
  "Глобальний список (або хеш-таблиця) для зберігання створених фреймів.")

;; ---- Допоміжні функції ----

(defun slots->internal (slots)
  "Перетворює список слотів у внутрішню структуру.
   Кожен слот має вигляд: (SLOT-NAME (ASPECT ... ) (ASPECT ...) ...)."
  ;; 'slots' is expected to be a list of slot-forms, e.g. ((TITLE ($VALUE ...)) (AUTHOR ($VALUE ...)) ...)
  (mapcar #'identity slots))

(defun merge-slots (old-slots new-slots)
  "Об'єднує старі слоти з новими, оновлюючи чи додаючи слоти."
  (let ((result old-slots))
    (dolist (slot new-slots)
      (let ((existing (assoc (car slot) result)))
        (if existing
            (setf (cdr existing) (cdr slot)) ;; Заміна на новий вміст
          (push slot result))))
    result))

;; ---- Створення/оновлення фреймів ----

(defmacro deframeq (frame-name &rest slots)
  "Оголошує або оновлює фрейм FRAME-NAME, додаючи SLOT definitions (as data)."
  `(deframeq-internal ',frame-name ',slots))

(defun deframeq-internal (frame-name slots)
  "Реалізує логіку створення/оновлення фрейму за допомогою 'slots->internal' і 'merge-slots'."
  (let ((old-frame (assoc frame-name *FRAMES*)))
    (if old-frame
        (setf (cdr old-frame)
              (merge-slots (cdr old-frame) (slots->internal slots)))
      (push (cons frame-name (slots->internal slots)) *FRAMES*)))
  frame-name)

(defun fassertq (frame-name &rest slots-forms)
  "Додає або змінює слот(и) у фреймі FRAME-NAME.
SLOTS-FORMS - це список, де кожен елемент - список слотів."
  ;; Each element in slots-forms is presumably a quoted list of slot definitions.
  (dolist (slotlist slots-forms)
    (deframeq-internal frame-name slotlist)))


;; ---- Отримання даних зі слота фрейма ----

(defun fget (frame-name slot-name &optional (aspect '$value))
  "Повертає список значень з аспекта ASPECT у слоті SLOT-NAME фрейма FRAME-NAME."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if (null frame)
        ;; Фрейм не знайдено
        nil
      (let ((slot (assoc slot-name (cdr frame))))
        (if (null slot)
            nil
          (let ((asp (assoc aspect (cdr slot))))
            (if asp
                (cdr asp)
              nil)))))))

;; ---- Демони, які викликаються при додаванні значень ----

(defun maybe-run-if-added (frame-name slot-name aspect value)
  "Якщо в даному слоті є аспект $IF-ADDED, викликаємо всі процедури звідти."
  (let ((demon (fget frame-name slot-name '$if-added)))
    (when demon
      (dolist (proc demon)
        (funcall proc frame-name slot-name aspect value)))))

(defun fput (frame-name slot-name aspect value)
  "Додає VALUE до аспекту ASPECT слота SLOT-NAME у фреймі FRAME-NAME.
Якщо існує демон ($IF-ADDED), він викликається."
  (fassertq frame-name
    `((,slot-name
       (,aspect (,value)))))
  (maybe-run-if-added frame-name slot-name aspect value)
  value)


;;; =========================
;;; ПРИКЛАД ВИКОРИСТАННЯ
;;; =========================

;; Очищаємо глобальний список фреймів
(setq *FRAMES* nil)

;; 1) Створимо фрейм 'LIB_BOOK' з даними про книгу
(deframeq LIB_BOOK
  (TITLE ($VALUE ("CLtL2 - Common Lisp the Language, 2nd Edition")))
  (AUTHOR ($VALUE ("Guy L. Steele")))
  (COPIES ($VALUE 5)))

;; 2) Додамо слот з роком (Зверніть увагу на подвійні дужки, бо це один слот.)
(fassertq 'LIB_BOOK
  '((YEAR ($VALUE 1990))))

;; 3) Отримаємо дані
(format t "~%--- Дані про фрейм LIB_BOOK ---~%")
(format t "Title: ~A~%"  (fget 'LIB_BOOK 'TITLE))
(format t "Author: ~A~%" (fget 'LIB_BOOK 'AUTHOR))
(format t "Copies: ~A~%" (fget 'LIB_BOOK 'COPIES))
(format t "Year: ~A~%"   (fget 'LIB_BOOK 'YEAR))

;; 4) Демонстрація "демона":
;;   Додамо в слот 'STATUS' аспект $IF-ADDED з лямбда-процедурою
;;   Зверніть увагу: у fassertq все ще потрібне подвоєння дужок
(fassertq 'LIB_BOOK
  '((STATUS ($IF-ADDED
             (#'(lambda (frm s a v)
                  (format t "~%[DEMON] Додано ~A до слота ~A фрейму ~A!~%"
                          v s frm)))))))

;; Тепер якщо ми викличемо fput - побачимо "демон"
(fput 'LIB_BOOK 'STATUS '$value 'OK)

;; Перевіримо структуру глобальної змінної *FRAMES*
(format t "~%--- *FRAMES* ---~%~S~%" *FRAMES*)
