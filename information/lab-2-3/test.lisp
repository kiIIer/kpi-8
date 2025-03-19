;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load FRAMLisp system: setting up global variable and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *FRAMES* nil "Список фреймів, що містить всі створені фрейми.")

(defun merge-slots (old-slots new-slots)
  "Об'єднує старі слоти old-slots із новими new-slots.
Якщо слот уже існує — оновлює його, інакше додає новий."
  (let ((result old-slots))
    (dolist (slot new-slots)
      (let ((found (assoc (car slot) result)))
        (if found
            (setf (cdr found) (cdr slot))
            (push slot result))))
    result))

(defun deframeq (frame-name &rest slots)
  "Створює або оновлює фрейм FRAME-NAME, додаючи слоти SLOTS.
Кожен елемент з SLOTS має формат (SLOT-NAME (ASPECT (...))...)."
  (let ((existing-frame (assoc frame-name *FRAMES*)))
    (if existing-frame
        (setf (cdr existing-frame)
              (merge-slots (cdr existing-frame) slots))
        (push (cons frame-name slots) *FRAMES*)))
  frame-name)

(defun fget (frame-name slot-name &optional (aspect '$VALUE))
  "Повертає значення (список) аспекту ASPECT у SLOT-NAME для FRAME-NAME.
Якщо фрейм, слот або аспект не знайдені, повертає NIL."
  (let ((frame (assoc frame-name *FRAMES*)))
    (when frame
      (let* ((slots (cdr frame))
             (slot  (assoc slot-name slots)))
        (when slot
          (let ((aspect-pair (assoc aspect (cdr slot))))
            (when aspect-pair
              (cdr aspect-pair))))))))

(defun fprint-slots (frame-name)
  "Виводить усі слоти фрейму frame-name у читабельному форматі."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if frame
        (progn
          (format t "~%Слоти фрейму ~A:" frame-name)
          (dolist (slot (cdr frame))
            (format t "~%  - ~A: ~A" (car slot) (cdr slot))))
        (format t "~%Фрейм ~A не знайдено." frame-name))))

(defun fupdate-slot (frame-name slot-name new-value)
  "Оновлює значення слота slot-name у фреймі frame-name з новим значенням new-value.
Якщо у слоті визначено демон ($IF-ADDED), він буде викликаний.
Повертає T, якщо оновлення вдалося, інакше NIL."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if frame
        (let ((slot (assoc slot-name (cdr frame))))
          (if slot
              (progn
                (let ((value-aspect (assoc '$VALUE (cdr slot))))
                  (if value-aspect
                      (setf (cdr value-aspect) (list new-value))
                      (push (list '$VALUE new-value) (cdr slot))))
                (ftrigger-demon frame-name slot-name new-value)
                t)
              (progn
                (push (list slot-name (list '$VALUE new-value))
                      (cdr frame))
                (ftrigger-demon frame-name slot-name new-value)
                t)))
        nil)))

(defun fattach-demon (frame-name slot-name demon-func)
  "Прикріплює процедуру-демон demon-func до слота slot-name фрейму frame-name.
Демон зберігається як аспект $IF-ADDED у слоті.
Якщо слот або фрейм не існують, повертає NIL, інакше T."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if frame
        (let ((slot (assoc slot-name (cdr frame))))
          (if slot
              (let ((demon-aspect (assoc '$IF-ADDED (cdr slot))))
                (if demon-aspect
                    (setf (cdr demon-aspect) demon-func)
                    (setf (cdr slot)
                          (cons (cons '$IF-ADDED demon-func)
                                (cdr slot))))
                t)
              (progn
                (setf (cdr frame)
                      (cons (list slot-name (list '$VALUE nil)
                                  (list '$IF-ADDED demon-func))
                            (cdr frame)))
                t)))
        nil)))

(defun ftrigger-demon (frame-name slot-name new-value)
  "Якщо у слоту slot-name фрейму frame-name присутній аспект $IF-ADDED,
викликає його з параметрами (frame-name slot-name new-value) та повертає результат.
Якщо демон не визначено, повертає NIL."
  (let ((frame (assoc frame-name *FRAMES*)))
    (when frame
      (let ((slot (assoc slot-name (cdr frame))))
        (when slot
          (let ((demon-aspect (assoc '$IF-ADDED (cdr slot))))
            (when demon-aspect
              (funcall (cdr demon-aspect) frame-name slot-name new-value))))))))

(defun fremove-slot (frame-name slot-name)
  "Видаляє слот slot-name з фрейму frame-name.
Повертає T, якщо успішно, або NIL, якщо слот не знайдено."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if frame
        (let ((slots (cdr frame)))
          (if (assoc slot-name slots)
              (progn
                (setf (cdr frame) (delete (assoc slot-name slots) slots))
                t)
              nil))
        nil)))

(defun fset-parent (child-frame parent-frame)
  "Встановлює для CHILD-FRAME зв'язок АКО з PARENT-FRAME.
Повертає T, якщо успішно, інакше NIL."
  (let ((child-pair (assoc child-frame *FRAMES*)))
    (if child-pair
        (progn
          (fremove-slot child-frame 'AKO)
          (push (list 'AKO (list '$VALUE parent-frame)) (cdr child-pair))
          t)
        nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Демонстраційний блок: завантаження системи, створення фреймів та операції
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "~%Завантаження системи FRAMLisp...~%")
(setf *FRAMES* nil)  ; Очищення бази фреймів

;; Створення фрейму BTI (Бюро технічної інвентаризації)
(deframeq 'BTI
  '(ALL-BUILDINGS ($VALUE nil))
  '(ALL-PREMISES ($VALUE nil))
  '(ACCESS-COUNT
    ($VALUE 0)
    ($IF-GETED
     (lambda (frm slot val)
       (incf (car val))
       (when (zerop (mod (car val) 10))
         (format t "~%[Демон] Фрейм ~A: кількість звернень = ~A (кратно 10)~%"
                 frm (car val))))))
  )
(format t "~%Фрейм BTI створено. Його поточний стан:~%")
(fprint-slots 'BTI)

;; Створення фрейму BUILDING – прототип для будівель
(deframeq 'BUILDING
  '(YEAR-BUILT ($VALUE 1900))
  '(FLOORS ($VALUE 1))
  '(TOTAL-AREA ($VALUE 0))
  '(PREMISES ($VALUE nil)))
(format t "~%Фрейм BUILDING (прототип) створено. Стан BUILDING:~%")
(fprint-slots 'BUILDING)

;; Створення фрейму PREMISE – прототип для нежитлових приміщень
(deframeq 'PREMISE
  '(FLOOR ($VALUE 1))
  '(AREA
    ($VALUE 10)
    ($IF-ADDED
     (lambda (frm slot new-val)
       (when (> new-val 265)
         (format t "~%[Демон] У фреймі ~A: площа ~A перевищує 265!~%" frm new-val)))))
  '(USAGE-TYPE ($VALUE "Unknown usage"))
  '(AKO ($VALUE BTI)))
(format t "~%Фрейм PREMISE (прототип) створено. Стан PREMISE:~%")
(fprint-slots 'PREMISE)

;; Створення конкретного екземпляра будівлі – BUILDING-1
(deframeq 'BUILDING-1
  '(AKO ($VALUE BUILDING))
  '(YEAR-BUILT ($VALUE 2020))
  '(FLOORS ($VALUE 5))
  '(TOTAL-AREA ($VALUE 800))
  '(PREMISES ($VALUE nil)))
(format t "~%Створено фрейм BUILDING-1. Стан BUILDING-1:~%")
(fprint-slots 'BUILDING-1)

;; Додавання BUILDING-1 до списку будівель у BTI
(let ((old (fget 'BTI 'ALL-BUILDINGS)))
  (fupdate-slot 'BTI 'ALL-BUILDINGS (cons 'BUILDING-1 old)))
(format t "~%Додано BUILDING-1 до BTI.ALL-BUILDINGS. Стан BTI зараз:~%")
(fprint-slots 'BTI)

;; Створення конкретного екземпляра приміщення – PREMISE-101
(deframeq 'PREMISE-101
  '(AKO ($VALUE PREMISE))
  '(FLOOR ($VALUE 1))
  '(AREA ($VALUE 60))
  '(USAGE-TYPE ($VALUE "Офісне приміщення")))
(format t "~%Створено фрейм PREMISE-101. Стан PREMISE-101:~%")
(fprint-slots 'PREMISE-101)

;; Додавання PREMISE-101 до списку приміщень у BUILDING-1 та оновлення BTI.ALL-PREMISES
(defun add-premise-to-building (premise building)
  "Додає фрейм premise до списку PREMISES у фреймі building,
а також до ALL-PREMISES у BTI."
  (let ((old-list (fget building 'PREMISES)))
    (fupdate-slot building 'PREMISES (cons premise old-list)))
  (let ((old-bti-list (fget 'BTI 'ALL-PREMISES)))
    (unless (member premise old-bti-list)
      (fupdate-slot 'BTI 'ALL-PREMISES (cons premise old-bti-list)))))
(add-premise-to-building 'PREMISE-101 'BUILDING-1)
(format t "~%Додано PREMISE-101 до BUILDING-1. Стан BUILDING-1 тепер:~%")
(fprint-slots 'BUILDING-1)

;; Оновлення площі PREMISE-101 до 300 (демон перевірить, якщо більше 265)
(format t "~%Оновлення площі PREMISE-101 до 300...~%")
(fupdate-slot 'PREMISE-101 'AREA 300)
(format t "~%Стан PREMISE-101 після оновлення площі:~%")
(fprint-slots 'PREMISE-101)

;; Отримання значення ACCESS-COUNT з BTI (демон спрацює при зверненні)
(format t "~%Отримання ACCESS-COUNT з BTI: ~A~%"
        (fget 'BTI 'ACCESS-COUNT))
(format t "~%Загальний стан фреймової бази (усі фрейми):~%")
(dolist (frame *FRAMES*)
  (fprint-slots (car frame))
  (format t "~%-----------------------------~%"))

(format t "~%Демонстрація завершена.~%")
