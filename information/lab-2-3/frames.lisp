(defvar *FRAMES* nil "Список фреймів, що містить всі створені фрейми.")

(defun fexists? (frame-name)
  "Перевіряє, чи існує фрейм з іменем FRAME-NAME у глобальному списку *FRAMES*.
   Повертає T, якщо фрейм існує, інакше NIL."
  (not (null (assoc frame-name *FRAMES*))))

(defun fcount-frames ()
  "Повертає кількість фреймів у глобальній змінній *FRAMES*."
  (length *FRAMES*))

(defun flist-frames ()
  "Повертає список імен усіх фреймів з *FRAMES*."
  (mapcar #'car *FRAMES*))

(defun fremove (frame-name)
  "Видаляє фрейм frame-name з глобальної змінної *FRAMES*, якщо він існує.
Повертає T, якщо успішно видалено, або NIL, якщо фрейму не знайдено."
  (let ((pair (assoc frame-name *FRAMES*)))
    (if pair
        (progn
          (setf *FRAMES* (delete pair *FRAMES*))
          t)
      nil)))

(defun frename (old-name new-name)
  "Змінює ім’я фрейма з old-name на new-name. Повертає T, якщо вдалося, інакше NIL."
  (cond
    ((fexists? new-name)
     ;; Вже існує фрейм з new-name
     nil)
    ((not (fexists? old-name))
     ;; Старого фрейма немає
     nil)
    (t
     (let ((pair (assoc old-name *FRAMES*)))
       (setf (car pair) new-name)
       t))))

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


(defun fprint-slots (frame-name)
  "Виводить усі слоти фрейма frame-name у читабельному форматі."
  (let ((frame (assoc frame-name *FRAMES*)))
    (if frame
        (progn
          (format t "~%Слоти фрейма ~A:" frame-name)
          (dolist (slot (cdr frame))
            (format t "~%  - ~A: ~A" (car slot) (cdr slot))))
      (format t "~%Фрейм ~A не знайдено." frame-name))))


(defun fremove-slot (frame-name slot-name)
  "Видаляє слот SLOT-NAME з фрейма FRAME-NAME у *FRAMES*.
   Повертає T, якщо успішно, NIL, якщо фрейм або слот не знайдено."
  (let ((frame-pair (assoc frame-name *FRAMES*)))
    (if (null frame-pair)
        ;; Фрейм не знайдено
        nil
      (let ((slot-pair (assoc slot-name (cdr frame-pair))))
        (if (null slot-pair)
            ;; Слот не знайдено
            nil
          ;; Видаляємо слот зі списку слотів (cdr frame-pair) 
          (progn
            (setf (cdr frame-pair) (delete slot-pair (cdr frame-pair)))
            t))))))


(defun fslot-exists? (frame-name slot-name)
  "Перевіряє, чи існує слот SLOT-NAME у фреймі FRAME-NAME.
   Повертає T, якщо слот існує, інакше NIL."
  (let ((frame-pair (assoc frame-name *FRAMES*)))
    (when frame-pair
      (not (null (assoc slot-name (cdr frame-pair)))))))

(defun frename-slot (frame-name old-slot new-slot)
  "Перейменовує слот OLD-SLOT у фреймі FRAME-NAME на NEW-SLOT.
   Повертає T, якщо вдалося, інакше NIL."
  (let ((frame-pair (assoc frame-name *FRAMES*)))
    (cond
      ;; Якщо фрейму немає:
      ((null frame-pair) 
       nil)
      ;; Якщо слот з назвою NEW-SLOT уже існує - уникаємо дублювання:
      ((assoc new-slot (cdr frame-pair))
       nil)
      (t
       (let ((target-slot (assoc old-slot (cdr frame-pair))))
         (if (null target-slot)
             ;; Слот з іменем OLD-SLOT не знайдено
             nil
           (progn
             ;; Замінюємо car у знайденого слота (стару назву) на NEW-SLOT
             (setf (car target-slot) new-slot)
             t)))))))


(defun fset-parent (child-frame parent-frame)
  "Встановлює (або оновлює) для CHILD-FRAME зв’язок АКО з PARENT-FRAME.
   Повертає T, якщо успішно, інакше NIL."
  (let ((child-pair (assoc child-frame *FRAMES*)))
    (if (null child-pair)
        ;; Фрейм CHILD-FRAME не існує
        nil
      (progn
        ;; Прибираємо старий слот AKO, якщо він був
        (fremove-slot child-frame 'AKO)
        ;; Додаємо слот AKO ($VALUE TRANSPORT),
        ;; щоб зберегти батька як один символ (не (TRANSPORT)).
        (push `(AKO ($VALUE ,parent-frame)) (cdr child-pair))
        t))))

(defun fget (frame-name slot-name &optional (aspect '$VALUE))
  "Повертає значення (список) зазначеного ASPECT у SLOT-NAME для FRAME-NAME.
   Якщо фрейм, слот або аспект не знайдені, повертає NIL."
  (let ((frame (assoc frame-name *FRAMES*)))
    (when frame
      (let* ((slots (cdr frame))
             (slot  (assoc slot-name slots)))
        (when slot
          ;; Структура слота має вигляд: (SLOT-NAME (ASPECT1 (val...)) (ASPECT2 (val...)) ...)
          ;; Ми знаходимо підсписок, перший елемент якого є ASPECT, і повертаємо його CDR.
          (let ((aspect-pair (assoc aspect (cdr slot))))
            (when aspect-pair
              ;; CDR aspect-pair містить фактичні дані
              (cdr aspect-pair))))))))

(defun get-parent (frame-name)
  "Повертає ІМ’Я батьківського фрейма (ціль `AKO`) для FRAME-NAME,
   або NIL, якщо батька не встановлено."
  (let ((parent-slot (fget frame-name 'AKO)))
    ;; parent-slot має бути списком типу ("TRANSPORT")
    ;; якщо було викликано (FSET-PARENT 'CAR 'TRANSPORT).
    (when (and parent-slot (consp parent-slot))
      (car parent-slot))))

(defun fget-inherited (frame-name slot-name &optional (aspect '$VALUE))
  "Повертає значення ASPECT у SLOT-NAME для FRAME-NAME, 
   використовуючи механізм успадкування через AKO.
   Якщо слот не знайдено ніде, повертає NIL."
  (let ((local-value (fget frame-name slot-name aspect)))
    (if local-value
        local-value
      (let ((parent (get-parent frame-name)))
        (when parent
          (fget-inherited parent slot-name aspect))))))

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
                    ;; Оновлюємо cdr слота напряму:
                    (setf (cdr slot)
                          (cons (cons '$IF-ADDED demon-func)
                                (cdr slot))))
                t)
              (progn
                ;; Якщо слот не існує – створюємо його
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

