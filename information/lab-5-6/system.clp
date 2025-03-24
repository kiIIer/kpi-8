;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Глобальні параметри (defglobal)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defglobal
   ?*min-premise-area* = 65.0
   ?*min-floor-area* = 150.0
   ?*max-premises-per-floor* = 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Шаблони фактів (deftemplate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftemplate building
   (slot building-name)
   (slot year-built (type INTEGER))
   (slot floors (type INTEGER))
   (slot total-area (type FLOAT))
   (multislot premises-list))

(deftemplate premise
   (slot id)
   (slot building-id)
   (slot floor (type INTEGER))
   (slot area (type FLOAT))
   (slot usage-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Додаткові функції (deffunction)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Підсумовує площу всіх приміщень, чиї ідентифікатори задані у списку
(deffunction total-premises-area (?premises-list)
   (bind ?total 0)
   (foreach ?pid ?premises-list
      ;; Знаходимо всі факти premise з вказаним id
      (bind ?found (find-all-facts ((?p premise))
                     (eq (fact-slot-value ?p id) ?pid)))
      ;; Якщо хоча б один факт знайдений – беремо перший та додаємо його area
      (if (> (length$ ?found) 0)
         then
           (bind ?theFact (nth$ 1 ?found))
           (bind ?total (+ ?total (fact-slot-value ?theFact area)))
      )
   )
   (return ?total)
)

(deffunction count-premises-on-floor (?floor)
   (return
      (length$
         (find-all-facts
            ((?p premise))
            (eq ?floor (fact-slot-value ?p floor))))))
   
(deffunction startup-message ()
  (printout t crlf
            "Експертна система для обліку будівель і приміщень завантажена."
            crlf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Початкове наповнення фактів (deffacts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deffacts initial-facts
   (building (building-name "B1")
             (year-built 2020)
             (floors 5)
             (total-area 1000.0)
             ;; Примітка: P101 та P102 - символи, не беріть у лапки
             (premises-list P101 P102))
   (premise (id P101)
            (building-id "B1")
            (floor 1)
            (area 75.0)
            (usage-type "ОФІС"))
   (premise (id P102)
            (building-id "B1")
            (floor 1)
            (area 100.0)
            (usage-type "МАГАЗИН"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Правила (defrule)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Перевірка: чи достатня площа для кожного приміщення
(defrule check-premise-area
   (premise (id ?pid) (area ?a&:(< ?a ?*min-premise-area*)) (usage-type ?ut))
   =>
   (printout t "Приміщення " ?pid " (" ?ut ") має недостатню площу: "
             ?a " кв.м." crlf))

;; Перевірка: чи достатня середня площа на поверх
(defrule check-building-floor-area
   (building (building-name ?b) (floors ?f) (total-area ?ta))
   =>
   (bind ?avg (/ ?ta ?f))
   (if (< ?avg ?*min-floor-area*)
       then 
         (printout t "Будівля " ?b " має недостатню середню площу на поверх: "
                   ?avg " кв.м." crlf)
       else 
         (printout t "Будівля " ?b " відповідає нормам, середня площа на поверх: "
                   ?avg " кв.м." crlf)))

;; Перевірка: чи не забагато приміщень на одному поверсі
(defrule check-premises-per-floor
   ?f <- (premise (floor ?fl))
   =>
   (bind ?count (length$
                   (find-all-facts
                      ((?p premise))
                      (eq ?fl (fact-slot-value ?p floor)))))
   (if (> ?count ?*max-premises-per-floor*)
       then 
         (printout t "На поверсі " ?fl " перевищено допустиму кількість приміщень: "
                   ?count crlf)))

(defrule update-building-total-area
   "Оновлення загальної площі будівлі на основі сумарної площі приміщень."
   ?bfact <- (building (building-name ?b)
                       (premises-list $?plist)
                       (total-area ?ta))
   =>
   (bind ?premises-area (total-premises-area $?plist))
   (bind ?newta ?premises-area)
   (if (neq ?newta ?ta)
       then
           (modify ?bfact (total-area ?newta))
           (printout t "Будівля " ?b " оновлена, нова загальна площа: "
                     ?newta crlf)
       else
           (printout t "Будівля " ?b " має незмінну загальну площу: "
                     ?ta crlf))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Стартове повідомлення
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(startup-message)