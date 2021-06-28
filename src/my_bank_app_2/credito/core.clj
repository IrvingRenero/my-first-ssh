(ns my-bank-app-2.credito.core
  (:require [my-bank-app-2.helpers.validation :refer [if-valid validate]]))

;;I also could make a map with map in the keys for use key to navegate;;;;;;;
(def credit-example {:salary  "100000"
                     :ammount-of-credit "6000000"
                     :credit-type "hipotecario"
                     :months "36"
                     :credit_approval  [10000 6000000 "hipotecario" 36]})

(defn traditional-interest-anual
  [creddit-approval]
  (if  (>= (last creddit-approval)  48) 0.28
       (if (>= (last creddit-approval) 36) 0.25
           (if (>= (last creddit-approval) 24) 0.22
               (if (>= (last creddit-approval) 18) 0.21
                   (if (>= (last creddit-approval) 12) 0.2
                       (println "el plazo es muy corto")))))))
(defn exp [x n] (if (zero? n) 1 (* x (exp x (dec n)))))

(defn payment-ammount
  "real amortization formula"
  [interests period  ammount]
  (/ ammount (/ (- 1 (/ 1 (exp (+ 1 interests) period))) interests)))

(defn traditional-interest-monthly
  "with anual interest for traditional credit compute interest per month"
  [creddit-approval]
  (/ (traditional-interest-anual creddit-approval) 12))

(defn hipotecary-interest-anual
  "with anual interest for traditional credit simulate interest for hipotecary credit"
  [creddit-approval]
  (+ (traditional-interest-anual creddit-approval) (/ -1 10)))

(defn hipotecay-interest-monthly
  "with anual interest for hipotecary credit compute interest per month"
  [creddit-approval]
  (/ (hipotecary-interest-anual creddit-approval) 12))

(defn current-interest
  [creddit-approval]
  (if (= (nth creddit-approval 2) "hipotecario")
    (let [interests  (hipotecay-interest-monthly creddit-approval)] interests)
    (let [interests  (traditional-interest-monthly creddit-approval)] interests)))

(defn real-monthly-payment
  "it give you the real ammount for payment with the real amoritzation formula choosing hipotecary or traditional credit"
  [creddit-approval]
  (if (= (nth creddit-approval 2) "hipotecario")
    (let [interests  (hipotecay-interest-monthly  creddit-approval)] (payment-ammount interests (last creddit-approval) (second creddit-approval)))
    (let [interests  (traditional-interest-monthly creddit-approval)] (payment-ammount interests (last creddit-approval) (second creddit-approval)))))

#_(macroexpand
   '(if-valid order-details order-details-validations my-error-name
              (println :success)
              (println :failure my-error-name)))
#_(let*
   [my-error-name (validate order-details order-details-validations)]
   (if (clojure.core/empty? my-error-name)
     (println :success)
     (println :failure my-error-name)))

#_(if-valid order-details order-details-validations my-error-name
            (println :success)
            (println :failure my-error-name))

;;vector with validations and error codes for the credit;;;
(def credit-validation
  {:salary
   ["Please enter an ammount of your current balance" not-empty]
   :ammount-of-credit
   ["Please enter an ammount of your current balance" not-empty]
   :credit-type
   ["no has puesto correctamente el tipo de credito" #(or (= "hipotecario" %) (= "tradicional" %))]
   :months
   ["Please enter an ammount of your current balance" not-empty
    "el plazo es muy pequeño" #(> (Integer. %) 12)
    "el plazo es muy grande" #(< (Integer. %) 60)]
   :credit_approval
   ["si es el único código de error significa que no cumples con los requisitos; prueba aumentar el plazo o reducir el monto"
    #(> (/ (first %) 3) (real-monthly-payment %))]})

(defn credit
  []
  "crea el map especial a usar en la validación de credit"
  (println "ingresa tu salario, da enter;
            ingresa el monto de credito que solicitas, da enter
            ingresa el tipo de credito: tradicional/hipotecario, da enter
                    ingresa el plazo que te gustaria")
  (let [[salary ammount type time]  (seq [(read-line) (read-line) (read-line) (read-line)])
        map-credit {:salary salary
                    :ammount-of-credit ammount
                    :credit-type  type
                    :months time
                    :credit_approval [(Integer. salary) (Integer. ammount) type (Integer. time)]}
        my-error-name (validate map-credit credit-validation)]
    (if-valid map-credit  credit-validation my-error-name
              ((println :success)
               (println (str "felicidades tu crédito fue aprobado por: " ammount))
               (println (str "tus mensualidades seran de: " (real-monthly-payment (get map-credit :credit_approval)) " MXN"))
               (println (str "tu tasa anual es de: " (* 1200 (current-interest (get map-credit :credit_approval))) "%"))
               (println (str "tu pago en el plazo total sera: "  (* (real-monthly-payment (get map-credit :credit_approval)) (last (get map-credit :credit_approval))) "MXN"))
               (println (str "el monto por intereses que pagaras en total sera de: " (- (* (real-monthly-payment (get map-credit :credit_approval)) (last (get map-credit :credit_approval))) (Integer. ammount)))))
              (println :failure my-error-name))))
