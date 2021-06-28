(ns my-bank-app-2.core
  (:require [my-bank-app-2.credito.core :as credito.core]
            [my-bank-app-2.helpers.io :as helpers.io]
            [my-bank-app-2.helpers.validation :refer [if-valid validate]]
            [my-bank-app-2.options.no-friends.core :as no-friends.core]))

(declare what-option
         multiple-options)

;;we begin the code for create a new-profile
;example for to-validate
#_(def order-details
    {:name "Mitchard Blimmons"
     :email "mitchard.blimmonsgmail.com"})

(def new-profile-validations
  "vector for validating with new-profile"
  {:name  ["Please enter a name" not-empty]
   :email ["Please enter an email address" not-empty
           "Your email address doesn't look like an email address"
           #(or (empty? %) (re-seq #"@" %))]})

(defn add-new-profile
  []
  ;;related function with option 2
  (println "ingresa un nombre, da enter; ingresa tu email da enter")
  (let [[name email] (seq [(read-line) (read-line)])
        map-newprofile {:name name :email email}
        my-error-name (validate map-newprofile new-profile-validations)]
    (if-valid map-newprofile new-profile-validations my-error-name
              (do (println "has creado un nuevo perfil") (multiple-options))
              (println "no has podido crear un perfil" my-error-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;; code for transaction;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def transaction-validation
  {:curren-required
   ["Please enter an ammount of your current balance" #(not-empty (first %))
    "pleease enter the ammount of your transaction required" #(not-empty (second %))
    "your balance is not enough" #(> (Integer. (first %)) (Integer. (second %)))]
   :origin-destiny
   ["please enter your current country" #(not-empty (first %))
    "please enter the transaction's destiny country" #(not-empty (first %))
    "you can't make international transactions"  #(= (first %) (second %))]
   :hour ["it's too late go to sleep" #(< 4 (Integer. %))]})

(defn transfer
  []
  (println "ingresa cuánto tienes, da enter;
            ingresa cuánto quieres transferir, da enter
            ingresa tu pais actual, da enter
            ingresa el pais destino, da enter
            ingresa la hora, da enter")
  (let [[balance ammount origin destiny hour]  (seq [(read-line) (read-line) (read-line) (read-line) (read-line)])
        map-transaction {:curren-required [balance ammount]
                         :origin-destiny [origin destiny]
                         :hour hour}
        my-error-name (validate map-transaction transaction-validation)]
    (if-valid map-transaction transaction-validation my-error-name
              (println :success)
              (println :failure my-error-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;here start the code for credit;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;I also could make a map with map in the keys for use key to navegate;;;;;;;


(def credit-example
  {:salary            "100000"
   :ammount-of-credit "6000000"
   :credit-type       "hipotecario"
   :months            "36"
   :credit_approval   [10000 6000000 "hipotecario" 36]})

(defn traditional-interest-anual
  [creddit-approval]
  (cond
    (>= (last creddit-approval)  48)
    0.28

    (>= (last creddit-approval) 36)
    0.25

    (>= (last creddit-approval) 24)
    0.22

    (>= (last creddit-approval) 18)
    0.21

    (>= (last creddit-approval) 12)
    (println "el plazo es muy corto")))

(defn exp [x n]
  (if (zero? n)
    1
    (* x (exp x (dec n)))))

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
  "it gives you the real ammount for payment with the real amoritzation formula choosing hipotecary or traditional credit"
  [creddit-approval]
  (if (= (nth creddit-approval 2) "hipotecario")
    (let [interests  (hipotecay-interest-monthly  creddit-approval)]
      (payment-ammount interests (last creddit-approval) (second creddit-approval)))
    (let [interests  (traditional-interest-monthly creddit-approval)]
      (payment-ammount interests (last creddit-approval) (second creddit-approval)))))

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


;; to choose what is going to do the program;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def ^:private application-options
  {"transferencia"         transfer
   "crear un perfil"       add-new-profile
   "solicitar un credito"  credito.core/credit
   "jugar no tengo amigos" no-friends.core/start!
   "no quiero hacer nada"  #(println "adios")})

(defn- option-index->option-fn
  [option-index]
  (nth (vals application-options)
       (dec option-index)
       nil))

(def ^:private app-options
  (map-indexed (fn [index text] (str (inc index) ". " text ":"))
               (keys application-options)))

(defn print-app-options! []
  (doall (map println app-options)))

(defn start-app-menu! []
  (print-app-options!)
  (let [option-index     (-> application-options count helpers.io/get-input! Integer.)
        option-function! (option-index->option-fn option-index)]
    (if option-function!
      (option-function!)
      (do (println "Opción inválida\n")
          (recur)))))

(defn -main
  []
  (start-app-menu!))
