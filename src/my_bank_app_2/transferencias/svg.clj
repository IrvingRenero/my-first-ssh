(ns my-bank-app-2.transferencias.svg
  (:require [my-bank-app-2.helpers.validation :refer [if-valid validate]]))

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
  (println "ingresa cuanto tienes, da enter;
            ingresa cuanto quieres transferir, da enter
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