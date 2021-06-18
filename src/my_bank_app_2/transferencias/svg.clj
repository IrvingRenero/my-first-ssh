(ns my-bank-app-2.transferencias.svg)

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

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

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

(defn transfer
  []
  (println "ingresa cuanto tienes, da enter;
            ingresa cuanto quieres transferir, da enter
            ingresa tu pais actual, da enter
            ingresa el pais destino, da enter
            ingresa la hora, da enter")
  (let [transaction-data  (seq [(read-line) (read-line) (read-line) (read-line) (read-line)])
        map-transaction {:curren-required [(first transaction-data) (second transaction-data)]
                         :origin-destiny [(nth transaction-data 2) (nth transaction-data 3)]
                         :hour (nth transaction-data 4)}
        my-error-name (validate map-transaction transaction-validation)]
    (if-valid map-transaction transaction-validation my-error-name
              (println :success)
              (println :failure my-error-name))))
