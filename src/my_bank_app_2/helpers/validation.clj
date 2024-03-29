(ns my-bank-app-2.helpers.validation)

(defn error-messages-for
  "Return a seq of error messages"
  [to-validate message-validator-pairs]
  (vec
    (map first (filter #(not ((second %) to-validate))
                          (partition 2 message-validator-pairs)))))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value                               (get to-validate fieldname)
                  error-messages                      (error-messages-for value validation-check-groups)]
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
