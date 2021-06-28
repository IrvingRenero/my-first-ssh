(ns my-bank-app-2.helpers.validation-test
  (:require [clojure.test :refer :all]
            [my-bank-app-2.helpers.validation :as helpers.validation]))

(def message-validator-pairs
  ["Please enter an email address"
   not-empty
   "irving is not allowed"
   #(not= "irving" %)
   "Your email address doesn't look like an email address"
   #(or (empty? %) (re-seq #"@" %))])

(def validator-map
  {:name  ["Please enter a name" not-empty]
   :email ["Please enter an email address" not-empty
           "Your email address doesn't look like an email address"
           #(or (empty? %) (re-seq #"@" %))
           "irving is not allowed" #(not= "irving" %)]})

(deftest error-messages-for
  (is (= ["irving is not allowed"
          "Your email address doesn't look like an email address"]
         (helpers.validation/error-messages-for
           "irving" message-validator-pairs)))

  (is (= ["Please enter an email address"]
         (helpers.validation/error-messages-for
           "" message-validator-pairs)))

  (is (= ["Please enter an email address"]
         (helpers.validation/error-messages-for
           nil message-validator-pairs)))

  (is (= []
         (helpers.validation/error-messages-for
           "juan@p" message-validator-pairs)))

  (is (= []
         (helpers.validation/error-messages-for
           "juan@p.com" message-validator-pairs))))


(deftest validate
  (are [expected name email]
    (= expected (helpers.validation/validate
                  {:name name
                   :email email }
                  validator-map ))
    {:name ["Please enter a name"]
     :email ["Your email address doesn't look like an email address"
             "irving is not allowed"]}
    ""   "irving"

    {:email ["Your email address doesn't look like an email address"]}
    "a"  "juangmail.com"

    {}
    "a" "juan@gmail.com"))
