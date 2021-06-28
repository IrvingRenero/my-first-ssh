(ns my-bank-app-2.perfil.svg
  (:require [my-bank-app-2.helpers.validation :refer [if-valid validate]]))

(def new-profile-validations
  "vector for validate with new-profile"
  {:name
   ["Please enter a name" not-empty]
   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})

(defn add-new-profile
  []
  ;;related function with option 2
  (println "ingresa un nombre, da enter; ingresa tu email da enter")
  (let [new-profile-data  (seq [(read-line) (read-line)])
        map-newprofile {:name (first new-profile-data) :email (second new-profile-data)}
        my-error-name (validate map-newprofile new-profile-validations)]
    (if-valid map-newprofile new-profile-validations my-error-name
              (do (println "has creado un nuevo perfil") (multiple-options))
              (println "no has podido crear un perfil" my-error-name))))