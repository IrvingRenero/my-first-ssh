(ns my-bank-app-2.helpers.io)

(defn get-input!
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input! nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))