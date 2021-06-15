(ns my-bank-app-2.core)

(declare what-option multiple-options get-input)



;;we begin the code for create a new-profile 22222222222222222222222222222222222222222222222222222222222

;example for to-validate
#_(def order-details
    {:name "Mitchard Blimmons"
     :email "mitchard.blimmonsgmail.com"})

(def new-profile-validations
  "vector for validate with new-profile"
  {:name
   ["Please enter a name" not-empty]
   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})


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


#_(def my-error-name (validate order-details order-details-validations))



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

;;the way that must look like error name
#_(if-valid order-details order-details-validations my-error-name
            (println :success)
            (println :failure my-error-name))


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;-----------code for the game----------;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(declare successful-move prompt-move game-over query-rows prompt-rows)

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
   (let [new-sum (+ sum n)]
     (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))

(defn connect-right
  [board max-pos pos]
  (let [neighbor (inc pos)
        destination (inc neighbor)]
    (if-not (or (triangular? neighbor) (triangular? pos))
      (connect board max-pos pos neighbor destination)
      board)))

(defn connect-down-left
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn connect-down-right
  [board max-pos pos]
  (let [row (row-num pos)
        neighbor (+ 1 row pos)
        destination (+ 2 row neighbor)]
    (connect board max-pos pos neighbor destination)))

(defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connection-creation-fn]
              (connection-creation-fn new-board max-pos pos))
            pegged-board
            [connect-right connect-down-left connect-down-right])))

(defn new-board
  "Creates a new board with the given number of rows"
  [rows]
  (let [initial-board {:rows rows}
        max-pos (row-tri rows)]
    (reduce (fn [board pos] (add-pos board max-pos pos))
            initial-board
            (range 1 (inc max-pos)))))

(defn pegged?
  "Does the position have a peg in it?"
  [board pos]
  (get-in board [pos :pegged]))

(defn remove-peg
  "Take the peg at given position out of the board"
  [board pos]
  (assoc-in board [pos :pegged] false))

(defn place-peg
  "Put a peg in the board at given position"
  [board pos]
  (assoc-in board [pos :pegged] true))

(defn move-peg
  "Take peg out of p1 and place it in p2"
  [board p1 p2]
  (place-peg (remove-peg board p1) p2))

(defn valid-moves
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil
  otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))

(defn make-move
  "Move peg from p1 to p2, removing jumped peg"
  [board p1 p2]
  (if-let [jumped (valid-move? board p1 p2)]
    (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
  "Do any of the pegged positions have valid moves?"
  [board]
  (some (comp not-empty (partial valid-moves board))
        (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)
(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
       (if (get-in board [pos :pegged])
         "0"
         "-" )))
(defn row-positions
  "Return all positions in the given row"
  [row-num]
  (range (inc (or (row-tri (dec row-num)) 0))
         (inc (row-tri row-num))))
(defn row-padding
  "String of spaces to add to the beginning of a row to center it"
  [row-num rows]
  (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
    (apply str (take pad-length (repeat " ")))))
(defn render-row
  [board row-num]
  (str (row-padding row-num (:rows board))
       (clojure.string/join " " (map (partial render-pos board)
                                     (row-positions row-num)))))
(defn print-board
  [board]
  (doseq [row-num (range 1 (inc (:rows board)))]
    (println (render-row board row-num))))

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))
(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))
(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "enter the letter that you want move, press enter, and then, the second one")
  (let [input (map letter->pos (seq [(read-line) (read-line)]))]
    (if-let [new-board (make-move board (first input) (second input))]
      (user-entered-valid-move new-board)
      (user-entered-invalid-move board))))

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))
(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn the-game
  []
  (prompt-rows))


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
        map-transaction {:curren-required [(first transaction-data ) (second transaction-data )]
                         :origin-destiny [(nth transaction-data 2) (nth transaction-data 3)]
                         :hour (nth transaction-data 4)}
        my-error-name (validate map-transaction transaction-validation)]
    (if-valid map-transaction transaction-validation my-error-name
              (println :success)
              (println :failure my-error-name))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;here start the code for credit;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;I also could make a map with map in the keys for use key to navegate;;;;;;;
(def credit-example {:salary  "100000"
                     :ammount-of-credit "6000000"
                     :credit-type "hipotecario"
                     :months "36"
                     :credit_approval  [10000 6000000 "hipotecario" 36]})

(defn traditional-interest-anual
  [creddit-approval]
  (if  (>= (last creddit-approval)  48 ) 0.28
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
  (if (= (nth creddit-approval 2) "hipotecario" )
    (let [interests  (hipotecay-interest-monthly creddit-approval)] interests)
    (let [interests  (traditional-interest-monthly creddit-approval)] interests)))

(defn real-monthly-payment
  "it give you the real ammount for payment with the real amoritzation formula choosing hipotecary or traditional credit"
  [creddit-approval]
  (if (= (nth creddit-approval 2) "hipotecario" )
    (let [interests  (hipotecay-interest-monthly  creddit-approval)] (payment-ammount interests (last creddit-approval) (second creddit-approval) ))
    (let [interests  (traditional-interest-monthly creddit-approval)] (payment-ammount interests (last creddit-approval) (second creddit-approval)))))




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
    #(> (/ (first %) 3) (real-monthly-payment %)  )]})

(defn credit
  []
  "crea el map especial a usar en la validación de credit"
  (println "ingresa tu salario, da enter;
            ingresa el monto de creido que solicitas, da enter
            ingresa el tipo de credito: tradicional/hipotecario, da enter
            ingresa el plazo que te gustaria")
  (let [credit-data  (seq [(read-line) (read-line) (read-line) (read-line)])
        map-credit {:salary (nth credit-data 0)
                    :ammount-of-credit (nth credit-data 1)
                    :credit-type  (nth credit-data 2)
                    :months (nth credit-data 3)
                    :credit_approval [(Integer. (nth credit-data 0)) (Integer. (nth credit-data 1)) (nth credit-data 2) (Integer. (nth credit-data 3))]}
        my-error-name (validate map-credit credit-validation)]
    (if-valid map-credit  credit-validation my-error-name
              ((println :success )
               (println (str "felicidades tu crédito fue aprobado por: " (nth credit-data 1)) )
               (println (str "tus mensualidades seran de: " (real-monthly-payment (get map-credit :credit_approval)) " MXN"))
               (println (str "tu tasa anual es de: " (* 1200 (current-interest (get map-credit :credit_approval))) "%") )
               (println (str "tu pago en el plazo total sera: "  (* (real-monthly-payment (get map-credit :credit_approval)) (last (get map-credit :credit_approval))) "MXN"))
               (println (str "el monto por intereses que pagaras en total sera de: " (- (nth credit-data 1) (* (real-monthly-payment (get map-credit :credit_approval)) (last (get map-credit :credit_approval))))))
               (println (str "gracias " "por tu preferencia")))
              (println :failure my-error-name))))


;; to choose what is going to do the program;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn what-option
  [options]
  (if  (= 1 options) (transfer)
                     (if (= 2 options) (add-new-profile)
                                       (if (= 3 options) (credit)
                                                         (if (= 4 options) (the-game)
                                                                           (if (= 5 options) (println "adios")
                                                                                             (multiple-options)))))))
(defn get-input
  "Waits for user to enter text and hit enter, then cleans the input"
  ([] (get-input nil))
  ([default]
   (let [input (clojure.string/trim (read-line))]
     (if (empty? input)
       default
       (clojure.string/lower-case input)))))



(defn multiple-options
  []
  "give you the possible options with the app"
  (println "que quieres hacer ahora, escribe el numero
  1 transferencia;
  2 crear un perfil;
  3 solicitar un credito;
  4 jugar no tengo amigos
  5 no quiero hacer nada")
  (let [options (Integer. (get-input 5))]
    (what-option options)))




(defn -main
  []
  (multiple-options))
