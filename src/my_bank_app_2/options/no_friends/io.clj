(ns my-bank-app-2.options.no-friends.io
  (:require [my-bank-app-2.helpers.io :as helpers.io]
            [my-bank-app-2.options.no-friends.board :as board]
            [my-bank-app-2.options.no-friends.move :as move]))

(declare prompt-move prompt-rows)

(defn letter->pos
  "Converts a letter string to the corresponding position number"
  [letter]
  (inc (- (int (first letter)) board/alpha-start)))

(defn prompt-move
  ([valid-move-handler!
    invalid-move-handler!
    board]
   (println "\nHere's your board:")
   (board/print-board board)
   (println "enter the letter that you want move, press enter, and then, the second one")
   (let [input (map letter->pos (seq [(read-line) (read-line)]))]
     (if-let [new-board (move/maybe-make-move board (first input) (second input))]
       (valid-move-handler! valid-move-handler! invalid-move-handler! new-board)
       (invalid-move-handler! valid-move-handler! invalid-move-handler! board)))))

(defn- prompt-empty-peg!
  [valid-move-handler!
   invalid-move-handler!
   board]
  (println "Here's your board:")
  (board/print-board board)
  (println "Remove which peg? [e]")
  (prompt-move valid-move-handler!
               invalid-move-handler!
               (move/remove-peg board (letter->pos (helpers.io/get-input! "e")))))

(defn prompt-rows!
  [valid-move-handler!
   invalid-move-handler!]
  (println "How many rows? [5]")
  (let [rows (Integer. (helpers.io/get-input! 5))
        board (board/new-board rows)]
    (prompt-empty-peg! valid-move-handler!
                       invalid-move-handler!
                       board)))