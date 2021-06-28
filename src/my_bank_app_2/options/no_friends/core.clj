(ns my-bank-app-2.options.no-friends.core
  (:require [my-bank-app-2.helpers.io :as helpers.io]
            [my-bank-app-2.options.no-friends.board :as board]
            [my-bank-app-2.options.no-friends.io :as io]
            [my-bank-app-2.options.no-friends.move :as move]))

(defn game-over
  "Announce the game is over and prompt to play again"
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (board/print-board board)
    (println "Play again? y/n [y]")
    (let [input (helpers.io/get-input! "y")]
      (if (= "y" input)
        (io/prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn user-entered-valid-move
  "Handles the next step after a user has entered a valid move"
  [valid-move-handler! invalid-move-handler! board]
  (if (move/can-move? board)
    (io/prompt-move  valid-move-handler! invalid-move-handler! board)
    (game-over board)))

(defn user-entered-invalid-move
  "Handles the next step after a user has entered an invalid move"
  [valid-move-handler! invalid-move-handler! board]
  (println "\n!!! That was an invalid move :(\n")
  (io/prompt-move valid-move-handler! invalid-move-handler! board))

(defn start!
  []
  (io/prompt-rows! user-entered-valid-move
                   user-entered-invalid-move))
