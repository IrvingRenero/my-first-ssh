(ns my-bank-app-2.options.no-friends.core
  (:require [my-bank-app-2.helpers.io :as helpers.io]
            [my-bank-app-2.options.no-friends.board :as board]
            [my-bank-app-2.options.no-friends.io.prompt :as io.prompt]))


(declare successful-move prompt-move game-over query-rows)



















(defn start!
  []
  (io.prompt/prompt-rows))
