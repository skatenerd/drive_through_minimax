(ns minimax.core)

(declare inverse)

(defprotocol GameState
  (reachable-states [this])
  (terminal? [this]))


(defn score-for-current-player [state terminal-scorer]
  (if (terminal? state)
    (terminal-scorer (:data state))
    (let [the-reachable-states (reachable-states state)
          best-scoring-for-opponent (apply max-key
                                           #(score-for-current-player
                                              %
                                              terminal-scorer)
                                           the-reachable-states)]
      (inverse (score-for-current-player
                 best-scoring-for-opponent
                 terminal-scorer)))))

(defn- inverse [x]
  (* x -1))
  
