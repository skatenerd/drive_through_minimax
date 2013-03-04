(ns minimax.core)


(defprotocol GameState
  (reachable-states [this])
  (terminal? [this]))


(defn score-for-current-player [state terminal-scorer]
  10000)

  
