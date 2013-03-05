(ns minimax.core-spec
  (:require [speclj.core :refer :all]
            [minimax.core :refer :all]))

(declare new-game-tree)

(defrecord TestGameState [data reachable-states]
  GameState
  (reachable-states [this]
    (map new-game-tree reachable-states))
  (terminal? [this]
    (empty? reachable-states)))

(defn new-game-tree [[data reachable-states]]
  (TestGameState. data reachable-states))

(defn new-leaf-scorer [scores]
  (fn score [state]
    (get scores state)))

(describe "Scoring"
  (it "scores current victory"
    (let [win-state
          (new-game-tree
            [{:round 0 :player 0 :path-to-state :0} []])
          leaf-scorer
          (new-leaf-scorer {
            {:round 0 :player 0 :path-to-state :0} 1
          })]
      (should= 1 (score-for-current-player win-state leaf-scorer))))

  (it "looks one move ahead and sees all losses"
    (let [imminent-loss-state
          (new-game-tree
            [{:round 0 :player 0 :path-to-state [0]} [
              [{:round 0 :player 1 :path-to-state [0 0]} []]]])
          leaf-scorer
          (new-leaf-scorer {
            {:round 0 :player 1 :path-to-state [0 0]} 1
          })]
      (should= -1 (score-for-current-player
                    imminent-loss-state
                    leaf-scorer))))

  (it "looks one move ahead and sees a mixture of wins ties and losses"
    (let [win-soon-state
          (new-game-tree
            [{:round 0 :player 0 :path-to-state [0]} [
              [{:round 0 :player 1 :path-to-state [0 0]} []]
              [{:round 0 :player 1 :path-to-state [0 1]} []]
              [{:round 0 :player 1 :path-to-state [0 2]} []]]])
          leaf-scorer
          (new-leaf-scorer {
            {:round 0 :player 1 :path-to-state [0 0]} 1
            {:round 0 :player 1 :path-to-state [0 1]} 0
            {:round 0 :player 1 :path-to-state [0 2]} -1
          })]
      (should= -1 (score-for-current-player
                    win-soon-state
                    leaf-scorer))))

  (it "looks far into future and gets tie"
    (let [distant-tie-state
          (new-game-tree
            [{:round 0 :player 0 :path-to-state [0]} [
              [{:round 0 :player 1 :path-to-state [0 0]} [
                [{:round 1 :player 0 :path-to-state [0 0 0]} []]
                [{:round 1 :player 0 :path-to-state [0 0 1]} []]
                [{:round 1 :player 0 :path-to-state [0 0 2]} []]]]
              [{:round 0 :player 1 :path-to-state [0 1]} [
                [{:round 1 :player 0 :path-to-state [0 1 0]} []]
                [{:round 1 :player 0 :path-to-state [0 1 1]} []]
                [{:round 1 :player 0 :path-to-state [0 1 2]} []]]]]])
          leaf-scorer
          (new-leaf-scorer {
            {:round 1 :player 0 :path-to-state [0 0 0]} 1
            {:round 1 :player 0 :path-to-state [0 0 1]} -1
            {:round 1 :player 0 :path-to-state [0 0 2]} 0
            {:round 1 :player 0 :path-to-state [0 1 0]} 0
            {:round 1 :player 0 :path-to-state [0 1 1]} 0
            {:round 1 :player 0 :path-to-state [0 1 2]} 0 
          })]
      (should= 0 (score-for-current-player
                   distant-tie-state
                   leaf-scorer))))

  (it "looks far into future and gets loss"
    (let [distant-loss-state
          (new-game-tree
            [{:round 0 :player 0 :path-to-state [0]} [
              [{:round 0 :player 1 :path-to-state [0 0]} [
                [{:round 1 :player 0 :path-to-state [0 0 0]} []]
                [{:round 1 :player 0 :path-to-state [0 0 1]} []]
                [{:round 1 :player 0 :path-to-state [0 0 2]} []]]]
              [{:round 0 :player 1 :path-to-state [0 1]} [
                [{:round 1 :player 0 :path-to-state [0 1 0]} []]
                [{:round 1 :player 0 :path-to-state [0 1 1]} []]
                [{:round 1 :player 0 :path-to-state [0 1 2]} []]]]]])
          leaf-scorer
          (new-leaf-scorer {
            {:round 1 :player 0 :path-to-state [0 0 0]} -1
            {:round 1 :player 0 :path-to-state [0 0 1]} -1
            {:round 1 :player 0 :path-to-state [0 0 2]} -1
            {:round 1 :player 0 :path-to-state [0 1 0]} 0
            {:round 1 :player 0 :path-to-state [0 1 1]} 0
            {:round 1 :player 0 :path-to-state [0 1 2]} 0
          })]
      (should= -1 (score-for-current-player
                    distant-loss-state
                    leaf-scorer))))

  (it "looks far into future and gets win"
    (let [distant-win-state
          (new-game-tree
            [{:round 0 :player 0 :path-to-state [0]} [
              [{:round 0 :player 1 :path-to-state [0 0]} [
                [{:round 1 :player 0 :path-to-state [0 0 0]} []]
                [{:round 1 :player 0 :path-to-state [0 0 1]} []]
                [{:round 1 :player 0 :path-to-state [0 0 2]} []]]]
              [{:round 0 :player 1 :path-to-state [0 1]} [
                [{:round 1 :player 0 :path-to-state [0 1 0]} []]
                [{:round 1 :player 0 :path-to-state [0 1 1]} []]
                [{:round 1 :player 0 :path-to-state [0 1 2]} []]]]]])
          leaf-scorer
          (new-leaf-scorer {
            {:round 1 :player 0 :path-to-state [0 0 0]} 1
            {:round 1 :player 0 :path-to-state [0 0 1]} -1
            {:round 1 :player 0 :path-to-state [0 0 2]} -1
            {:round 1 :player 0 :path-to-state [0 1 0]} 1
            {:round 1 :player 0 :path-to-state [0 1 1]} -1
            {:round 1 :player 0 :path-to-state [0 1 2]} -1
          })]
      (should= 1 (score-for-current-player
                   distant-win-state
                   leaf-scorer))))
)

(run-specs)

