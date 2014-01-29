(ns rick-roll-my-markov-chain.markov)

(defn- words->edges
  [words]
  (map vector words (rest words)))

(defn- edges->counted-edges
  [edges]
  (let [accumulate (fn [memo edge] (merge-with + memo {edge 1}))]
    (map flatten (reduce accumulate {} edges))))

(defn- counted-edges->markov-map
  [counted-edges]
  (let [merge-edge (fn [memo [word following occurrences]] (merge-with merge memo {word {following occurrences}}))]
    (reduce merge-edge {} counted-edges)))

(defn build-markov-chain
  [words]
  (-> words
      words->edges
      edges->counted-edges
      counted-edges->markov-map))
