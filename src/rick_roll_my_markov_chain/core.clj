(ns rick-roll-my-markov-chain.core
  (:require [rick-roll-my-markov-chain.markov :as markov] :reload)
  (:use [clojure.string :only [split trim]]
        [clojure.set :only [subset?]]))

(def ^{:private true}
  rick-roll-lyrics
  "never gonna give you up
   never gonna let you down
   never gonna run around and desert you
   never gonna make you cry
   never gonna say goodbye
   never gonna tell a lie and hurt you")

(def ^{:private true}
  synonyms
  {"gonna" (split "going to" #"\s+")})

(defn- replace-synonyms
  [words]
  (flatten (map #(get synonyms %1 %1) words)))

(def ^{:private true}
  rick-roll-sequence
  (flatten (replace-synonyms (split rick-roll-lyrics #"\s+"))))

(defn- contains-all-lyrics?
  [markov-map]
  (let [words-in-markov-map (set (keys markov-map))
        words-in-lyrics     (set rick-roll-sequence)]
    (subset? words-in-lyrics words-in-markov-map)))

(defn- generate-path
  [length-between-lyrics]
  (let [blanks (repeat length-between-lyrics :pick-a-word)]
    (flatten (interleave rick-roll-sequence (repeat blanks)))))

(defn- pick-word
  [occurrences]
  (when-not (empty? occurrences)
    (let [weighted-words (map (fn [[k v]] (repeat v k)) occurrences)
          pick-list      (flatten weighted-words)
          index-to-pick  (rand-int (count pick-list))]
      (nth pick-list index-to-pick))))

(def ^{:private true}
  remove-word
  dissoc)

(declare walk-the-path)

(defn- try-various-steps-in-path
  [markov-map occurrences-from-previous-word path-to-walk rick-roll-text-so-far]
  (loop [occurrences occurrences-from-previous-word]
    (when-let [suggested-word (pick-word occurrences)]
      (let [rick-roll-text (walk-the-path markov-map path-to-walk (cons suggested-word rick-roll-text-so-far))]
        (if-not (nil? rick-roll-text)
          rick-roll-text
          (recur (remove-word occurrences suggested-word)))))))

(defn- walk-the-path
  [markov-map [current-word & path-to-walk] [previous-word & rest-of-rick-roll :as rick-roll-text-so-far]]
  (when (contains-all-lyrics? markov-map)
    (if (nil? current-word)
      (cons previous-word rest-of-rick-roll)
      (let [occurrences-from-previous-word (get markov-map previous-word)]
        (if (= :pick-a-word current-word)
          (try-various-steps-in-path markov-map occurrences-from-previous-word path-to-walk rick-roll-text-so-far)
          (when-let [occurrence-of-current-word (get occurrences-from-previous-word current-word)]
            (walk-the-path markov-map path-to-walk (cons current-word rick-roll-text-so-far))))))))

(defn rick-roll
  [words length]
  (let [markov-chain                (markov/build-markov-chain words)
        [first-word & path-to-walk] (generate-path length)
        where-to-begin              (seq [first-word])]
    (when-let [rick-roll-text (walk-the-path markov-chain path-to-walk where-to-begin)]
      (reverse rick-roll-text))))

(comment
  (require '[rick-roll-my-markov-chain.core :as core] :reload)
  (require '[rick-roll-my-markov-chain.markov :as markov] :reload)
  (require '[clojure.java.io :as io])
  (require '[clojure.string :as str])
  (defn article->text [filename]
    (remove #(= % "")
            (map (fn [w] (-> w str/lower-case (str/replace #"[^a-z'!.]" "") str/trim))
                 (str/split (str/join " " (line-seq (io/reader filename))) #"\s+"))))
  
  (defn sequence->paragraphs [rick-roll-text sentence-length]
    (loop [text-sequence rick-roll-text
           p             []]
      (if (empty? text-sequence)
        p
        (let [head (take sentence-length text-sequence)
              tail (drop sentence-length text-sequence)]
          (recur tail (conj p head))))))

  (let [sentence-length 15
        filenames       ["the-adventures-of-sherlock-holmes" "frankenstein"]
        texts           (map (comp article->text #(str "resources/" %1 ".txt")) filenames)
        rick-roll-text  (core/rick-roll (apply concat texts) (dec sentence-length))
        paragraphs      (sequence->paragraphs rick-roll-text sentence-length)]
    (doseq [p paragraphs]
      (println (str/join " " p))))
  
)
