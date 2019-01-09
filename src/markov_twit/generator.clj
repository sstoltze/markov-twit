;; Markov chains: http://howistart.org/posts/clojure/1/
;; Twitter:
(ns markov-twit.generator
  (:gen-class))

(def example "And the Golden Grouse And the Pobble who")

(defn word-chain [transitions]
  (let [n (reduce max (map count transitions))]
    (reduce (fn [r t]
              (merge-with clojure.set/union
                          r
                          {(cond
                             (< (count t) n) (concat (butlast t)
                                                     (take (- n (count t))
                                                           (repeat nil)))
                             :else (butlast t))
                           #{(last t)}}))
            {}
            transitions)))

(defn text->word-chain
  ([text]
   (text->word-chain text 2))
  ([text n]
   (let [words (filter not-empty (clojure.string/split text #"[\s|\n]"))
         transitions (partition-all (+ n 1) 1 words)]
     (word-chain transitions))))

(defn word-list->text [chain]
  (apply str (interpose " " chain)))

(defn walk-chain [chain prefix result min-length]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix (concat (rest prefix) [suffix])
            new-suffix (concat result [suffix])
            text-length (count (word-list->text new-suffix))]
        (if (>= text-length min-length)
          new-suffix
          (recur chain new-prefix new-suffix min-length))))))

(defn generate-text
  ([chain start-phrase min-length]
   (let [prefix (clojure.string/split start-phrase #" ")]
     (word-list->text (walk-chain chain prefix prefix min-length))))
  ([chain start-phrase]
   (generate-text chain start-phrase 280)))

(defn file->word-chain
  ([file-name chain-length]
   (text->word-chain (slurp file-name) chain-length))
  ([file-name]
   (text->word-chain (slurp file-name))))

;; To process several files:
;; (apply merge-with clojure.set/union (map file->word-chain files))
