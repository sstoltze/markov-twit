;; Markov chains: http://howistart.org/posts/clojure/1/
;; Twitter:
(ns markov-twit.generator
  (:gen-class)
  (:require
   [twitter.api.restful :as twitter]
   [twitter.oauth :as twitter-oauth]
   [environ.core :refer [env]]))

(def example "And the Golden Grouse And the Pobble who")

(def credentials (twitter-oauth/make-oauth-creds (env :app-consumer-key)
                                                 (env :app-consumer-secret)
                                                 (env :user-access-token)
                                                 (env :user-access-secret)))

(defn word-chain [transitions]
  (let [n (reduce max (map count transitions))]
    (reduce (fn [r t]
              (merge-with clojure.set/union
                          r
                          {(cond
                             (< (count t) n) #{}
                             ;; (concat (butlast t)
                             ;;                         (take (- n (count t))
                             ;;                               (repeat nil)))
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

(defn walk-chain [chain prefix result max-length]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      result
      (let [suffix (first (shuffle suffixes))
            new-prefix (concat (rest prefix) [suffix])
            new-suffix (concat result [suffix])
            text-length (count (word-list->text new-suffix))]
        (if (>= text-length max-length)
          result
          (recur chain new-prefix new-suffix max-length))))))

(defn generate-text
  ([chain]
   (generate-text chain (word-list->text (first (first chain)))))
  ([chain start-phrase]
   (generate-text chain start-phrase 280))
  ([chain start-phrase max-length]
   (let [prefix (clojure.string/split start-phrase #" ")]
     (word-list->text (walk-chain chain prefix prefix max-length)))))

(defn file->word-chain
  ([file-name]
   (text->word-chain (slurp file-name)))
  ([file-name chain-length]
   (text->word-chain (slurp file-name) chain-length)))

(defn clean-text [text]
  (let [trimmed (if (re-find #"[.,!?]" text)
                  (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text)) ; Trim to last punctuation
                  (apply str (re-seq #".*[^a-zA-Z]+" text))) ; Trim last word
        cleaned (clojure.string/replace trimmed #"[,| ]$" ".")]
    (clojure.string/replace cleaned #"\"" "'")))

;; Max 200 tweets returned by Twitter api
(defn tweets [user]
  (->> (twitter/statuses-user-timeline :oauth-creds credentials :params {:screen-name user :count 200})
       :body
       (map :text)
       (map clojure.string/lower-case))
  ;(map clojure.string/lower-case (map :text (:body (twitter/statuses-user-timeline :oauth-creds credentials :params {:screen-name user :count 200}))))
  )

(defn list->chain [list]
  (apply merge-with clojure.set/union (map text->word-chain list)))

(defn generate-tweet [user]
  (generate-text (list->chain (tweets user))))
;; To process several files:
;; (apply merge-with clojure.set/union (map file->word-chain files))