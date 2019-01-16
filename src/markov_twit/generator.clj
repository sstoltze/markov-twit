;; Markov chains and getting started with Twitter: http://howistart.org/posts/clojure/1/
(ns markov-twit.generator
  (:gen-class)
  (:require
   [cheshire.core :as cheshire]
   [clj-http.client :as client]
   [environ.core :refer [env]]
   [twitter.api.restful :as twitter]
   [twitter.oauth :as twitter-oauth]))

(def http-header {"User-Agent" "markov-twit clj-http.client/3.9.1"})

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
                             :else (butlast t))
                           #{(last t)}}))
            {}
            transitions)))

(defn text->word-chain
  ([text]
   (text->word-chain text 2))
  ([text n]
   (let [words (filter not-empty
                       (clojure.string/split text #"[\s|\n]"))
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
   (generate-text chain (word-list->text (first (first (shuffle (filter first chain)))))))
  ([chain start-phrase]
   (generate-text chain start-phrase 280))
  ([chain start-phrase max-length]
   (let [prefix (if (string? start-phrase)
                  (clojure.string/split start-phrase #" ")
                  start-phrase)]
     (word-list->text (walk-chain chain prefix prefix max-length)))))

(defn file->word-chain
  ([file-name]
   (text->word-chain (slurp file-name)))
  ([file-name chain-length]
   (text->word-chain (slurp file-name) chain-length)))

(defn clean-text [text]
  (let [trimmed (if (re-find #"[.,!?]" text)
                  (apply str (re-seq #"[\s\w]+[^.!?,]*[.!?,]" text)) ; Trim to last punctuation
                  (apply str (re-seq #".*[^a-zA-Z]+" text)))         ; Trim last word
        strip-link (clojure.string/replace trimmed #" https://.*\.$" "")
        cleaned (clojure.string/replace strip-link #"[,| |:]$" ".")]
    (clojure.string/replace cleaned #"\"" "'")))

(defn list->word-chain [list]
  (apply merge-with clojure.set/union (map text->word-chain list)))

(defn tweets [user]
  (->> (twitter/statuses-user-timeline :oauth-creds credentials
                                       ;; Max 200 tweets returned by Twitter api
                                       :params {:screen-name user :count 200})
       :body
       (map :text)
       ;; (map clojure.string/lower-case)
       ))

(defn- get-reddit-helper [param key size user after result]
  (let [url (str "https://reddit.com/"
                 (if user
                   "u/"
                   "r/")
                 param
                 ".json?limit=100&after="
                 after)
        info (:data
              (:body
               (client/get url
                           {:as :json
                            :headers http-header})))
        children (filter not-empty
                         (map (comp key :data)
                              (:children info)))
        new-after (:after info)
        n (count children)]
    (if (or (<= size n)
            (empty? new-after)
            (= new-after after))
      (concat result (take size children))
      (recur param key (- size n) user new-after (concat result children)))))

(defn get-reddit-info
  "Key-options:
    For subreddit - :title, :selftext
    For user - :body
    For other options, consult the Reddit API."
  ([param key & {:keys [size user after]}]
   (get-reddit-helper param key (or size 200) user after [])))

(defn get-reddit-comments
  ([user & [n]]
   (get-reddit-info user :body :user true :size n)))

(defn get-reddit-titles
  ([subreddit & [n]]
   (get-reddit-info subreddit :title :size n)))

(defn get-reddit-posts
  ([subreddit & [n]]
   (get-reddit-info subreddit :selftext :size n)))

(defn generate-and-run-chain
  ([input-list n]
   (map (comp clean-text
              generate-text)
        ;; n copies of the generated chain
        (repeat n (list->word-chain input-list))
        ;; n starting phrases
        (take n
              (shuffle
               (map (fn [x]
                      (take 2
                            (clojure.string/split x #" ")))
                    input-list))))))

(defn generate-tweet
  ([user]
   (generate-tweet user 1))
  ([user n]
   (generate-and-run-chain (tweets user) n)))

(defn generate-reddit-comment
  ([user]
   (generate-reddit-comment user 1))
  ([user n]
   (generate-and-run-chain (get-reddit-comments user) n)))

(defn generate-reddit-post
  ([subreddit]
   (generate-reddit-post subreddit 1))
  ([subreddit n]
   (generate-and-run-chain (get-reddit-posts subreddit) n)))

(defn generate-reddit-title
  ([subreddit]
   (generate-reddit-title subreddit 1))
  ([subreddit n]
   (generate-and-run-chain (get-reddit-titles subreddit) n)))
