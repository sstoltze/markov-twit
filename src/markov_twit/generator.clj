;; Markov chains and getting started with Twitter: http://howistart.org/posts/clojure/1/
(ns markov-twit.generator
  (:gen-class)
  (:require
   [cheshire.core :as cheshire]        ;; JSON, used by clj-http
   [clj-http.client :as client]        ;; HTTP
   [environ.core :refer [env]]         ;; Twitter authentication
   [twitter.api.restful :as twitter]   ;; Twitter interface
   [twitter.oauth :as twitter-oauth])) ;; Twitter authentication

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

(defn list->word-chain
  ([list]
   (list->word-chain list 2))
  ([list chain-length]
   (apply merge-with clojure.set/union (map text->word-chain
                                            list
                                            (repeat chain-length)))))

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
  ([input-list]
   (generate-and-run-chain input-list 1))
  ([input-list n]
   (generate-and-run-chain input-list n 2))
  ([input-list n chain-length]
   (map (comp clean-text
              generate-text)
        ;; n copies of the generated chain
        (repeat n (list->word-chain input-list chain-length))
        ;; n starting phrases
        (take n
              (shuffle
               (map (fn [x]
                      (take chain-length
                            (clojure.string/split x #" ")))
                    input-list))))))

(defn generate-tweet-orig
  ([user]
   (generate-tweet-orig user 1))
  ([user n]
   (generate-and-run-chain (tweets user) n)))

(defmacro def-markov-function [name function & {:keys [doc-string parameter-name count-name chain-length-name]}]
  "name is the name of the function to be defined, function should accept a single parameter
and return a list of texts used for generating the markov chain.

Parameter-name and count-name allows naming of parameters to the generated function. If one of them is not supplied, a gensym is used instead."
  (let [parameter-name (or parameter-name    (gensym "parameter"))
        count-name     (or count-name        (gensym "count"))
        chain-length   (or chain-length-name (gensym "chain-length"))]
    (concat `(defn ~name)
            (when doc-string `(~doc-string))
            `(([~parameter-name]
               (~name ~parameter-name 1))
              ([~parameter-name ~count-name]
               (generate-and-run-chain (~function ~parameter-name) ~count-name))
              ([~parameter-name ~count-name ~chain-length]
               (generate-and-run-chain (~function ~parameter-name) ~count-name ~chain-length))))))

(def-markov-function generate-tweet
  tweets
  :parameter-name user
  :count-name n
  :chain-length-name chain-length
  :doc-string "Returns n tweets generated from the timeline of user.")
(def-markov-function generate-reddit-comment
  get-reddit-comments
  :parameter-name user
  :count-name n
  :chain-length-name chain-length
  :doc-string "Returns n comments generated from the comments made by user.")
(def-markov-function generate-reddit-post
  get-reddit-posts
  :parameter-name subreddit
  :count-name n
  :chain-length-name chain-length
  :doc-string "Returns n posts generated from the posts of the subreddit.")
(def-markov-function generate-reddit-title
  get-reddit-titles
  :parameter-name subreddit
  :count-name n
  :chain-length-name chain-length
  :doc-string "Returns n titles generated from the titles of the subreddit.")
