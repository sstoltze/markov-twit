(ns markov-twit.server
  (:gen-class)
  (:require
   [ring.adapter.jetty :as jetty]
   [ring.util.response :as response]
   [ring.middleware.params :as params]
   [ring.middleware.keyword-params :as keyword-params]
   [markov-twit.generator :as generator]))

(defn parse-query-string [request]
  (let [params       (:params request)
        reddit-user  (:reddit-user  params)
        subreddit    (:subreddit    params)
        twitter-user (:twitter-user params)
        chain-length (Integer. (or (:chain params)
                                   "2"))
        tweets       (if (not-empty twitter-user)
                       (try
                         (reduce concat
                                 (map (fn [tweet] (concat "<p>" tweet "</p>"))
                                      (generator/generate-tweet twitter-user 5 280 chain-length)))
                         (catch Exception e
                           ""))
                       "")
        reddit-posts (if (not-empty subreddit)
                       (try
                         (reduce concat (map concat
                                             (repeat 5 "<p><h3>")
                                             (generator/generate-reddit-title subreddit 5 350 chain-length)
                                             (repeat 5 "</h3>")
                                             (generator/generate-reddit-post  subreddit 5 600 chain-length)
                                             (repeat 5 "</p>")))
                         (catch Exception e
                           ""))
                       "")
        reddit-comments (if (not-empty reddit-user)
                          (try
                            (reduce concat
                                    (map (fn [comment] (concat "<p>" comment "</p>"))
                                         (generator/generate-reddit-comment reddit-user 5 500 chain-length)))
                            (catch Exception e
                              ""))
                          "")]
    (concat
     (if (not-empty twitter-user)
       (if (seq tweets)
         (concat "<h2>Generated tweets from @"
                 twitter-user
                 "</h2>"
                 tweets
                 "<br>")
         (concat "<h2>Could not find twitter user @" twitter-user "</h2>"))
       "")
     (if (not-empty subreddit)
       (if (seq reddit-posts)
         (concat "<h2>Generated posts from subreddit "
                 subreddit
                 "</h2>"
                 reddit-posts
                 "<br>")
         (concat "<h2>Could not find subreddit " subreddit "</h2>"))
       "")
     (if (not-empty reddit-user)
       (if (seq reddit-comments)
         (concat "<h2>Generated comments from reddit user "
                 reddit-user
                 "</h2>"
                 reddit-comments
                 "<br>")
         (concat "<h2>Could not find reddit user " reddit-user "</h2>"))))))

(defn handle-query [request]
  (-> {:status 200
      :header {"Content-Type" "text/html; charset=UTF-8"}
      :body (parse-query-string request)}))

(defn static-file
  "Return a static file"
  [filename]
  (response/resource-response filename {:root "public"}))

(defn main-handler [request]
  (let [uri (:uri request)]
    (-> (case uri
         ;; Index
         "/"           (static-file "index.html")
         "/index.html" (static-file "index.html")
         ;; Query handling
         "/query"      (handle-query request)
         "")
       (response/content-type "text/html;charset=UTF-8"))))

(def web-app
  (-> main-handler
     keyword-params/wrap-keyword-params
     params/wrap-params))

(defn -main []
  (jetty/run-jetty web-app
                   {:port (Integer. (or (System/getenv "PORT")
                                        "3000"))}))
