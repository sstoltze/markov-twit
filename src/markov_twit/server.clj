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
        tweets       (if twitter-user
                       (reduce concat
                               (map (fn [tweet] (concat "<p>" tweet "</p>"))
                                    (generator/generate-tweet twitter-user 5)))
                       "")
        reddit-posts (if subreddit
                       (reduce concat (map concat
                                           (repeat 5 "<p><h3>")
                                           (generator/generate-reddit-title subreddit 5 350)
                                           (repeat 5 "</h3>")
                                           (generator/generate-reddit-post  subreddit 5 600)
                                           (repeat 5 "</p>")))
                       "")
        reddit-comments (if reddit-user
                          (reduce concat
                                  (map (fn [comment] (concat "<p>" comment "</p>"))
                                       (generator/generate-reddit-comment reddit-user 5 500)))
                          "")]
    (concat (if twitter-user
              (concat "<h2>Generated tweets from @"
                      twitter-user
                      "</h2>"
                      tweets
                      "<br>")
              "")
            (if subreddit
              (concat "<h2>Generated posts from "
                      subreddit
                      "</h2>"
                      reddit-posts
                      "<br>")
              "")
            (if reddit-user
              (concat "<h2>Generated comments from "
                      reddit-user
                      "</h2>"
                      reddit-comments)))))

(defn handle-query [request]
  (-> {:status 200
      ;;:header {"Content-Type" "text/html; charset=UTF-8"}
      :body (parse-query-string request)}
     (response/charset "UTF-8")
     (response/content-type "text/html")))

(defn static-file
  "Return a static file"
  [filename]
  (response/resource-response filename {:root "public"}))

(defn main-handler [request]
  (let [uri (:uri request)]
    (case uri
      ;; Index
      "/"           (static-file "index.html")
      "/index.html" (static-file "index.html")
      ;; Query handling
      "/query"      (handle-query request))))

(def web-app
  (-> main-handler
     keyword-params/wrap-keyword-params
     params/wrap-params))

(defn -main []
  (jetty/run-jetty web-app
                   {:port 3019}))
