(ns markov-twit.server
  (:gen-class)
  (:require
   [ring.adapter.jetty :as jetty]
   [ring.util.response :as response]
   [ring.middleware.params :as params]
   [ring.middleware.keyword-params :as keyword-params]
   [markov-twit.generator :as generator]))

(defn parse-twitter [twitter-user chain-length]
  (if (not-empty twitter-user)
    (try
      (reduce concat
              (map (fn [tweet] (concat "<p>" tweet "</p>"))
                   (generator/generate-tweet twitter-user 5 280 chain-length)))
      (catch Exception e
        ""))
    ""))

(defn parse-reddit-posts [subreddit chain-length]
  (if (not-empty subreddit)
    (try
      (reduce concat (map concat
                          (repeat 5 "<p><h3>")
                          (generator/generate-reddit-title subreddit 5 350 chain-length)
                          (repeat 5 "</h3>")
                          (generator/generate-reddit-post  subreddit 5 600 chain-length)
                          (repeat 5 "</p>")))
      (catch Exception e
        ""))
    ""))

(defn parse-reddit-comments [reddit-user chain-length]
  (if (not-empty reddit-user)
    (try
      (reduce concat
              (map (fn [comment] (concat "<p>" comment "</p>"))
                   (generator/generate-reddit-comment reddit-user 5 500 chain-length)))
      (catch Exception e
        ""))
    ""))

(defn format-generated [from generated name-of-generated]
  (if (seq generated)
    (concat "<h2>Generated " name-of-generated " from "
            from
            "</h2>"
            generated
            "<br>")
    (concat "<h2>Could not find " from "</h2>")))

(defn parse-query-string [request]
  (let [params       (:params       request)
        reddit-user  (:reddit-user  params)
        subreddit    (:subreddit    params)
        twitter-user (:twitter-user params)
        chain-length (Integer. (or (:chain params)
                                   "2"))]
    (concat
     (if (not-empty twitter-user)
       (format-generated (concat "@" twitter-user)
                         (parse-twitter twitter-user chain-length)
                         "tweets")
       "")
     (if (not-empty subreddit)
       (format-generated (concat "/r/" subreddit)
                         (parse-reddit-posts subreddit chain-length)
                         "posts")
       "")
     (if (not-empty reddit-user)
       (format-generated (concat "/u/" reddit-user)
                         (parse-reddit-comments reddit-user chain-length)
                         "comments")
       ""))))

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
