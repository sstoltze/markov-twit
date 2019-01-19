(defproject markov-twit "0.1.0"
  :description "A small web service using a markov chain to generate tweets and reddit posts/comments."
  :url "https://github.com/sstoltze/markov-twit"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :min-lein-version "2.8.3"
  :dependencies [[cheshire "5.8.1"]
                 [clj-http "3.9.1"]
                 [environ "1.1.0"]
                 [ring "1.7.0"]
                 [twitter-api "1.8.0"]
                 [org.clojure/clojure "1.9.0"]]
  :plugins [[lein-environ "1.1.0"]]
  :main ^:skip-aot markov-twit.server
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :uberjar-name "markov-standalone-server.jar")
