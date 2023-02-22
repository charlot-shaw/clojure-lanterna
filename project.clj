(defproject babashka/clojure-lanterna "0.9.8"
  :description "A Clojure wrapper around the Lanterna terminal output library."
  :url "https://github.com/babashka/clojure-lanterna"
  :license {:name "LGPL"}
  :dependencies [[com.googlecode.lanterna/lanterna "3.0.3"]]
  :java-source-paths ["./java"]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.2-alpha1"]]}}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_user
                                    :password :env/clojars_pass
                                    :sign-releases false}]])
