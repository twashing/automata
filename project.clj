(defproject automata "0.2.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [com.rpl/specter "1.1.2"]]

  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.9"]
                                  [org.clojure/test.check "0.10.0-alpha3"]]
                   :source-paths ["src" "workbench"]}}

  :plugins [[lein-pprint "1.2.0"]]
  :repositories [["releases" {:url "https://clojars.org/repo"
                              :username :env
                              :sign-releases false
                              :password :env}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v" "--no-sign"] ; disable signing and add "v" prefix
                  ["deploy"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]])
