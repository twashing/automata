(defproject automata "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.10.0-alpha3"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.trace "0.7.9"]]
                   :source-paths ["src" "workbench"]}})
