(defproject my-bank-app-2 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :plugins [[lein-nsorg "0.3.0"]
            [lein-cljfmt "0.7.0"]]
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :repl-options {:init-ns my-bank-app-2.core}
  :aliases {"lint" ["do" ["cljfmt" "check"] ["cljfmt" "check" "project.clj"] ["nsorg"]]
            "lint-fix" ["do" ["cljfmt" "fix"] ["cljfmt" "fix" "project.clj"] ["nsorg" "--replace"]]}
  :main ^:skip-aot my-bank-app-2.core)
