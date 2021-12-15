(defproject advent-of-code-2021 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [lambdaisland/kaocha "1.60.945"]
                 [org.clojure/core.logic "1.0.0"]
                 [com.clojure-goes-fast/clj-async-profiler "0.5.1"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :repl-options {:init-ns advent-of-code-2021.core}
  :aliases {"kaocha" ["run" "-m" "kaocha.runner"]}
  :jvm-opts ["-Djdk.attach.allowAttachSelf"])
