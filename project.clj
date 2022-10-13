(defproject tieminos "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.3.618"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/tools.namespace "1.2.0"]
                 [potemkin "0.4.5"]
                 [org.clojure/data.generators "1.0.0"]
                 [overtone "0.10.6"]
                 [time-time "0.1.0-SNAPSHOT"]
                 [erv "0.1.0-SNAPSHOT"]
                 [com.taoensso/timbre "4.10.0"]
                 [incanter "1.9.3"]
                 ;; working with intervals
                 [io.helins/interval "1.0.0-beta0"]
                 ;; another interesting possibility, may offer other options
                 #_[com.dean/interval-tree "0.1.2"]]
  :plugins [[com.github.clojure-lsp/lein-clojure-lsp "1.1.4"]]
  :jvm-opts ^:replace ["-XX:-OmitStackTraceInFastThrow"]
  :repl-options {:init-ns tieminos.core})
