(defproject erv-fib-synth "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.0-alpha3"]
                 [org.clojure/core.async "1.3.618"]
                 [potemkin "0.4.5"]
                 [org.clojure/data.generators "1.0.0"]
                 [overtone "0.10.6"]
                 [time-time "0.1.0-SNAPSHOT"]
                 [erv "0.1.0-SNAPSHOT"]
                 [com.taoensso/timbre "4.10.0"]]
  :jvm-opts ^:replace ["-XX:-OmitStackTraceInFastThrow"]
  :repl-options {:init-ns erv-fib-synth.core})
