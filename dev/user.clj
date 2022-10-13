(ns user
  (:require [taoensso.timbre :as timbre]
            [clojure.tools.namespace.repl :as repl :refer [refresh]]))

(timbre/set-level! :info)
(comment
  (repl/clear)
  (refresh))
