(ns user
  (:require [taoensso.timbre :as timbre]
            [clojure.tools.namespace.repl :refer [refresh]]))

(timbre/set-level! :info)
(comment
  (refresh))
