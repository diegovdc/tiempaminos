(ns user
  (:require [taoensso.timbre :as timbre]
            [clojure.tools.namespace.repl :as repl :refer [refresh set-refresh-dirs]]))

(timbre/set-level! :info)
(set-refresh-dirs "src")
(comment
  (repl/clear)
  ;; FIXME Ya casi funciona solo hay que arreglar el require en tieminos.compositions.garden-earth.synths.granular
  (refresh))
