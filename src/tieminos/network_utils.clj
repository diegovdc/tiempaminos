(ns tieminos.network-utils)

(defn get-local-host []
  (.getHostAddress (java.net.InetAddress/getLocalHost)))

(defn local-addresses
  "IP will show up in the list"
  []
  (->> (java.net.NetworkInterface/getNetworkInterfaces)
       enumeration-seq
       (map bean)
       (filter (complement :loopback))
       (mapcat :interfaceAddresses)
       (map #(.. % (getAddress) (getHostAddress)))))
