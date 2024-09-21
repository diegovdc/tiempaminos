(ns tieminos.network-utils)

(defn get-lan-ip []
  (let [interfaces (java.net.NetworkInterface/getNetworkInterfaces)]
    (loop [ifaces (enumeration-seq interfaces)]
      (if (empty? ifaces)
        nil
        (let [iface (first ifaces)
              addresses (enumeration-seq (.getInetAddresses iface))]
          (if-let [addr (some #(when (and (not (.isLoopbackAddress %))
                                          (.isSiteLocalAddress %))
                                 (.getHostAddress %))
                              addresses)]
            addr
            (recur (rest ifaces))))))))

(defn get-local-host []
                                        ; returns 127.0.0.1
  #_(.getHostAddress (java.net.InetAddress/getLocalHost))
  (get-lan-ip))

(comment
  (get-lan-ip))
