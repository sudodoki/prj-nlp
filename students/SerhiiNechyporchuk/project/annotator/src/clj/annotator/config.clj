(ns annotator.config
  (:require [environ.core :refer [env]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [ring.middleware.logger :refer [wrap-with-logger]]))

(defn config []
  {:http-port  (Integer/parseInt (or (env :port) "10555"))
   :phrases-no (Integer/parseInt (or (env :phrases-np) "5"))
   :max-tasks (Integer/parseInt (or (env :max-tasks) "10"))
   :middleware [[wrap-defaults api-defaults]
                wrap-with-logger
                wrap-gzip]})
