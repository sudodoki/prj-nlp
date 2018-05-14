(ns annotator.application
  (:gen-class)
  (:require [com.stuartsierra.component :as component]
            [system.components.endpoint :refer [new-endpoint]]
            [system.components.handler :refer [new-handler]]
            [system.components.middleware :refer [new-middleware]]
            [system.components.jetty :refer [new-web-server]]
            [annotator.config :refer [config]]
            [annotator.routes :refer [home-routes]]))

(defn app-system [config]
  (component/system-map
   :annotation-component {}
   :routes (component/using
            (new-endpoint home-routes)
            [:annotation-component])
   :middleware (new-middleware {:middleware (:middleware config)})
   :handler    (-> (new-handler)
                   (component/using [:routes :middleware]))
   :http       (-> (new-web-server (:http-port config))
                   (component/using [:handler]))))

(defn -main [& _]
  (let [config (config)]
    (-> config
        app-system
        component/start)
    (println "Started annotator on" (str "http://localhost:" (:http-port config)))))
