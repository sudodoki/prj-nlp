(ns annotator.routes
  (:require [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [response]]
            [clojure.data.json :as json]
            [annotator.common :as com]))

(defmulti handler (fn [components method args] method))

(defmethod handler "init-annotation"
  [components _ args]
  (let [N (rand-nth com/number-of-attributes)]
    {:search-attributes (repeatedly N #(rand-nth com/search-attributes))}))

(defn call [components {:keys [body]}]
  (let [{:keys [args method]} (json/read (io/reader body) :key-fn keyword)]
    {:body (json/write-str (handler components method args))}))

(defn home-routes [endpoint]
  (routes
   (GET "/" _
     (-> "public/index.html"
         io/resource
         io/input-stream
         response
         (assoc :headers {"Content-Type" "text/html; charset=utf-8"})))
   (POST "/call" req (call endpoint req))
   (resources "/")))
