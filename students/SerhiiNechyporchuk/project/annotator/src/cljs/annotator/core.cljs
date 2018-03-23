(ns annotator.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [clojure.string :as str]))

(defonce app-state (atom {:text "Hello Chestnut!"
                          :screen :start
                          :search-attributes []
                          :phrases []}))

(defn call [method args]
  (go (let [res (<! (http/post "/call" {:json-params {:method method :args args}}))]
        (-> (:body res)
            js/JSON.parse
            (js->clj :keywordize-keys true)))))


(defmulti handler (fn [method args] method))

(defmethod handler :init-annotation
  [_ args]
  (go
   (let [resp (<! (call "init-annotation" {}))]
     (prn resp)
     (swap! app-state assoc
            :search-attributes (:search-attributes resp)
            :screen :annotation
            :phrases [""]))))

(defmethod handler :save-phrases-and-next
  [_ ])

(defmethod handler :halt-annotation
  [_ args]
  (swap! app-state assoc :screen :start))

(defmethod handler :update-textarea [_ {:keys [text i]}]
  (swap! app-state assoc-in [:phrases i] text)
  true)

(enable-console-print!)

(defn annotation-main []
  [:div
   [:button {:on-click #(handler :halt-annotation {})} "Back"]
   [:pre (str/join "\n" (:search-attributes @app-state))]
   (for [[i phrase] (map-indexed vector (:phrases @app-state))]
     [:div {:key (hash phrase)}
      [:textarea {:on-change #(handler :update-textarea {:text (-> % .-target .-value)
                                                            :i    i})
                  :defaultValue       phrase}
       ]])
   [:button {:on-click #(handler :save-phrases-and-next {})} "Save"]])

(defn start-screen []
  [:button {:on-click #(handler :init-annotation {})} "Start"])
                                                                      f
(defn main-screen []
  (case (:screen @app-state)
    :start (start-screen)
    :annotation (annotation-main)))

(defn render []
  (reagent/render [main-screen] (js/document.getElementById "app")))
