(ns annotator.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [clojure.string :as str]
            [cljs.pprint :as pp]
            [cljs-time.format :as fmt]
            [cljs-time.coerce :as coerce]
            [goog.string :as gstring]
            [goog.string.format]
            [annotator.common :as com]))

(defonce app-state (atom {:screen          :start
                          :jobs            []
                          :job-id          nil
                          :tasks           []
                          :task-no         nil
                          :phrases         []
                          :annotator-email nil}))

(defn call [method args]
  (go (let [res (<! (http/post "/call" {:json-params {:method method :args args}}))]
        (-> (:body res)
            js/JSON.parse
            (js->clj :keywordize-keys true)))))

(def max-phrases 2)

(defmulti handler (fn [method args] method))

(defmethod handler :create-job
  [_ args]
  (go
   (let [resp (<! (call "create-job" {:annotator (:annotator-email @app-state)}))
         task-no (or (:task-no @app-state) 0)]
     (swap! app-state assoc
            :tasks (:tasks resp)
            :job-id (:id resp)
            :task-no task-no
            :screen :annotation))))

(defmethod handler :load-job
  [_ {:keys [id]}]
  (let [as @app-state
        job (some #(when (= (:id %) id) %) (:jobs as))]
    (swap! app-state assoc
           :tasks (:tasks job)
           :job-id id
           :task-no 0
           :screen :annotation)))

(defn inc-task-no [curr total]
  (mod (inc curr) total))

(defn dec-task-no [curr total]
  (mod (dec curr) total))

(defmethod handler :init [_ {}]
  (go
   (let [resp (<! (call "list-jobs" {}))]
     (swap! app-state assoc :jobs (:jobs resp)))))

(defmethod handler :save-tasks-and-next [_ {}]
  (go
   (let [as @app-state
         resp (<! (call "save-phrases" {:tasks (:tasks as)
                                        :job-id (:job-id as)}))
         {:keys [task-no tasks]} as]
     (swap! app-state assoc :task-no (inc-task-no task-no (count tasks))))))

(defmethod handler :save-tasks-and-prev [_ {}]
  (go
   (let [as @app-state
         resp (<! (call "save-phrases" {:tasks (:tasks as)
                                        :job-id (:job-id as)}))
         {:keys [task-no tasks]} as]
     (swap! app-state assoc :task-no (dec-task-no task-no (count tasks))))))

(defmethod handler :halt-annotation
  [_ args]
  (swap! app-state assoc :screen :start)
  (handler :init {}))

(defmethod handler :update-phrase [_ {:keys [text task-no phrase-idx]}]
  (swap! app-state assoc-in [:tasks task-no :phrases phrase-idx] text))

(defmethod handler :update-email-field [_ {:keys [text]}]
  (swap! app-state assoc :annotator-email text))

(defmethod handler :add-empty-phrase [_ {:keys [task-no]}]
  (swap! app-state update-in [:tasks task-no :phrases] concat [""]))

(enable-console-print!)

(defn trace [o]
  (with-out-str (pp/pprint o)))

(defn annotation-main []
  (let [{:keys [task-no tasks annotator-email]} @app-state
        task (nth tasks task-no)]
    [:div
     [:div.form-row
      [:button.btn.btn-light {:on-click #(handler :halt-annotation {})} "Back"]
      ]
     [:h6 "You have to find attendee who"]
     [:pre
      (map (fn [{:keys [name human]}]
             [:li human
              #_(->> com/search-attributes
                       (some #(when (= (:name %) name) %))
                       :human)])
           (:search-attributes task))]
     [:div.btn-toolbar
      [:div.btn-group.mr-2
       [:button.btn.btn-primary {:type "button" :on-click #(handler :save-tasks-and-prev {})} "Save & Prev"]]
      [:div.btn-group
       [:button.btn.btn-primary {:type "button" :on-click #(handler :save-tasks-and-next {})} "Save & Next"]]]
     [:form
      (for [[i phrase] (map-indexed vector (:phrases task))]
        [:div.form-group.row
         [:label.col-sm-2.col-form-label {:for (str (:id task) "-" i)}
          "Phrase #" (inc i)]
         [:div.col-sm-10
          [:textarea.form-control
           {:key          (str (:id task) "-" i)
            :id           (str (:id task) "-" i)
            :defaultValue phrase
            :on-change    #(handler :update-phrase {:text       (-> % .-target .-value)
                                                    :task-no    task-no
                                                    :phrase-idx i})}]]])]
     [:button.btn.btn-light {:on-click #(handler :add-empty-phrase {:task-no task-no})} "Add phrase"]
     [:div.btn-toolbar
      [:div.btn-group.mr-2
       [:button.btn.btn-primary {:type "button" :on-click #(handler :save-tasks-and-prev {})} "Save & Prev"]]
      [:div.btn-group
       [:button.btn.btn-primary {:type "button" :on-click #(handler :save-tasks-and-next {})} "Save & Next"]]]]))

(defn job->status [job]
  (let [filled (count (filter (fn [task] (some seq (:phrases task))) (:tasks job)))]
    (gstring/format "%.1f%%" (float (* 100 (/ filled (count (:tasks job))))))))

(defn start-screen []
  [:div
   [:h3 "Create new job"]
   [:div.input-group
    [:input {:type        :text
             :class       "form-control"
             :placeholder "Enter your name"
             :on-change   #(handler :update-email-field {:text (-> % .-target .-value)})}]
    [:div.input-group-append
     [:button.btn.btn-primary {:on-click #(handler :create-job {})} "Create"]]]
   [:h3 "Select already created job"]
   [:table.table.table-hover
    [:thead
     [:tr
      [:th {:scope "col"} "#"]
      [:th {:scope "col"} "Annotator"]
      [:th {:scope "col"} "Created at"]
      [:th {:scope "col"} "Modified at"]
      [:th {:scope "col"} "Status"]]]
    [:tbody
     (for [[i job] (map-indexed vector (:jobs @app-state))]
       [:tr {:style {:cursor "pointer"} :on-click #(handler :load-job {:id (:id job)})}
        [:th {:scope "row"} (str i)]
        [:td (:annotator job)]
        [:td (fmt/unparse (fmt/formatters :mysql) (coerce/from-long (:created job)))]
        [:td (fmt/unparse (fmt/formatters :mysql) (coerce/from-long (:modified job)))]
        [:td (job->status job)]]
       #_[:li {:key (:id job)}
        [:a {:href     "javascript:void(0)"
             :on-click #(handler :load-job {:id (:id job)})}
         (:annotator job) " " (str (js/Date. (:created job)))]])]]])

(defn main-screen []
  (case (:screen @app-state)
    :start (start-screen)
    :annotation (annotation-main)))

(defn render []
  (reagent/render [main-screen] (js/document.getElementById "app")
                  (fn [_]
                    (handler :init {}))))
