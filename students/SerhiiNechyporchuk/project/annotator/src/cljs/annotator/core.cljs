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
                          :annotator-email nil
                          :show-ann-info   true}))

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
            :tasks (mapv #(update % :phrases vec) (:tasks resp))
            :job-id (:id resp)
            :task-no task-no
            :screen :annotation))))

(defmethod handler :load-job
  [_ {:keys [id]}]
  (let [as @app-state
        job (some #(when (= (:id %) id) %) (:jobs as))]
    (swap! app-state assoc
           :tasks (mapv #(update % :phrases vec) (:tasks job))
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
         {:keys [task-no tasks jobs job-id]} as]
     (swap! app-state assoc
            :task-no (inc-task-no task-no (count tasks))
            :jobs (mapv (fn [job]
                          (if (= (:id job) job-id)
                            (assoc job :progress (com/job-progress (assoc job :tasks tasks)))
                            job))
                        jobs)))))

(defmethod handler :save-tasks-and-prev [_ {}]
  (go
   (let [as @app-state
         resp (<! (call "save-phrases" {:tasks (:tasks as)
                                        :job-id (:job-id as)}))
         {:keys [task-no tasks jobs job-id]} as]
     (swap! app-state assoc
            :task-no (dec-task-no task-no (count tasks))
            :jobs (mapv (fn [job]
                          (if (= (:id job) job-id)
                            (assoc job :progress (com/job-progress (assoc job :tasks tasks)))
                            job))
                        jobs)))))

(defmethod handler :halt-annotation
  [_ args]
  (swap! app-state assoc :screen :start)
  (handler :init {}))

(defmethod handler :update-phrase [_ {:keys [text task-no phrase-idx]}]
  (swap! app-state assoc-in [:tasks task-no :phrases phrase-idx] text))

(defmethod handler :update-email-field [_ {:keys [text]}]
  (swap! app-state assoc :annotator-email text))

(defmethod handler :add-empty-phrase [_ {:keys [task-no]}]
  (swap! app-state update-in [:tasks task-no :phrases] conj ""))

(defmethod handler :toggle-info [_ {}]
  (swap! app-state update :show-ann-info not))

(enable-console-print!)

(defn trace [o]
  (with-out-str (pp/pprint o)))

(defn prev-next-buttons [progress]
  [:div.form-group.row
   [:div.col-sm-12
    [:div.btn-toolbar
     [:div.btn-group.mr-2
      [:button.btn.btn-secondary {:type "button" :on-click #(handler :save-tasks-and-prev {})} "<- Prev"]]
     [:div.btn-group.mr-2
      [:button.btn.btn-primary {:type "button" :on-click #(handler :save-tasks-and-next {})} "-> Next"]]
     [:div.btn-group
      [:button.btn.btn-success {:type "button"
                                :on-click #(handler :halt-annotation {})
                                :disabled (< progress 100)} "Finish"]]]]])

(defn render-task [task]
  [:pre
   [:ul
    (->> (:search-attributes task)
         (group-by (fn [task] (first (str/split (:name task) #"\."))))
         (map (fn [[section tasks]]
                (if (= section "profile")
                  (for [{:keys [human]} tasks]
                    [:li {:key human} human])
                 [:li {:key section}
                  (com/section->human section)
                  [:ul
                   (for [{:keys [human short]} tasks]
                     [:li {:key human} (or short human)])]]))))]])


(defn annotation-main []
  (let [{:keys [task-no tasks annotator-email jobs job-id show-ann-info]} @app-state
        task (nth tasks task-no)
        job (some #(when (= (:id %) job-id) %) jobs)]
    [:div
     #_[:pre (trace tasks)]
     [:div.form-group.row
      [:div.col-sm-2
       [:button.btn.btn-light {:on-click #(handler :halt-annotation {})} "Back"]]]
     [:div.alert.alert-info.collapse.show
      [:h4 {:style {:cursor :pointer}
            :on-click #(handler :toggle-info {})}
       (if show-ann-info "▼" "▶") " What you need to do" (if show-ann-info "" "...")]
      [:div.collapse
       {:class (when show-ann-info "show")}
       [:p "Your task is to write search queries to find some attendees. In the section below you will have some information
           about attendees you want to find. For example:"]
       [:pre
        [:ul
         [:li "wrote post"
          [:ul
           [:li "8 times"]]]
         [:li "replied"
          [:ul
           [:li "with 'My honesty is a pain, and I want to play hacky sack. A path towards creepy costumes, again.'"]]]
         [:li "has Burkina as location"]]]
       [:p "This means you have to write query to search attendees from Burkina who wrote 8 posts and make reply about 'pain'.
           Actually, it is pretty valid query, so you fill in into " [:code "Query #1"] " following query:"]
       [:pre "Find attendees from Burkina who wrote 8 posts and make reply about 'pain'"]
       [:p "In the " [:code "Query #2"] " you have to write query for the same information but rephrased, e.g.:"]
       [:pre "Find attendees who posted 8 times, commented about 'pain' and is from Burkina"]
       [:p "Attributes in the task may grouped:"]
       [:pre
        [:ul
         [:li "liked"
          [:ul
           [:li "post 'My job is debt free, and I want to pluck a cactus. Ridiculously good dinner dates, again.'"]
           [:li "Sherman Cooper's post"]]]
         [:li "wrote post"
          [:ul
           [:li "with Lynn Boyd's mention "]]]]]
       [:p "Here we have group like with 2 attributes. Try to combine them so they will describe the same
            post/like/reply/etc, e.g.:"]
       [:pre "Find profiles who liked post about 'debt' by Sherman Cooper or mentioned Lynn Boyd"]

       [:p "And so on. There is no limit on how many phrases you write, the more the better."]
       [:h4 "Tips for annotation"]
       [:p
        [:ul
         [:li "All changes are saved after you press " [:code "<- Prev"] " or " [:code "-> Next"]]
         [:li "You can combine queries using " [:code "or/and"]]
         [:li "Provide as many phrases for each task as you can."]
         [:li "Try to write queries with following structure:" [:br]
          [:code "['Find'/'Search all'/'etc'] [Adverbs/Adjectives] [attendees/contacts/profiles/etc] [who ...]"]]
         [:li "You can omit article and auxilary verbs " [:code "(is, are, etc, ...)"]]
         [:li "If you add to many empty phrases, just leave them empty"]
         [:li "Use force"]
         #_[:li "Please, follow Enghlish grammar whenever possible"]
         #_[:li "Ты можешь комбинировать любой аттрибут с любым другим"]
         #_[:li "Лучше не делать запросы с боллее чем 5 аттрибутами, т.к. они будут очень сложны в понимании, но не злоупотребляй and/or"]
         #_[:li "Следуйте английским правилам граммматики насколько это возможность"]
         #_[:li "можно опускать артыкли и aux verbs"]
         #_[:li "Необходимо чтобы каждая фраза имела структуру '[Find/Search all/etc] [ADV/ADJ]* [attendees/contacts/etc]"]
         #_[:li "используй любые аттрибуты что находятся в таблице, но всегда следуй здравому смыслу"]
         #_[:li "если ты пишешь запрос во вхождению какой-то фразы, заверны ее в кавычки"]
         #_[:li "just open your imagination"]]]
       [:p.lead "Thanks for what you doing!"]]]
     [:div.row
      [:div.col-sm-2
       [:h6 (str "Task " (inc task-no) " of " (count tasks)) ]]
      [:div.col-sm-1
       [:h6 "Progress:"]]
      [:div.col-sm-9
       [:div.progress
        [:div.progress-bar {:style {:width (str (:progress job) "%")}}
         (str (:progress job) "%")]]]]

     [:p]

     [:p.alert.alert-secondary

      [:h4 "Write query to find attendees who"]
      (render-task task)
      ]
     [:form
      #_(prev-next-buttons (:progress job))
      (for [[i phrase] (map-indexed vector (:phrases task))]
        [:div.form-group.row {:key (str (:id task) "-" i)}
         [:label.col-sm-2.col-form-label {:for (str (:id task) "-" i)}
          "Query #" (inc i)]
         [:div.col-sm-10
          [:textarea.form-control
           {:key          (str (:id task) "-" i)
            :id           (str (:id task) "-" i)
            :rows         1
            :defaultValue phrase
            :on-change    #(handler :update-phrase {:text       (-> % .-target .-value)
                                                    :task-no    task-no
                                                    :phrase-idx i})}]]])
      [:div.form-group.row
       [:div.col-sm-12
        [:button.btn.btn-light {:type "button" :on-click #(handler :add-empty-phrase {:task-no task-no})} "Add phrase"]]]
      (prev-next-buttons (:progress job))]]))

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
      [:th {:scope "col"} "Progress"]]]
    [:tbody
     (for [[i job] (map-indexed vector (:jobs @app-state))]
       [:tr  {:key (:id job) :style {:cursor "pointer"} :on-click #(handler :load-job {:id (:id job)})}
        [:th {:scope "row"} (str i)]
        [:td (:annotator job)]
        [:td (fmt/unparse (fmt/formatters :mysql) (coerce/from-long (:created job)))]
        [:td (fmt/unparse (fmt/formatters :mysql) (coerce/from-long (:modified job)))]
        [:td (let [p (:progress job)]
               (if (= 100 p)
                [:span.badge.badge-success "FINISHED"]
                [:span.badge.badge-primary (str p "%")]))]]
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
