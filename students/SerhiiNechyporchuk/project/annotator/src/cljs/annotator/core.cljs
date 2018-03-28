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
                          :show-ann-info   true
                          :show-tips true}))

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
   (let [resp (<! (call "create-job" {:annotator (:annotator-email @app-state)}))]
     (swap! app-state assoc
            :jobs (conj (:jobs @app-state) resp)
            :tasks (mapv #(update % :phrases vec) (:tasks resp))
            :job-id (:id resp)
            :task-no 0
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
     (swap! app-state assoc :jobs (vec (:jobs resp))))))

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

(defmethod handler :toggle-tips [_ {}]
  (swap! app-state update :show-tips not))

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
   #_(trace (->> (:search-attributes task) (group-by (fn [task] (first (str/split (:name task) #"\."))))))
   [:ul
    (->> (:search-attributes task)
         (group-by (fn [task] (first (str/split (:name task) #"\."))))
         (map (fn [[section tasks]]
                (if (com/top-level section)
                  (for [{:keys [short]} tasks]
                    [:li {:key short
                          :dangerouslySetInnerHTML {:__html short}} ])
                 [:li {:key section}
                  (or (com/section->human section)
                      [:b section])
                  [:ul
                   (for [{:keys [human short]} tasks]
                     [:li {:key short
                           :dangerouslySetInnerHTML {:__html (or short human)}} ])]]))))]])

(defn leaderboard [jobs]
  (->> jobs
       (map-indexed (fn [i job]
                      {:score (->> (:tasks job)
                                   (map #(count (filter seq (:phrases %))))
                                   (reduce +))
                       :annotator (:annotator job) :job-idx i}))
       (sort-by :score (comp - compare))))

(defn leaderboard-component []
  [:div
   [:h4 "Leaderboard" [:code {:style {:cursor :pointer}
                              :title "Total number of paraphrases"} "  ?"]]
   #_[:pre (trace (leaderboard (:jobs @app-state)))]
   [:table.table.table-striped
    [:thead
     [:tr
      [:th "№ of paraphrases"]
      [:th "Annotator"]
      [:th "Job №"]]]
    [:tbody
     (for [[i row] (take 10 (map-indexed vector (leaderboard (:jobs @app-state))))]
       [:tr {:style {:background-color (case i
                                         0 "rgba(255,215,0, 0.3)"
                                         1 "rgba(211,211,211, 0.3)"
                                         2 "rgba(80, 50, 20, 0.2)"
                                         "")}}
        [:td (:score row)]
        [:td
         (case i
                0 "\uD83E\uDD47"
                1 "\uD83E\uDD48"
                2 "\uD83E\uDD49"
                "")
         (:annotator row)]
        [:td (:job-idx row)]])]]]
  )


(defn annotation-main []
  (let [{:keys [task-no tasks annotator-email jobs job-id show-ann-info show-tips]} @app-state
        task (nth tasks task-no)
        job (some #(when (= (:id %) job-id) %) jobs)]
    [:div
     #_[:pre (trace tasks)]
     [:div.form-group.row
      [:div.col-sm-2
       [:button.btn.btn-light {:on-click #(handler :halt-annotation {})} "Back"]]]
     [:div.alert.alert-info
      [:h4 {:style    {:cursor :pointer}
            :on-click #(handler :toggle-info {})}
       (if show-ann-info "▼" "▶") " What you need to do" (if show-ann-info "" "...")]
      [:div.collapse
       {:class (when show-ann-info "show")}
       [:p "Your task is to write search queries to find some attendees. Attendees is participants of some events, that
            use awesome " [:code "Attendify"] " event apps for communication within event. This apps allows you post content, like, reply, participate in polls
            and many more. In the task below you will have some information about attendee you want to find. For example:"]
       [:pre
        [:ul
         [:li "wrote post"
          [:ul
           [:li "8 times"]]]
         [:li "replied"
          [:ul
           [:li "with 'My honesty is a pain, and I want to play hacky sack. A path towards creepy costumes, again.'"]]]
         [:li "has Burkina as location"]]]
       [:p "This means you have to write a query to search attendees from Burkina who wrote 8 posts and made a reply about 'pain'.
           Actually, it is a pretty valid query, so you fill in the following query into " [:code "Query #1"] ":"]
       [:pre "Find attendees from Burkina who wrote 8 posts and made reply about 'pain'"]
       [:p "In " [:code "Query #2"] " you have to write a query for the same information but paraphrased, e.g.:"]
       [:pre "Find attendees who posted 8 times, commented about 'pain' and are from Burkina"]
       [:p "Attributes in the task may be grouped:"]
       [:pre
        [:ul
         [:li "liked"
          [:ul
           [:li "post 'My job is debt free, and I want to pluck a cactus. Ridiculously good dinner dates, again.'"]
           [:li "Sherman Cooper's post"]]]
         [:li "wrote post"
          [:ul
           [:li "with Lynn Boyd's mention "]]]]]
       [:p "Here we have grouped " [:code "like"] " with 2 attributes. Try to combine them so they will describe the same
            post/like/reply/etc, e.g.:"]
       [:pre "Find profiles who liked post about 'debt' by Sherman Cooper or mentioned Lynn Boyd"]

       [:p "And so on. There is no limit on how many phrases you write, the more the better."]
       [:p.lead "Thanks for your contribution!"]]]

     [:div.alert.alert-info.collapse.show
      [:h4 {:style {:cursor :pointer}
            :on-click #(handler :toggle-tips {})}
       (if show-tips "▼" "▶") " Tips for annotation" (if show-tips "" "...")]
      [:div.collapse
       {:class (when show-tips "show")}
       [:ul
        [:li "All changes are saved after you press " [:code "<- Prev"] " or " [:code "-> Next"]]
        [:li "You can combine queries using " [:code "or/and"]]
        [:li "Do not stick to the words in the information. It's even better if you use different words:" [:br]
         [:code "Find attendees who rated page with type session => Find attendees who rated session"]]
        [:li "Provide as many phrases for each task as you can."]
        [:li "Try to write queries with the following structure:" [:br]
         [:code "['Find'/'Search all'/'etc'] [Adverbs/Adjectives] [attendees/contacts/profiles/etc] [who ...]"]]
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
        #_[:li "just open your imagination"]]]]

     [:div.row
      [:div.col-sm-3.col-lg-2
       [:h6 (str "Task " (inc task-no) " of " (count tasks)) ]]
      [:div.col-sm-2.col-lg-1
       [:h6 "Progress:"]]
      [:div.col-sm-7.col-lg-9
       [:div.progress
        [:div.progress-bar {:style {:width (str (:progress job) "%")}}
         (str (:progress job) "%")]]]]

     [:p]

     [:div.alert.alert-primary

      [:h4 "Write query to find attendees who"]
      (render-task task)
      ]
     [:form
      #_(prev-next-buttons (:progress job))
      (for [[i phrase] (map-indexed vector (:phrases task))]
        [:div.form-group.row {:key (str (:id task) "-" i)}
         [:label.col-sm-3.col-form-label {:for (str (:id task) "-" i)}
          "Query #" (inc i)]
         [:div.col-sm-9
          [:textarea.form-control
           {:key          (str (:id task) "-" i)
            :read-only    (= (:readonly job) "true")
            :id           (str (:id task) "-" i)
            :rows         2
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
   [:div.alert.alert-info
    [:h5 "Hi there!"]
    [:p "Paraqueries is a tool for paraphrasing search queries written in English.
         Here you will help us to collect as many paraphrased search queries as possible."]
    [:p "To start working " [:b "enter your name"] " and press " [:b "Create"] " or
         select previously created job in the list."]
    [:p [:b "Please, read carefully instructions on the job page."]]
    [:p [:b "There is an example job annotated by 'Example' user. You should definitely check it."]]]
   [:h3 "Create new job"]
   [:div.input-group
    [:input {:type        :text
             :class       "form-control"
             :placeholder "Enter your name"
             :on-change   #(handler :update-email-field {:text (-> % .-target .-value)})}]
    [:div.input-group-append
     [:button.btn.btn-primary {:on-click #(handler :create-job {})} "Create"]]]

   [:p]
   [:br
    ]

   [:div.row
    [:div.col-lg-4
     (leaderboard-component)]

    [:div.col-lg-8
     [:h4 "Select already created job"]
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
         [:tr {:key (:id job) :style {:cursor "pointer"} :on-click #(handler :load-job {:id (:id job)})}
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
             (:annotator job) " " (str (js/Date. (:created job)))]])]]]]])

(defn main-screen []
  (case (:screen @app-state)
    :start (start-screen)
    :annotation (annotation-main)))

(defn render []
  (reagent/render [main-screen] (js/document.getElementById "app")
                  (fn [_]
                    (handler :init {}))))
