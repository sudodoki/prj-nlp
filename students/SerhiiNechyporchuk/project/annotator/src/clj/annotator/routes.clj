(ns annotator.routes
  (:require [clojure.java.io :as io]
            [compojure.core :refer [ANY GET PUT POST DELETE routes]]
            [compojure.route :refer [resources]]
            [ring.util.response :refer [response]]
            [cheshire.core :as json]
            [annotator.common :as com]
            [annotator.config :as config]
            [clojure.pprint :as pp]
            [clojure.java.jdbc :as jdbc]
            [clojure.string :as str]
            [bigml.sampling.simple :as ssample])
  (:import (java.util UUID Date)
           (java.sql Timestamp)))

;; todo
;; * sampling with no duplicates

(defn interpolate [sas]
  (->> sas
       (map (fn [{:keys [human short] :as attrs}]
              (let [#_#_human' (reduce (fn [s [nm vs]]
                                     (str/replace s (str "{" (name nm) "}") (rand-nth vs)))
                                   human
                                   com/values)
                    short' (when short
                             (reduce (fn [s [nm vs]]
                                       (str/replace s (str "{" (name nm) "}") (rand-nth vs)))
                                     short
                                     com/values))]

                (assoc attrs #_#_ :human human'
                       :short short'))))))

(defn sample-attributes []
  (let [intp-search-attrs (interpolate com/search-attributes)]
    (->> (range (:max-tasks (config/config)))
         (map (fn [_]
                (take 3 (ssample/sample intp-search-attrs)))))
    #_(repeatedly number-of-tasks (fn [] (repeatedly 3 #(rand-nth intp-search-attrs))))))


(defn rand-id []
  (str (UUID/randomUUID)))

(def db-spec {:dbtype "sqlite"
              :dbname "db"})

(jdbc/db-do-commands
 db-spec
 [(jdbc/create-table-ddl :jobs
                         [[:id "varchar(36)" "not null"]
                          [:annotator "varchar(50)" "not null"]
                          [:created "timestamp with timezone" "not null"]
                          [:modified "timestamp with timezone" "not null"]
                          [:readonly "bool" "not null default false"]
                          [:tasks "text" "not null"]]
                         {:conditional? true})])

(comment
 (jdbc/db-do-commands
  db-spec
  [(jdbc/drop-table-ddl :jobs {:conditional? true})])
 )

(defn now []
  (Timestamp. (.getTime (Date.))))

(def number-of-tasks 20)
(def max-phrases 2)

(defmulti handler (fn [components method args] method))

(defmethod handler "create-job"
  [components _ {:keys [annotator]}]
  (let [tasks (->> (if (= annotator "rootroot")
                     [(interpolate com/search-attributes)]
                     (sample-attributes))
                   (map #(-> {:search-attributes %
                              :id (rand-id)
                              :phrases (repeat (:phrases-no (config/config)) "")})))
        job {:id        (rand-id)
             :annotator annotator
             :created   (now)
             :modified  (now)
             :tasks     (json/generate-string tasks)}]
    (jdbc/insert! db-spec :jobs job)
    ;; todo: save to db
    (assoc job :tasks tasks)))

(defmethod handler "save-phrases"
  [components _ {:keys [job-id tasks]}]
  (jdbc/update! db-spec :jobs {:tasks (json/generate-string tasks)
                               :modified (now)}
                ["id = ?" job-id])
  "ok")

(defmethod handler "list-jobs"
  [comps _ args]
  {:jobs (->> (jdbc/query db-spec ["select * from jobs order by created asc"])
              (map #(update % :tasks json/parse-string true))
              (map #(assoc % :progress (com/job-progress %))))})

(defn call [components {:keys [body]}]
  (let [{:keys [args method]} (json/parse-stream (io/reader body) true)]
    {:body (json/generate-string (handler components method args))}))

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
