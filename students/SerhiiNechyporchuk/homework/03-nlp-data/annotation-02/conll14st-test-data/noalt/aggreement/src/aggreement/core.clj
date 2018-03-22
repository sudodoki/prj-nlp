(ns aggreement.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clj-fuzzy.metrics :refer [levenshtein]]))

(defn parse-correction [corr-str]
  (let [[_ pos-str rst] (re-find #"A ([^\|]*)\|\|\|(.*)" corr-str)
        [start end] (map #(Integer/parseInt %) (str/split pos-str #" "))
        [type replacement _ _ _] (str/split rst #"\|\|\|")]
    {:start       start
     :end         end
     :type        type
     :replacement replacement}))

(defn same-correction? [& corrs]
  (apply = corrs))

(defn trace [_] (println _) _)

(defn apply-correction [sent correction]
  (let [{:keys [start end replacement]} correction
        sent (vec sent)]
    (->> (concat (subvec sent 0 start)
                 (if (empty? replacement)
                   []
                   [replacement])
                 (subvec sent end (count sent)))
         (filter seq))))

(defn apply-corrections [corrections]

  (reduce (fn [tokenized-sent correction]
            (apply-correction tokenized-sent correction))
          (:sent corrections)
          (:corrections corrections)))

(defn similarity [tokenized1 tokenized2]
  (pprint [(str/join " " tokenized1) (str/join " " tokenized2)])
  (double
   (- 1
      (/ (levenshtein (str/join " " tokenized1)
                      (str/join " " tokenized2))
       (max (count tokenized1) (count tokenized2))))))

(defn compare-corrections [corrs]
  (pprint corrs)
  (->> corrs
       (map (fn [tuple]
              (trace (:sent (first tuple)))
              (trace (->> (map apply-corrections tuple)
                          (apply similarity)))
              ))))

(defn accuracy [bin-list]
  (double
   (/ (count (filter true? bin-list))
      (count bin-list))))

(defn mean [xs]
  (double (/ (reduce + xs)
             (count xs))))

(defn parse-corrections [& lss]
  (->> lss
       (map (fn [ls]
              (->> ls
                   (take 300)
                   (partition-by empty?)
                   (filter #(some seq %))
                   (map (fn [[sent & sent-corrections]]
                          {:sent        (rest (str/split sent #" "))
                           :corrections (mapv parse-correction sent-corrections)})))))
       (apply map vector)))

(with-open [f1 (io/reader "official-2014.0.m2")
            f2 (io/reader "official-2014.1.m2")]
  (->> (parse-corrections (line-seq f1) (line-seq f2))
       (compare-corrections)
       mean
       pprint
       doall))