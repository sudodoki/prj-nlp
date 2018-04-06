(ns aggreement.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [clj-fuzzy.metrics :refer [levenshtein]]))

(defn parse-correction [corr-str]
  (let [[_ pos-str rst] (re-find #"A ([^\|]*)\|\|\|(.*)" corr-str)
        [start end] (map #(Integer/parseInt %) (str/split pos-str #" "))
        [type replacement _ _ annotator] (str/split rst #"\|\|\|")]
    {:start       start
     :end         end
     :type        type
     :replacement (str/split replacement #" ")
     :annotator   annotator}))

(defn apply-correction [sent correction offset]
  (let [{:keys [start end replacement]} correction
        sent (vec sent)]
    (->> (concat (subvec sent 0 (+ start offset))
                 (if (empty? replacement)
                   []
                   replacement)
                 (subvec sent (+ end offset) (count sent)))
         (filter seq))))

(defn calc-offset [{:keys [start end replacement]}]
  (let [orig-len (- end start)
        repl-len (count (filter seq replacement))]
    (- repl-len orig-len)))

(defn apply-corrections [orig-sent corrections]
  (:sent (reduce (fn [{:keys [sent offset]} correction]
                   (if (= (:type correction) "noop")
                     {:sent sent
                      :offset offset}
                     {:sent   (apply-correction sent correction offset)
                      :offset (+ offset (calc-offset correction))}))
                 {:sent   orig-sent
                  :offset 0}
                 corrections)))

(defn similarity
  ([] 1)
  ([_] 0)
  ([sent1 sent2]
   (let [c1 (count sent1)
         c2 (count sent2)]
     (cond
       (and (zero? c1) (zero? c2)) 1
       (or (zero? c1) (zero? c2)) 0
       :else (double
              (- 1 (/ (levenshtein (str/join " " sent1) (str/join " " sent2))
                      (max c1 c2))))))))

(defn mean [xs]
  (double (/ (reduce + xs) (count xs))))

(defn evaluate-corrections [{:keys [sent corrections]}]
  (->> corrections
       (map #(apply-corrections sent %))
       (map #(str/join " " %))
       (apply similarity)))

(defn aggreement [m2-file]
  (with-open [f (io/reader m2-file)]
    (->> (line-seq f)
         (take 100)
         (partition-by empty?)
         (filter #(some seq %))
         (map-indexed (fn [i [sent & annotations]]
                        {:sent        (rest (str/split sent #" "))
                         :corrections (->> annotations
                                           (map parse-correction)
                                           (sort-by :annotator)
                                           (partition-by :annotator))}))
         (map evaluate-corrections)
         (mean))))

(if-let [m2-file (first *command-line-args*)]
  (println (format "Calculated agreement between anotators is %.1f%%" (* 100 (aggreement m2-file))))
  (println "Usage: clj agreement.clj <path to m2 COMBINED file>"))


