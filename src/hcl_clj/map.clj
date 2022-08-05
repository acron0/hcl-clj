(ns hcl-clj.map
  (:require [clojure.string :as str]))

(defn ->sequential [x] (if (sequential? x) x [x]))

;; https://gist.github.com/danielpcox/c70a8aa2c36766200a95
(defn deep-merge
  [a b]
  (if (map? a)
    (into a (for [[k v] b] [k (deep-merge (a k) v)]))
    b))

(declare collapse)

(defn process-node
  [a node]
  (if (:name node)
    ;; named nodes - scopes, kvps
    (let [names (str/split (:name node) #"\.")
          dst (hash-map (keyword (last names))
                        (condp = (:type node)
                          :scope           (collapse (:content node))
                          :key-value-pair  (collapse (->sequential (-> node :content)))))]
      (deep-merge a (reduce (fn [a' x] (hash-map (keyword x) a')) dst (reverse (butlast names)))))
    ;; unnamed nodes - literals, lists
    (let []

      (if (= :scope (:type node))
        (reduce (fn [a n] (conj a (process-node {} n))) [] (:content node))
        ;; literals
        (:content node)))))

(defn collapse
  [nodes]
  (reduce process-node {} nodes))

(defn ast->map
  [{:keys [content] :as ast}]
  ;; assert that we start with a file scope
  (assert (= :scope (:type ast)))
  (assert (= :file (:scope-type ast)))
  (collapse content))
