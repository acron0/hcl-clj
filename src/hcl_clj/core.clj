(ns hcl-clj.core
  (:require [clojure.java.io :as io]
            [clojure.walk :as walk])
  (:import [com.bertramlabs.plugins.hcl4j HCLParser]))

(defn- lhm->pam
  [m]
  (walk/prewalk
    (fn [x]
      (if (instance? java.util.LinkedHashMap x)
        (zipmap (map keyword (.keySet x)) (.values x))
        x))
    m))

(defn parse
  [^String s]
  (let [m (.parse (HCLParser.) s)]
    (lhm->pam m)))
