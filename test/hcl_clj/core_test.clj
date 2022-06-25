(ns hcl-clj.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hcl-clj.core :as hcl-clj]))

(def expected-outcome
  {:job
   {:build
    {:datacenters ["ap-southeast-2"]
     :update {:stagger "30s"
              :max-parallel 1.0}
     :group {:load-balancers
             {:count 1.0
              :restart {:attempts 10.0}}}}}})

(deftest test
  (testing "Parsing the test file matches expected outcome"
    (is (= expected-outcome (hcl-clj/parse* (slurp (io/resource "test.tf")))))))
