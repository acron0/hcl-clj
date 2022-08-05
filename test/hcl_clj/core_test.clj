(ns hcl-clj.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hcl-clj.core :as hcl-clj]))

(deftest basic-core-test
  (testing "Parsing the test file matches expected outcome - basic"
    (let [expected-outcome {:job
                            {:build
                             {:datacenters ["ap-southeast-1" "ap-southeast-2"]
                              :update {:stagger "30s"
                                       :max-parallel 1
                                       :immediately true}
                              :group {:load-balancers
                                      {:ratio 12.34
                                       :count 1
                                       :restart {:attempts 10}}}}}}]
      (is (= expected-outcome (hcl-clj/parse (slurp (io/resource "test.tf"))))))))

(deftest advanced-core-test
  (testing "Parsing the test file matches expected outcome - advanced"
    (let [expected-outcome {}]
      (is (= expected-outcome (hcl-clj/parse (slurp (io/resource "vmware.tf"))))))))

(comment
  (require '[hcl-clj.lexer :as hcl-lexer])
  (require '[hcl-clj.ast :as hcl-ast])
  (def foo (slurp (io/resource "test.tf")))
  (def tokens (hcl-lexer/str->tokens foo))
  (def ast (hcl-ast/tokens->ast tokens))
  (hcl-clj/parse (slurp (io/resource "test.tf"))))
