(ns hcl-clj.lexer-test
  (:require [clojure.test :refer :all]
            [hcl-clj.lexer :refer :all]))

(deftest standard-tokens-test
  (testing "The expcted tokens are generated - basic number literal"
    (let [hcl
          "foo \"bar\" {
             baz = 123
             qux: 456
          }"
          expected '({:type :number-literal :line 2 :content 123}
                     {:type :number-literal :line 3 :content 456})]
      (is (= expected (filter #(= :number-literal (:type %))
                              (:tokens (str->tokens hcl))) ))))

  (testing "The expcted tokens are generated - basic boolean literal"
    (let [hcl
          "foo \"bar\" {
             baz = true,
             qux:false
          }"
          expected '({:type :boolean-literal :line 2 :content true}
                     {:type :boolean-literal :line 3 :content false})]
      (is (= expected (filter #(= :boolean-literal (:type %))
                              (:tokens (str->tokens hcl))) ))))

  (testing "The expcted tokens are generated - spaces in strings"
    (let [hcl
          "foo \"bar baz\" {
          }"
          expected {:type :string-literal :line 1 :content "bar baz"}]
      (is (= expected (nth (:tokens (str->tokens hcl)) 2) ))))

  (testing "The expcted tokens are generated - multiple roots"
    (let [hcl
          "foo \"bar\" {
          }
          baz \"qux\" {
          }"
          expected {:lines 4
                    :tokens [{:type :sof :line 1}
                             {:type :keyword :line 1 :content "foo"}
                             {:type :string-literal :line 1 :content "bar"}
                             {:type :scope-open, :line 1}
                             {:type :scope-close, :line 2}
                             {:type :keyword :line 3 :content "baz"}
                             {:type :string-literal :line 3 :content "qux"}
                             {:type :scope-open, :line 3}
                             {:type :scope-close, :line 4}
                             {:type :eof, :line 4}]}]
      (is (= expected (str->tokens hcl) )))))

(deftest weird-syntax-test
  (testing "no spaces"
    (let [hcl
          "foo \"bar\" {
             baz=123
             qux:456
          }"
          expected '({:type :number-literal :line 2 :content 123}
                     {:type :number-literal :line 3 :content 456})]
      (is (= expected (filter #(= :number-literal (:type %))
                              (:tokens (str->tokens hcl))) )))))
