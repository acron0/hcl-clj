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

  (testing "The expcted tokens are generated - basic string literal"
    (let [hcl
          "foo {
             baz = \"alice\"
             qux: \"bob\"
          }"
          expected '({:type :string-literal :line 2 :content "alice"}
                     {:type :string-literal :line 3 :content "bob"})]
      (is (= expected (filter #(= :string-literal (:type %))
                              (:tokens (str->tokens hcl))) ))))

  (testing "The expcted tokens are generated - basic string literal with spaces"
    (let [hcl
          "foo {
             baz = \"alice with a lot of spaces\"
             qux: \"bob with a lot of spaces\"
          }"
          expected '({:type :string-literal :line 2 :content "alice with a lot of spaces"}
                     {:type :string-literal :line 3 :content "bob with a lot of spaces"})]
      (is (= expected (filter #(= :string-literal (:type %))
                              (:tokens (str->tokens hcl))) ))))

  (testing "The expcted tokens are generated - strange string literal"
    (let [s "~> 1.3.0 <foo> /bar/ \\baz\\"
          hcl (format "foo { baz = \"%s\" }" s)
          expected (list {:type :string-literal :line 1 :content s})]
      (is (= expected (filter #(= :string-literal (:type %))
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

  (testing "The expcted tokens are generated - multiple strings"
    (let [hcl
          "foo \"bar\" \"baz\" {
          }"
          expected '({:type :string-literal :line 1 :content "bar"}
                     {:type :string-literal :line 1 :content "baz"})]
      (is (= expected (take 2 (drop 2 (:tokens (str->tokens hcl))))))))

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
  (testing "no spaces in assignments"
    (let [hcl
          "foo \"bar\" {
             baz=123
             qux:456
          }"
          expected '({:type :number-literal :line 2 :content 123}
                     {:type :number-literal :line 3 :content 456})]
      (is (= expected (filter #(= :number-literal (:type %))
                              (:tokens (str->tokens hcl))) ))))

  (testing "no spaces in blocks"
    (let [hcl
          "foo\"bar\"{}"
          expected '({:type :keyword :line 1 :content "foo"}
                     {:type :string-literal :line 1 :content "bar"})]
      (is (= expected (take 2 (drop 1 (:tokens (str->tokens hcl)))) )))))
