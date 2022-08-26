(ns hcl-clj.lexer-test
  (:require [clojure.test :refer :all]
            [hcl-clj.lexer :refer :all]))

;; a lot of these were inspired by
;; https://github.com/bertramdev/hcl4j/blob/master/src/test/groovy/com/bertramlabs/plugins/hcl4j/HCLParserSpec.groovy

(deftest basic-literals
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
             bar = \"alice\"
             baz = 'bob'
             qux: \"charlie\"
          }"
          expected '({:type :string-literal :line 2 :content "alice"}
                     {:type :string-literal :line 3 :content "bob"}
                     {:type :string-literal :line 4 :content "charlie"})]
      (is (= expected (filter #(= :string-literal (:type %))
                              (:tokens (str->tokens hcl))) ))))

  (testing "The expcted tokens are generated - basic string literal with spaces"
    (let [hcl
          "foo {
             bar = \"alice with a lot of spaces\"
             baz = 'bob with a lot of spaces'
             qux: \"charlie with a lot of spaces\"
          }"
          expected '({:type :string-literal :line 2 :content "alice with a lot of spaces"}
                     {:type :string-literal :line 3 :content "bob with a lot of spaces"}
                     {:type :string-literal :line 4 :content "charlie with a lot of spaces"})]
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
                              (:tokens (str->tokens hcl))) )))))

(deftest string-tokens
  (testing "The expcted tokens are generated - spaces in strings that are IDs"
    (let [hcl
          "foo \"bar baz\" {
          }"
          expected {:type :string-literal :line 1 :content "bar baz"}]
      (is (= expected (nth (:tokens (str->tokens hcl)) 2)))))

  (testing "The expcted tokens are generated - multiple strings IDs"
    (let [hcl
          "foo \"bar\" \"baz\" {
          }"
          expected '({:type :string-literal :line 1 :content "bar"}
                     {:type :string-literal :line 1 :content "baz"})]
      (is (= expected (take 2 (drop 2 (:tokens (str->tokens hcl))))))))

  (testing "The expected tokens are generated - multiline string literal, no stripping"
    (let [hcl
          "foo {
             baz = <<EOL
                   This is a cool String
                   I love multiple lines
                   Don't you?
                   EOL
          }"
          expected '({:type :string-literal :line 2 :content "                   This is a cool String
                   I love multiple lines
                   Don't you?"})]
      (is (= expected (filter #(= :string-literal (:type %))
                              (:tokens (str->tokens hcl)))))))

  (testing "The expected tokens are generated - multiline string literal, with stripping"
    (let [hcl
          "foo {
             baz = <<-EOL
                   This is a cool String
                   I love multiple lines
                   Don't you?
                   EOL
          }"
          expected '({:type :string-literal :line 2 :content "This is a cool String
I love multiple lines
Don't you?"})]
      (is (= expected (filter #(= :string-literal (:type %))
                              (:tokens (str->tokens hcl)))))))

  (testing "The expcted tokens are generated - strange string literal"
    (let [s "~> 1.3.0 <foo> /bar/ \\baz\\"
          hcl (format "foo { baz = \"%s\" }" s)
          expected (list {:type :string-literal :line 1 :content s})]
      (is (= expected (filter #(= :string-literal (:type %))
                              (:tokens (str->tokens hcl))))))))

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
                              (:tokens (str->tokens hcl)))))))

  (testing "no spaces in blocks"
    (let [hcl
          "foo\"bar\"{}"
          expected '({:type :keyword :line 1 :content "foo"}
                     {:type :string-literal :line 1 :content "bar"})]
      (is (= expected (take 2 (drop 1 (:tokens (str->tokens hcl)))))))))

(deftest structural-tests
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
      (is (= expected (str->tokens hcl)))))

  (testing "Root level attributes"
    (let [hcl "foo = \"bar\""
          expected '({:type :keyword :line 1 :content "foo"}
                     {:type :assignment :line 1}
                     {:type :string-literal :line 1 :content "bar"})]
      (is (= expected (take 3 (drop 1 (:tokens (str->tokens hcl)))))))))

(deftest interpolation
  (testing "We can lex out interpolation literals - strings"
    (let [hcl "foo = \"${bar}\""
          expected '({:type :keyword :line 1 :content "foo"}
                     {:type :assignment :line 1}
                     {:type :interpolation-literal :line 1 :content "\"${bar}\""})]
      (is (= expected (take 3 (drop 1 (:tokens (str->tokens hcl))))))))
  (testing "We can lex out interpolation literals - raw"
    (let [hcl "foo = ${bar}"
          expected '({:type :keyword :line 1 :content "foo"}
                     {:type :assignment :line 1}
                     {:type :interpolation-literal :line 1 :content "${bar}"})]
      (is (= expected (take 3 (drop 1 (:tokens (str->tokens hcl)))))))))
