(ns hcl-clj.ast-test
  (:require [clojure.test :refer :all]
            [hcl-clj.ast :refer :all]
            [hcl-clj.lexer :as lexer]))

(defn file
  [content]
  {:type :scope,
   :line 1,
   :scope-type :file,
   :content content})

(deftest ast-capture-scope-test
  (testing "We're able to correctly capture scope - basic"
    (let [hcl
          "foo \"bar\" {
             baz = 123
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected [[(file
                       [{:type :keyword, :line 1, :content "foo"}
                        {:type :string-literal, :line 1, :content "bar"}
                        {:type :scope,
                         :line 1,
                         :scope-type :block,
                         :content [{:type :keyword, :line 2, :content "baz"}
                                   {:type :assignment, :line 2}
                                   {:type :number-literal, :line 2, :content 123}]}])]
                    (inc (count tks))]]
      (is (= expected (capture-scope tks)))))

  (testing "We're able to correctly merge tokens and capture scope - nested"
    (let [hcl
          "foo \"bar\" {
             baz {
               qux = 123
             }
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected [[(file
                       [{:type :keyword, :line 1, :content "foo"}
                        {:type :string-literal, :line 1, :content "bar"}
                        {:type :scope,
                         :line 1,
                         :scope-type :block,
                         :content
                         [{:type :keyword, :line 2, :content "baz"}
                          {:type :scope,
                           :line 2,
                           :scope-type :block,
                           :content
                           [{:type :keyword, :line 3, :content "qux"}
                            {:type :assignment, :line 3}
                            {:type :number-literal, :line 3, :content 123}]}]}])]
                    (inc (count tks))]]
      (is (= expected (capture-scope tks)))))

  (testing "We're able to correctly merge tokens and capture scope - nested, double root"
    (let [hcl
          "foo \"bar\" {
             baz {
               qux = 123
             }
          }
          alice \"bob\" {
            charlie = \"danny\"
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected [[(file
                       [{:type :keyword, :line 1, :content "foo"}
                        {:type :string-literal, :line 1, :content "bar"}
                        {:type :scope,
                         :line 1,
                         :scope-type :block,
                         :content
                         [{:type :keyword, :line 2, :content "baz"}
                          {:type :scope,
                           :line 2,
                           :scope-type :block,
                           :content
                           [{:type :keyword, :line 3, :content "qux"}
                            {:type :assignment, :line 3}
                            {:type :number-literal, :line 3, :content 123}]}]}
                        {:type :keyword, :line 6, :content "alice"}
                        {:type :string-literal, :line 6, :content "bob"}
                        {:type :scope,
                         :line 6,
                         :scope-type :block,
                         :content
                         [{:type :keyword, :line 7, :content "charlie"}
                          {:type :assignment, :line 7}
                          {:type :string-literal, :line 7, :content "danny"}]}])]
                    (inc (count tks))]]
      (is (= expected (capture-scope tks))))))


(comment
  (def tks [{:type :sof, :line 1}
            {:type :keyword, :line 1, :content "foo"}
            {:type :string-literal, :line 1, :content "bar"}
            {:type :scope-open, :line 1}
            {:type :keyword, :line 2, :content "baz"}
            {:type :scope-open, :line 2}
            {:type :keyword, :line 3, :content "qux"}
            {:type :assignment, :line 3}
            {:type :number-literal, :line 3, :content 123}
            {:type :scope-close, :line 4}
            {:type :scope-close, :line 5}
            {:type :eof, :line 5}]))
