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
          expected (file
                     [{:type :keyword, :line 1, :content "foo"}
                      {:type :string-literal, :line 1, :content "bar"}
                      {:type :scope,
                       :line 1,
                       :scope-type :block,
                       :content [{:type :keyword, :line 2, :content "baz"}
                                 {:type :assignment, :line 2}
                                 {:type :number-literal, :line 2, :content 123}]}])]
      (is (= expected (capture-scope tks)))))

  (testing "We're able to correctly merge tokens and capture scope - nested"
    (let [hcl
          "foo \"bar\" {
             baz {
               qux = 123
             }
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file
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
                          {:type :number-literal, :line 3, :content 123}]}]}])
          ]
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
          expected (file
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
                        {:type :string-literal, :line 7, :content "danny"}]}])
          ]
      (is (= expected (capture-scope tks))))))

(deftest ast-name-scope-test
  (testing "We can apply a name to a scope - basic"
    (let [hcl
          "foo {
             baz = 123
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo"
                           :content [{:type :keyword, :line 2, :content "baz"}
                                     {:type :assignment, :line 2}
                                     {:type :number-literal, :line 2, :content 123}]}])]
      (is (= expected (-> tks
                          (capture-scope)
                          (name-scope))))))
  ;;
  (testing "We can apply a name to a scope - basic 2"
    (let [hcl
          "foo \"bar\" \"lol\" {
             baz = 123
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo.bar.lol"
                           :content [{:type :keyword, :line 2, :content "baz"}
                                     {:type :assignment, :line 2}
                                     {:type :number-literal, :line 2, :content 123}]}])]
      (is (= expected (-> tks
                          (capture-scope)
                          (name-scope))))))
  ;;
  (testing "We can apply a name to a scope - basic 3 - dots in name"
    (let [hcl
          "foo \"bar\" \"lol\" \"aaa.bbb\" {
             baz = 123
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo.bar.lol.aaa.bbb" ;; TODO do we actually want this? it
                           :content [{:type :keyword, :line 2, :content "baz"}
                                     {:type :assignment, :line 2}
                                     {:type :number-literal, :line 2, :content 123}]}])]
      (is (= expected (-> tks
                          (capture-scope)
                          (name-scope))))))
  ;;
  (testing "We can apply a name to a scope - nested"
    (let [hcl
          "foo \"bar\" {
             baz {
               qux = 123
             }
           }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo.bar"
                           :content [{:type :scope,
                                      :line 2,
                                      :scope-type :block,
                                      :name "baz"
                                      :content [{:type :keyword, :line 3, :content "qux"}
                                                {:type :assignment, :line 3}
                                                {:type :number-literal, :line 3, :content 123}]}]}])]
      (is (= expected (-> tks
                          (capture-scope)
                          (name-scope)))))))

(deftest ast-assignments-test
  (testing "We can group assignment tokens - basic"
    (let [hcl
          "foo {
             bar = 123
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo"
                           :content [{:type :key-value-pair,
                                      :line 2,
                                      :name "bar"
                                      :content {:type :number-literal, :line 2, :content 123}}]}])]
      (is (= expected (tokens->ast {:tokens tks})))))
  ;;
  (testing "We can group assignment tokens - basic 2"
    (let [hcl
          "foo {
             bar = 123
             baz = 456,
             qux: 789
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo"
                           :content [{:type :key-value-pair,
                                      :line 2,
                                      :name "bar"
                                      :content {:type :number-literal, :line 2, :content 123}}
                                     {:type :key-value-pair,
                                      :line 3,
                                      :name "baz"
                                      :content {:type :number-literal, :line 3, :content 456}}
                                     {:type :key-value-pair,
                                      :line 4,
                                      :name "qux"
                                      :content {:type :number-literal, :line 4, :content 789}}]}])]
      (is (= expected (tokens->ast {:tokens tks}))))))

(deftest lists
  (testing "We can group assignment tokens - lists"
    (let [hcl
          "foo {
             bar = [1, 2, 3]
          }"
          tks (:tokens (lexer/str->tokens hcl))
          expected (file [{:type :scope,
                           :line 1,
                           :scope-type :block,
                           :name "foo"
                           :content [{:type :key-value-pair,
                                      :line 2,
                                      :name "bar"
                                      :content {:type :scope,
                                                :line 2,
                                                :scope-type :list
                                                :content [{:type :number-literal, :line 2, :content 1}
                                                          {:type :number-literal, :line 2, :content 2}
                                                          {:type :number-literal, :line 2, :content 3}]}}]}])]
      (is (= expected (tokens->ast {:tokens tks}))))))


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
