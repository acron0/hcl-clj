(ns hcl-clj.map-test
  (:require [clojure.test :refer :all]
            [hcl-clj.ast :as ast]
            [hcl-clj.lexer :as lexer]
            [hcl-clj.map :refer :all]))

(deftest basic-map-test
  (testing "We're able to create a map from a basic ast"
    (let [hcl
          "foo \"bar\" {
             baz = 123
          }"
          expected {:foo
                    {:bar
                     {:baz 123}}}]
      (is (= expected (-> hcl
                          (lexer/str->tokens)
                          (ast/tokens->ast)
                          (ast->map)))))))

(deftest shared-name-test
  (testing "We're able to produce a valid map when names are shared - 2 deep"
    (let [hcl
          "foo \"bar\" {
             abc = 123
          }
          foo \"baz\" {
             def = 456
          }"
          expected {:foo
                    {:bar
                     {:abc 123}
                     :baz
                     {:def 456}}}]
      (is (= expected (-> hcl
                          (lexer/str->tokens)
                          (ast/tokens->ast)
                          (ast->map))))))
  (testing "We're able to produce a valid map when names are shared - 3 deep"
    (let [hcl
          "foo \"bar\" \"baz\"{
             abc = 123
          }
          foo \"bar\" \"qux\"{
             def = 456
          }
          foo \"baz\"{
             xyz = 789
          }"

          expected {:foo
                    {:bar
                     {:baz {:abc 123}
                      :qux {:def 456}}
                     :baz {:xyz 789}}}]
      (is (= expected (-> hcl
                          (lexer/str->tokens)
                          (ast/tokens->ast)
                          (ast->map)))))))
