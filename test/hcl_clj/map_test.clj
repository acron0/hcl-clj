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
