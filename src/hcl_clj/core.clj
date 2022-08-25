(ns hcl-clj.core
  (:require [hcl-clj.ast :as hcl-ast]
            [hcl-clj.lexer :as hcl-lexer]
            [hcl-clj.map :as hcl-map]))

(defn parse
  [^String s]
  (-> s
      (hcl-lexer/str->tokens)
      (hcl-ast/tokens->ast)
      (hcl-map/ast->map)))
