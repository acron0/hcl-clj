(ns hcl-clj.core
  (:require [hcl-clj.ast :as hcl-ast]
            [hcl-clj.lexer :as hcl-lexer]))

(defn parse
  [^String s]
  (-> s
      (hcl-lexer/str->tokens)
      (hcl-ast/tokens->ast)))
