(ns hcl-clj.lexer
  (:require [clojure.string :as str]))

(def internal-newline-substitute "__<newline>__")
(def internal-newline-substitute-token-type :newline)

;;

(def tokens
  ;; order is important
  ;; token first items can be string matches OR predicates
  ;; NB predicatess should be after string matches
  [["{" :scope-open]
   ["}" :scope-close]
   ["[" :list-open]
   ["]" :list-close]
   ["=" :assignment]
   [internal-newline-substitute internal-newline-substitute-token-type]
   ;;
   [(partial re-find #"\".*\"") :string-literal]
   [(partial re-find #"[0-9\.]+") :number-literal]
   ;;
   [string? :keyword]])

(defn token
  ([t l]
   {:type t
    :line l})
  ([t l content]
   {:type t
    :line l
    :content content}))

(defn eof-token
[l]
(token :eof l))

(def empty-token-container
{:line    1
 :tokens  []})

;;

(defn lexeme->token-type
[lex]
(some (fn [[m-or-p token-type]]
        (when
            (or
              (and (string? m-or-p) (= m-or-p lex))
              (and (fn? m-or-p) (m-or-p lex)))
          token-type)) tokens))

(defn lexeme->token
  [{:keys [line] :as a} lex]
  (let [token-type   (lexeme->token-type lex)
        current-line (if (= internal-newline-substitute-token-type token-type) (inc line) line)]
    (-> a
        (update :tokens conj (token token-type current-line lex))
        (assoc :line current-line))))

(defn str->tokens
  [s]
  (let [lexemes (-> s
                    ;; we add a space so the split still works
                    (str/replace #"\n" (str " " internal-newline-substitute))
                    (str/split #"\s|\n|\r|,"))
        tokens (->> lexemes
                    (remove str/blank?)
                    (reduce lexeme->token empty-token-container))]
    ;; 1. having an eof token makes our life a little easier
    ;; 2. remove the newline tokens
    (-> tokens
        (update :tokens #(conj % (eof-token (or (-> % last :line) 1))))
        (update :tokens #(remove (fn [t] (= internal-newline-substitute-token-type (:type t))) %)))))
