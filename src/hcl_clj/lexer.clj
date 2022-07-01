(ns hcl-clj.lexer
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def internal-newline-substitute "__<newline>__")
(def internal-newline-substitute-token-type :newline)

(def string-literal-space-substitution "_<space>_")
(defn string-literal-space-substitution-fn [s]
  (str/replace (first s) #"\s+" string-literal-space-substitution))

(def string-delimiter-re "['\"]")
(def valid-characters-re-* "[a-zA-Z0-9\\-_\\s]+")

(defn re-patterns [& s]
  (re-pattern (apply str s)))

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
   [":" :assignment]
   [internal-newline-substitute internal-newline-substitute-token-type]
   ;;
   [(partial re-find (re-patterns string-delimiter-re ".*" string-delimiter-re)) :string-literal]
   [(partial re-find #"[0-9\.]+") :number-literal]
   [(partial re-find #"true|false") :boolean-literal]
   ;;
   [string? :keyword]])

(def single-character-lexemes
  #{:scope-open :scope-close :list-open :list-close :assignment})

(def single-character-lexemes-split-re
  (str/join "|"
            (reduce (fn [a [c t]]
                      (if (single-character-lexemes t)
                        (conj a (str "\\" c)) a)) [] tokens)))

;;

(defmulti sanitize (comp first vector))

(defmethod sanitize :default
  [& _]
  nil)

(defmethod sanitize :keyword
  [_ lex]
  lex)

(defmethod sanitize :string-literal
  [_ lex]
  (-> (last (re-find (re-patterns string-delimiter-re "(.*)" string-delimiter-re) lex))
      (str/replace (re-pattern string-literal-space-substitution) " ")))

(defmethod sanitize :boolean-literal
  [_ lex]
  (not (= lex "false")))

(defmethod sanitize :number-literal
  [_ lex]
  ;; comment on security:
  ;; yes, read-string is very powerful and can do some horrendous things
  ;; but we've already identified the token at this point so we are
  ;; _relatively_ sure it's just a number... so it's probably fine :)
  (edn/read-string lex))

;;

(defn token
  ([t l]
   {:type t
    :line l})
  ([t l content]
   (let [c (sanitize t content)]
     (cond-> {:type t
              :line l}
             (not (nil? c)) (assoc :content c)))))

(def sof-token
  (token :sof 1))

(defn eof-token
  [l]
  (token :eof l))

(def empty-token-container
  {:lines    1
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
  [{:keys [lines] :as a} lex]
  (let [token-type   (lexeme->token-type lex)
        current-line (if (= internal-newline-substitute-token-type token-type) (inc lines) lines)]
    (-> a
        (update :tokens conj (token token-type current-line lex))
        (assoc :lines current-line))))

;;

(defn str->lexemes
  [s]
  ;; TODO this is flaky and we could do with a better way to generate lexemes
  ;; for example, this doesn't respect spaces in string literals so we have to
  ;; pre/post process
  (remove str/blank?
          (-> s
              ;; remove line comments
              (str/replace #"\/{2}.*|#.*" "")
              ;; add padding to single character lexemes
              (str/replace (re-pattern single-character-lexemes-split-re) #(str " " %1 " "))
              ;; we add a space so the split still works
              (str/replace #"\n" (str " " internal-newline-substitute))
              (str/replace  (re-patterns string-delimiter-re
                                         valid-characters-re-*
                                         "(\\s+)"
                                         valid-characters-re-*
                                         string-delimiter-re)
                            string-literal-space-substitution-fn)
              (str/split (re-pattern "\\s|\\n|\\r|,")))))

(defn str->tokens
  [s]
  (let [lexemes (str->lexemes s)
        tokens  (reduce lexeme->token empty-token-container lexemes)]
    ;; 1. having an sof and eof token makes our life a little easier
    ;; 2. remove the newline tokens
    (-> tokens
        (update :tokens #(into [sof-token] %))
        (update :tokens #(conj % (eof-token (or (-> % last :line) 1))))
        (update :tokens #(remove (fn [t] (= internal-newline-substitute-token-type (:type t))) %)))))
