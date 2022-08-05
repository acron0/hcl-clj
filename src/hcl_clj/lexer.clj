(ns hcl-clj.lexer
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(def internal-newline-substitute "__<newline>__")
(def internal-newline-substitute-token-type :newline)

(def string-literal-space-substitution "__<space>__")
(defn string-literal-space-substitution-fn [s]
  (str/replace (first s) #"\s+" string-literal-space-substitution))

(def string-delimiter-re "['\"]")
(def valid-characters-re-*
  (str "["
       (str/join ["a-z"
                  "A-Z"
                  "0-9"
                  "-"
                  "_"
                  ","
                  "~"
                  ";"
                  "\\."
                  "\\<"
                  "\\>"
                  "\\?"
                  "\\!"
                  "\\/"
                  "\\s"])
       "]+"))

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

(defn remove-string-literals
  [s]
  (->> s
       (re-seq #"\"(.*?)\"")
       (reduce (fn [{:keys [string lookup idx] :as d}
                    [match _group]]
                 (-> d
                     (update :string str/replace (re-pattern (str/re-quote-replacement match)) (format "__<string_%d>__" idx))
                     (update :lookup assoc idx match)
                     (update :idx inc)))
               {:string s :lookup {} :idx 1})))

(defn replace-string-literals
  [s lookup]
  (map (fn [x]
         (if-let [[_ i] (re-find #"__<string_(\d+)>__" x)]
           (get lookup (read-string i) "<ERROR: STRING LITERAL MISSING>") x)) s))


(defn str->lexemes
  [s]
  ;; TODO this is flaky and we could do with a better way to generate lexemes
  ;; for example, this doesn't respect spaces in string literals so we have to
  ;; pre/post process
  (let [{:keys [string lookup]} (remove-string-literals s)]
    (remove str/blank?
            (-> string
                ;; remove line comments
                (str/replace #"\/{2}.*|#.*" "")
                ;; add padding to single character lexemes
                (str/replace (re-pattern single-character-lexemes-split-re) #(str " " %1 " "))
                ;; add padding to string literals
                (str/replace (re-pattern "\".*\"") #(str " " %1 " "))
                ;; we add a space so the split still works
                (str/replace #"\n" (str " " internal-newline-substitute))
                ;; we split against spacing characters
                (str/split (re-pattern "\\s|\\n|\\r|,"))
                ;; replace string literals
                (replace-string-literals lookup)))))

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
