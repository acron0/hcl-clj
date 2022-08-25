(ns hcl-clj.ast
  (:require [clojure.string :as str]))


;; predicates

(defn start-scope?
  [{:keys [type]}]
  (#{:scope-open :list-open :sof} type))

(defn end-scope?
[{:keys [type]}]
(#{:scope-close :list-close :eof} type))

(defn realised-scope?
  [{:keys [type scope-type]}]
  (and (= :scope type)
       (not= :list scope-type)))

(defn list-scope?
  [{:keys [type scope-type]}]
  (and (= :scope type)
       (= :list scope-type)))

(defn scope-type
  [{:keys [type]}]
  (cond
    (#{:scope-open :scope-close} type) :block
    (#{:list-open :list-close} type)   :list
    (#{:sof :eof} type)                :file
    :else                              (throw (Exception. (str "Couldn't deduce scope type for " type)))))

(defn literal?
  [{:keys [type]}]
  (re-find #"^[a-zA-Z]+-literal$" (name type)))

(defn assignment-tuple?
  [[nt at vt]]
  (and nt at vt
       (= :keyword (:type nt))
       (= :assignment (:type at))
       (or (literal? vt)
           (list-scope? vt))))

(defn scope-name?
  [[kt & ts]]
  ;; a keyword followed by zero or more string-literals
  ;; and possibly an assignment
  (and (= :keyword (:type kt))
       (boolean
         (loop [tokens ts
                results []]
           (when-let [ft (first tokens)]
             (cond (= :string-literal (:type ft))
                   (recur (rest tokens) (conj results (:type ft)))
                   (= :assignment (:type ft))
                   (recur (rest tokens) results)
                   ;; NB. `every?` on an empty collection returns true
                   (realised-scope? ft) (every? #{:string-literal} results)
                   ;;
                   :else false))))))

;;

(defn capture-scope
  "Collapses a collection of tokens into blocks of scope.
  Scope tokens (start/end) are defined by the `start-scope?` and `end-scope?` predicates.
  Tokens inside scope blocks appear in their `:content` field"
  ([tokens]
   (-> (capture-scope tokens 0)
       first
       first))
  ([tokens start-depth]
   ;; depth is mainly for debugging purposes
   ;; skip is a total count of tokens consumed by scope, used to navigate the collection
   (loop [depth  start-depth
          skip   1
          ;;
          result []
          remaining-tokens tokens]
     (if-let [current-token (first remaining-tokens)]
       (cond
         ;; create a new scope block
         (start-scope? current-token)
         (let [[nested-results nested-skip] (capture-scope (rest remaining-tokens) (inc depth))]
           (recur depth
                  (+ skip (inc nested-skip))
                  (conj result {:type :scope
                                :line (:line current-token)
                                :scope-type (scope-type current-token)
                                :content nested-results})
                  (drop (inc nested-skip) remaining-tokens)))
         ;; the current scope block has ended
         (end-scope? current-token)
         [result skip]
         ;; move to the next token
         :else
         (recur depth (inc skip) (conj result current-token) (rest remaining-tokens)))
       [result skip]))))

(defn apply-name-to-scope
  "We collect scope-preceding keywords, string-literals and assignment
  and collapse them into a string `:name` which is applied to the current scope"
  [[kt & ts]]
  ;; a keyword followed by zero or more string-literals
  (loop [tokens ts
         results [(:content kt)]]
    ;; TODO this fn is very similar to scope-name
    (when-let [ft (first tokens)]
      (cond (= :string-literal (:type ft))
            (recur (rest tokens) (conj results (:content ft)))
            (= :assignment (:type ft))
            (recur (rest tokens) (conj results nil)) ;;
            ;;
            (realised-scope? ft) [(assoc ft :name (str/join "." (filter identity results))) (count results)]
            :else (throw (Exception. (str "How have we got here?" (realised-scope? ft) ft )))))))

(defn name-scope
  "Iterates through a list of tokens that has already been scoped (see `capture-scope`).
  As it detects 'names' (keywords followed by zero or more stirng-literals, optionally an assignment)
  it applies them to the associated scope block. It operates recursively to work down the tree."
  ([scope]
   (update scope
           :content
           (fn [tokens]
             (-> (name-scope tokens 0)
                 first))))
  ([tokens start-depth]
   ;; see comments re recursion in `capture-scope`
   (loop [depth  start-depth
          skip   1
          ;;
          result []
          remaining-tokens tokens]
     (if-let [current-token (first remaining-tokens)]
       (cond
         (scope-name? remaining-tokens)
         (let [[named-scope additional-skip] (apply-name-to-scope remaining-tokens)
               new-scope (update named-scope
                                 :content
                                 (fn [tokens]
                                   (-> (name-scope tokens (inc depth))
                                       first)))]
           (recur depth
                  (+ (inc additional-skip) skip)
                  (conj result new-scope)
                  (drop (inc additional-skip) remaining-tokens)))
         ;;
         :else
         (recur depth (inc skip) (conj result current-token) (rest remaining-tokens)))
       [result skip]))))

(defn create-kv-pair
  [[nt _ vt]]
  {:type :key-value-pair
   :line (:line nt)
   :name (:content nt)
   :content vt})


(defn realise-assignments
  ([scope]
   (update scope
           :content
           (fn [tokens]
             (-> (realise-assignments tokens 0)
                 first))))
  ([tokens start-depth]
   ;; see comments re recursion in `capture-scope`
   (loop [depth  start-depth
          skip   1
          ;;
          result []
          remaining-tokens tokens]
     (if-let [current-token (first remaining-tokens)]
       (cond
         ;; is this an assignment tuple?
         (assignment-tuple? remaining-tokens)
         (let [assignment-token  (create-kv-pair remaining-tokens)
               additional-skip 2]
           (recur depth
                  (+ (inc additional-skip) skip)
                  (conj result assignment-token)
                  (drop (inc additional-skip) remaining-tokens)))
         ;; found scope so recur
         (realised-scope? current-token)
         (let [new-scope (update current-token
                                 :content
                                 (fn [tokens]
                                   (-> (realise-assignments tokens (inc depth))
                                       first)))]
           (recur depth
                  (inc skip)
                  (conj result new-scope)
                  (rest remaining-tokens)))
         ;;
         :else
         (recur depth (inc skip) (conj result current-token) (rest remaining-tokens)))
       [result skip]))))

(defn tokens->ast
  "Converts a list of tokens from the lexer into something
  resembling an AST."
  [{:keys [tokens]}]
  (-> tokens
      ;; each of the following functions traverses the whole tree
      ;; so this is probably pretty inefficient at the moment.
      capture-scope ;; <- has to be first
      name-scope
      realise-assignments))
