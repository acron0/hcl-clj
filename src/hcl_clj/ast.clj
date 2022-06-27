(ns hcl-clj.ast)

(declare tokens->ast)

;;

(defn create-empty-ast-node
  [op id]
  {:op op
   :id id
   :children []})

;;

(defn create-binding
  [name-token value-tokens]
  (let [binding (create-empty-ast-node :bind (:content name-token))]

    binding))
;;

(defmulti process-token
  (fn [_ t _] (:type t)))

(defmethod process-token :keyword
  ;; keywords can either be blocks OR assignments
  ;; blocks might have one or more IDs
  [ast token next-tokens]
  (let [nt (first next-tokens)]
    (if (= :assignment nt)
      (create-binding token (rest next-tokens)))))

(defmethod process-token :default
  [_ t _]
  (throw (Exception. (str "Invalid token:" t))))

;;

;; (defn tokens->ast
;;   ([m]
;;    (tokens->ast m {:op :root}))
;;   ([{:keys [tokens]} state]
;;    (loop [ast state
;;           remaining-tokens tokens]
;;      (let [current-token (first remaining-tokens)]
;;        (when-not (= :eof (:type current-token))
;;          (let [[ast' cursor] (process-token ast current-token (rest tokens))]
;;            (recur ast' (drop cursor remaining-tokens)))
;;          )))))



(defn start-scope?
  [{:keys [type]}]
  (#{:scope-open :list-open :sof} type))

(defn end-scope?
  [{:keys [type]}]
  (#{:scope-close :list-close :eof} type))

(defn scope-type
  [{:keys [type]}]
  (cond
    (#{:scope-open :scope-close} type) :block
    (#{:list-open :list-close} type)   :list
    (#{:sof :eof} type)                :file
    :else                              (throw (Exception. (str "Couldn't deduce scope type for " type)))))

(defn capture-scope
  ([tokens]
   (-> (capture-scope tokens 0)
       first
       first))
  ([tokens start-depth]
   (loop [depth  start-depth
          skip   1
          result []
          remaining-tokens tokens]
     (println "d=" depth "s=" skip "r=" result "rtc=" (count remaining-tokens))
     ;; (Thread/sleep 1000)
     (if-let [current-token (first remaining-tokens)]
       (do
         (println "ct=" current-token)
         (cond (start-scope? current-token)
               (let [[nested-results nested-skip] (capture-scope (rest remaining-tokens) (inc depth))]
                 (recur depth
                        (+ skip (inc nested-skip))
                        (conj result {:type :scope
                                      :line (:line current-token)
                                      :scope-type (scope-type current-token)
                                      :content nested-results})
                        (drop (inc nested-skip) remaining-tokens)))
               (end-scope? current-token)
               [result skip]
               ;;
               :else
               (recur depth (inc skip) (conj result current-token) (rest remaining-tokens))))
       [result skip]))))

(defn tokens->ast
  [{:keys [tokens]}]
  (let [scoped-tokens (capture-scope tokens)]
    scoped-tokens))
