;; ##canonical##
(ns kc.impl.macro-helpers)

(defn src-from-env [form env the-ns]
  (let [m (meta form)
        fn-scope (-> env :fn-scope first)
        in-fn (when fn-scope
                (symbol (str (-> fn-scope :info :ns))
                        (str (-> fn-scope :name))))
        src nil
        src (if-let [line (:line m)]
              (assoc src :src/line line)
              src)
        src (if-let [column (:column m)]
              (assoc src :src/column column)
              src)
        src (if-let [file (:file m)]
              (assoc src :src/file file)
              src)
        src (if the-ns
              (assoc src :src/ns (symbol (str the-ns)))
              src)
        src (if in-fn
              (assoc src :src/fn in-fn)
              src)]
    src))

(defn parse-error-form [message data]
   (when data
     (clojure.core/assert (map? data) "Error `data` must be a map"))
   (clojure.core/assert
    (or (qualified-keyword? message)
        (string? message))
    "Error `message` must be a string or qualified keyword")
   (let [message-is-id? (qualified-keyword? message)
         [error-id message] (if message-is-id?
                              [message (str message)]
                              [nil message])

         ;; extract cause
         cause (or (:err/cause data)
                   (:cause data))
         data (dissoc data :err/cause)
         data (dissoc data :cause)

         data (if error-id
                (assoc data :err/id error-id)
                data)
         ]
     [message data cause]))


