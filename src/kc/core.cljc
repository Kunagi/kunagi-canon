;; ##canonical##
(ns kc.core
  #?(:cljs (:require-macros [kc.core :refer [SRC THROW mk-error]]))
  (:require
   [clojure.string :as str]))

;;; context

(defonce CONTEXT (atom {}))

;;; introspection

#?(:clj (defn -src-from-env [form env the-ns]
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
            src)))

(defmacro SRC
  "Macro: current source code location.

  `{:src/ns some/namespace
    :src/file \"some/namespace.cljc\"
    :src/line 42}`"
  []
  (let [src (-src-from-env &form &env *ns*)]
    `'~src))

(comment
  (macroexpand '(SRC))
  )

;;; errors

(:clj
 (defn -parse-error-form [message data]
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

         ;; data (assoc data
         ;;             :err/source (-src-from-env &form &env *ns*))

         data (if error-id
                (assoc data :err/id error-id)
                data)
         ]
     [message data cause])))

(defmacro mk-error
  [message & [data]]
  (let [[message data cause] (-parse-error-form message data)]
    `(ex-info ~message
              ;; TODO optimize by removing merge
              (merge ~data
                     {:err/source '~(-src-from-env &form &env *ns*)})
              ~cause)))


(defmacro THROW
  [message & [data]]
  (let [[message data cause] (-parse-error-form message data)]
    `(throw
      (ex-info ~message
               ;; TODO optimize by removing merge
               (merge ~data
                      {:err/source '~(-src-from-env &form &env *ns*)})
               ~cause))))

(defn error-data? [thing]
  (boolean (:err/message thing)))

(defn- -conform-error-stack-line [line]
  (when line
    (when-not (or (str/includes? line "runtime/shadow.cljs")
                  (str/includes? line "runtime/shadow.cljs")
                  (str/includes? line "react_dom_client_development.js")
                  (str/includes? line "runtime/cljs.core.js"))
      (let [line (str/trim line)
            line (if (str/starts-with? line "at ")
                   (subs line 3)
                   line)
            line (str/trim line)]
        line))))

(defn- -conform-error-stack [stack]
  (when stack
    (let [lines (str/split-lines stack)]
      (->> lines
           (mapv -conform-error-stack-line)
           (remove nil?)
           (into [])))))

(defn as-error-data
  "Converts `thing` to error map."
  [thing]
  (try
    (cond
      (nil? thing)
      {:err/id ::error-as-nil
       :err/message "Unexpected nil"}

      (error-data? thing) thing

      (string? thing)
      {:err/message thing}

      ;; TODO Java Throwable

      #?(:cljs
         (instance? js/Error thing))
      #?(:cljs
         (let [filename (-> thing .-fileName)
               source (or
                       (when filename
                         {:src/file filename
                          :src/line (-> thing .-lineNumber)})
                       (when-let [data (-> thing .-data)]
                         (when (map? data)
                           (get data :err/source))))]
           {:err/id ::javascript-error
            :err/message (-> thing .-message)
            :err/cause (when-let [cause (-> thing .-cause)]
                         (as-error-data cause))
            :err/stack (-conform-error-stack (-> thing .-stack))
            :err/source source
            :err/data (when-let [data (-> thing .-data)]
                        (cond
                          (nil? data) nil
                          (map? data) (when-not (empty? data)
                                        (-> data
                                            (dissoc :err/source)))
                          :else data))}))

      (map? thing)
      {:err/id ::map-error
       :err/message "Error with data"
       :err/data thing}

      #?(:cljs
         (instance? js/Object thing))
      #?(:cljs
         {:err/id ::error-as-js-object
          :err/message "Error as JavaScript object"
          :err/data {:js-object thing}})

      :else
      {:err/id ::error-as-unsupported-thing
       :err/message (str thing)
       :err/data {:thing thing}})

    (catch #?(:cljs :default :clj Exception) ex
      {:err/id ::error-conforming-error
       :err/message "kc.core/->error failed"
       :err/data {:thing-which-failed-to-interpret thing}
       :err/cause ex})))

;;; incubator

(defn hello []
  "hello")

(comment
  (hello))
