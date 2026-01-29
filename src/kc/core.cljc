;; ##canonical##
(ns kc.core
  #?(:cljs (:require-macros [kc.core :refer [SRC THROW mk-error def-ui $ -mk-env]]))
  (:require
   [clojure.string :as str]
   [kc.interface :as interface]
   #?(:clj [kc.impl.macro-helpers :as macro-helpers])
   ))

(defn tsm []
  #?(:cljs (.getTime (js/Date.))
     :clj (System/currentTimeMillis)))

;;; context

(defmacro -mk-env []
  (let [env {
             ;; :keys (keys &env)
             ;; :DEBUG (->> &env :fn-scope (take 10))

             :build/mode (-> &env :shadow.build/mode)
             ;; :fn-scope (-> &env :fn-scope)
             ;; :shadow.build/root-form (-> &env :shadow.build/root-form)

             :build/tsm (System/currentTimeMillis)
             }]

    `'~env
    ))

(def -env (-mk-env))

(def rt-live? #?(:cljs (boolean (not goog.DEBUG))
                      :clj false))

(defn context? [thing]
  (boolean (:context/id thing)))

(defn- -mk-context []
  (let [context (merge -env
                       {:context/id (str (random-uuid))
                        :rt/created-tsm (tsm)
                        :rt/live? rt-live?})
        context (interface/init-context context)
        _ (clojure.core/assert (context? context))
        ]
    context))

(defonce CONTEXT (atom (-mk-context)))



(defn debug-mode? []
  (:rt/debug-mode? @CONTEXT))

;;; introspection

(defmacro SRC
  "Macro: current source code location.

  `{:src/ns some/namespace
    :src/file \"some/namespace.cljc\"
    :src/line 42}`"
  []
  (let [src (macro-helpers/src-from-env &form &env *ns*)]
    `'~src))

(comment
  (macroexpand '(SRC))
  )

;;; errors

(defmacro mk-error
  [message & [data]]
  (let [[message data cause] (macro-helpers/parse-error-form message data)]
    `(ex-info ~message
              ;; TODO optimize by removing merge
              (merge ~data
                     {:err/source '~(macro-helpers/src-from-env &form &env *ns*)})
              ~cause)))

(defmacro THROW
  [message & [data]]
  (let [[message data cause] (macro-helpers/parse-error-form message data)]
    `(throw
      (ex-info ~message
               ;; TODO optimize by removing merge
               (merge ~data
                      {:err/source '~(macro-helpers/src-from-env &form &env *ns*)})
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

;;; user interface (HTML)

(defmacro def-ui
  {:clj-kondo/lint-as 'clojure.core/defn}
  [type & form-body]
  `(interface/def-ui ~type ~@form-body))

(defmacro $
  [tag-name & attrs-and-children]
  `(interface/$ ~tag-name ~@attrs-and-children))


;;; incubator

(defn hello []
  "hello")

(comment
  (hello))
