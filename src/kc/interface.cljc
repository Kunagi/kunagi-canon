(ns kc.interface
  #?(:cljs (:require-macros [kc.interface :refer [def-ui $]]))
  )

(defmacro def-ui
  {:clj-kondo/lint-as 'clojure.core/defn}
  [type & form-body]
  `(defn ~type ~@form-body))

(defmacro $
  [tag-name & attrs-and-children]
  `(defn ~tag-name ~@attrs-and-children))
