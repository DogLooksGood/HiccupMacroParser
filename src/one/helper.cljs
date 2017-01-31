(ns one.helper)

(defn as-js-obj [m]
  (->> m
       (map (fn [[k v]] [(key->js k) v]))
       flatten
       (apply js-obj)))

(defn create-class [m]
  (.createClass js/React (clj->js m)))

(defn create-element
  ([comp props children]
   (apply js/React.createElement
          comp
          (as-js-obj props)
          children)))

(defn props [^js/React.Component this]
  (js->clj (aget this "props") :keywordize-keys true))

(defn state [^js/React.Component this]
  (js->clj (aget this "state") :keywordize-keys true))

(defn set-state [^js/React.Component this new-state]
  (.setState this (clj->js new-state)))

(defn mount [comp mount-point]
  (.render js/ReactDOM
           (create-element comp nil nil)
           mount-point))
