(ns one.parser
  (:require [clojure.walk :as walk]
            [cljs.analyzer :as analyzer]))

(defn component? [env x]
  (and (symbol? x)
       (:react-component (analyzer/resolve-var env x))))

(defn read? [env x]
  (and (symbol? x)
       (:react-read (analyzer/resolve-var env x))))

(comment
  (component? 'one.dom/div))

(defn read-call? [env x]
  (and (list? x)
       (read? env (first x))))

(defn deep-flatten [f x]
  (->> x
       (tree-seq f seq)
       (remove f)))

(defn log [f x]
  (spit f (str x))
  x)

(defn find-read-calls
  [env hiccup]
  (->> hiccup
       (deep-flatten (fn [x] (and (coll? x) (not (read-call? env x)))))
       (filter (partial read-call? env))
       (mapv (fn [[x & xs]]
               (apply list
                      (:name (analyzer/resolve-var env x))
                      xs)))))

(defn as-ns-name [env n]
  (-> (analyzer/resolve-var env n)
      :name
      str))

(defn parse-hiccup
  [env hiccup]
  (walk/postwalk
   (fn [v]
     (if (and (vector? v)
              (component? env (first v)))
       (cond
         (= 1 (count v))
         `(one.helper/create-element ~(first v) nil nil)

         (map? (second v))
         `(one.helper/create-element
           ~(first v) ~(second v) ~(mapv (partial parse-hiccup env) (subvec v 2)))

         :else
         `(one.helper/create-element
           ~(first v) nil ~(mapv (partial parse-hiccup env) (subvec v 1))))
       v))
   hiccup))
  
(comment
  (parse-hiccup
   '[dom/div {:style {:background-color "blue"}}
     [dom/span nil "hello, world"]]))
  
