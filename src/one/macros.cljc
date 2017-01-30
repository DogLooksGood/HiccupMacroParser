(ns one.macros
  #?(:cljs
     (:require cljsjs.react
               cljsjs.react.dom
               [one.helper :as helper]
               [one.db :as db])
     :clj
     (:require [one.parser :as parser]
               [cljs.analyzer :as analyzer])))

#?(:cljs
   (let [i (atom 0)]
     (defn gen-id []
       (swap! i inc))))

#?(:cljs
   (defn obj-eq [obj1 obj2]
     (let [ks1 (js/Object.keys obj1)]
       (and (= (.-length ks1)
               (.-length (js/Object.keys obj2)))
            (->> ks1
                 (map #(= (aget obj1 %)
                          (aget obj2 %)))
                 (every? identity))))))

#?(:clj
   (defmacro hiccup
     [body]
     `(do ~(parser/parse-hiccup &env body))))

#?(:clj
   (defmacro ^{:style/indent :defn} defcomp
     ([name body]
      `(def ~(with-meta name {:react-component true}) ~body))
     ([name binding body]
      (let [display-name (parser/as-ns-name &env name)
            read-calls  (parser/find-read-calls &env body)
            read-names  (mapv str read-calls)]
        `(def ~(with-meta name {:react-component true})
           (helper/create-class
            {:displayName ~display-name
             :getInitialState
             (fn []
               (cljs.core/js-obj ~@(interleave read-names read-calls)))
             :componentWillMount
             (fn []
               (cljs.core/this-as this#
                 (let [i# (gen-id)]
                   (cljs.core/aset this# "__atom_watch_id" i#)
                   (cljs.core/add-watch one.db/app-state
                                        (keyword (str ~display-name "__" i#))
                                        (fn [k# iref# old-val# new-val#]
                                          (binding [one.db/*new-state-val* new-val#]
                                            (let [new-state# (cljs.core/js-obj ~@(interleave read-names read-calls))]
                                              (when-not (obj-eq new-state# (.-state this#))
                                                (.setState this# new-state#)))))))))
             :componentWillUnmount
             (fn []
               (cljs.core/this-as this#
                 (let [i# (cljs.core/aget this# "__atom_watch_id")]
                   (cljs.core/remove-watch one.db/app-state
                                           (keyword (str ~display-name "__" i#))))))
             :render
             (fn []
               (cljs.core/this-as this#
                 (let [~(first binding) (helper/props this#)]
                   ~(parser/parse-hiccup &env body))))}))))))

#?(:clj
   (defmacro ^{:style/indent :defn} defread
     [name f]
     `(defn ~(with-meta name {:react-read true})
        ([]
         (if one.db/*new-state-val*
           (~f one.db/*new-state-val*)
           (~f @one.db/app-state)))
        ([params#]
         (if one.db/*new-state-val*
           (~f one.db/*new-state-val* params#)
           (~f @one.db/app-state params#))))))

#?(:clj
   (defmacro ^{:style/indent :defn} defmut
     [name f]
     `(defn ~(with-meta name {:react-mutate true})
        ([]
         (swap! one.db/app-state ~f))
        ([params#]
         (swap! one.db/app-state ~f params#)))))
