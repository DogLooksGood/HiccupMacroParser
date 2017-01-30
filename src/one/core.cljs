(ns one.core
  (:require cljsjs.react
            cljsjs.react.dom
            cljsjs.react-datepicker
            [clojure.string :as str]
            [goog.dom :as gdom]
            [one.helper :as helper]
            [one.dom :as dom]
            [one.macros :refer-macros [defcomp defread defmut]]))

(enable-console-print!)

(defn on-js-reload [])

;; -----------------------------------------------------------------------------
;; Mutates
;; -----------------------------------------------------------------------------

(defmut init-db
  (fn [db]
    {:todo-id-serial 0
     :todos []
     :todo-edit -1
     :input ""
     :filter :all}))

(defmut update-todo-text
  (fn [db [id text]]
    (letfn [(update-by-id [todo]
              (if (= id (:id todo))
                (assoc todo :text text)
                todo))]
      (-> db
          (update :todos (partial mapv update-by-id))
          (assoc :todo-edit -1)))))

(defmut update-todo-filter
  (fn [db filter]
    (assoc db :filter filter)))

(defmut begin-todo-edit
  (fn [db id]
    (assoc db :todo-edit id)))

(defmut add-todo
  (fn [db text]
    (let [todo-id (:todo-id-serial db)]
      (-> db
          (update :todos conj {:id todo-id :text text :completed? false})
          (update :todo-id-serial inc)
          (assoc :input "")))))

(defmut update-todo-input
  (fn [db text]
    (assoc db :input text)))

(defmut toggle-todo-completed
  (fn [db id]
    (letfn [(toggle-by-id [todo]
              (if (= id (:id todo))
                (update todo :completed? not)
                todo))]
      (update db :todos (partial mapv toggle-by-id)))))

(defmut toggle-all-todos
  (fn [db completed?]
    (update db :todos (partial mapv #(assoc % :completed? completed?)))))

;; -----------------------------------------------------------------------------
;; Reads
;; -----------------------------------------------------------------------------

(defread get-todo-edit
  (fn [db]
    (get db :todo-edit)))

(defread get-todo-input
  (fn [db]
    (get db :input)))

(defread get-todos
  (fn [db]
    (->> (get db :todos)
         (filter (case (:filter db)
                   :all identity
                   :active (complement :completed?)
                   :completed :completed?)))))

(defread count-active-todos
  (fn [db]
    (->> (get db :todos)
         (filter (complement :completed?))
         count)))

(defread get-todo-filter
  (fn [db]
    (get db :filter)))

;; -----------------------------------------------------------------------------
;; Event Handlers
;; -----------------------------------------------------------------------------

(defn on-edit-todo [id]
  (begin-todo-edit id))

(defn on-edit-keydown [id e]
  (when (= 13 (.-which e))
    (let [text (.. e -target -value)]
      (when-not (str/blank? text)
        (update-todo-text [id text])))))

(defn on-input-keydown [e]
  (when (= 13 (.-which e))
    (let [text (.. e -target -value)]
      (when-not (str/blank? text)
        (add-todo text)))))

(defn on-input-change [e]
  (update-todo-input (.. e -target -value)))

(defn on-toggle-todo [id e]
  (toggle-todo-completed id))

(defn on-toggle-all [e]
  (let [completed? (.. e -target -checked)]
    (toggle-all-todos completed?)))

(defn on-change-filter [f]
  (update-todo-filter f))

;; -----------------------------------------------------------------------------
;; React Components
;; -----------------------------------------------------------------------------

(defcomp todo-item-edit [{:keys [id text]}]
  [dom/li
   [dom/input {:className "edit"
               :onKeyDown (partial on-edit-keydown id)
               :autoFocus true
               :style #js {:display "inline"}}]])

(defcomp todo-item [{:keys [id text completed?]}]
  [dom/li {:className (if completed? "completed" "")}
   [dom/div {:className "view"}
    [dom/input {:className "toggle"
                :checked completed?
                :onChange (partial on-toggle-todo id)
                :type "checkbox"}]
    [dom/label {:onDoubleClick (partial on-edit-todo id)}
     text]]])

(defcomp todo-list [props]
  (let [todos (get-todos)
        todo-edit (get-todo-edit)]
    [dom/section {:className "main"}
     [dom/input {:className "toggle-all"
                 :type "checkbox"
                 :checked (every? :completed? todos)
                 :onChange on-toggle-all}]
     [dom/ul {:className "todo-list"}
      (for [{:keys [id text completed?]} todos]
        (if (= id todo-edit)
          [todo-item-edit {:key (str "key-" id)
                           :id id}]
          [todo-item {:key (str "key-" id)
                      :id id
                      :text text
                      :completed? completed?}]))]]))

(defcomp todo-input [props]
  (let [input (get-todo-input)]
    [dom/header {:className "header"}
     [dom/input {:className "new-todo"
                 :value input
                 :onChange on-input-change
                 :onKeyDown on-input-keydown
                 :placeholder "What needs to be done?"}]])) 

(defcomp todo-filter [{:keys [filter text]}]
  [dom/li [dom/a {:className (when (= (get-todo-filter) filter) "selected")
                  :onClick (partial on-change-filter filter)} text]])

(defcomp todo-footer [props]
  [dom/footer {:className "footer"}
   [dom/span {:className "todo-count"}
    [dom/strong (count-active-todos)]
    [dom/span " items left"]]
   [dom/ul {:className "filters"}
    [todo-filter {:filter :all :text "All"}]
    [todo-filter {:filter :active :text "Active"}]
    [todo-filter {:filter :completed :text "Completed"}]]])

(defcomp index [props]
  [dom/div
   [dom/section {:className "todoapp"}
    [dom/h1 "todos"]
    [todo-input]
    [todo-list]
    [todo-footer]]
   [dom/footer {:className "info"}
    [dom/p "Double-click to edit a todo"]
    [dom/p "Written with an experimental framework for React/ClojureScript"]]])
   
;; -----------------------------------------------------------------------------

(defonce init (init-db))
(helper/mount index (gdom/getElement "app"))