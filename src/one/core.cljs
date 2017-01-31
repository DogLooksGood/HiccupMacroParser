(ns one.core
  (:require [clojure.string :as str]
            [goog.dom :as gdom]
            [one.helper :as helper]
            [one.dom :as dom]
            [one.macros :refer-macros [defcomp defread defmut]]))

(set! *warn-on-infer* true)

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
     :todo-edit-text ""
     :input ""
     :filter :all}))

(defmut update-todo-edit-text
  (fn [db text]
    (assoc db :todo-edit-text text)))

(defmut update-todo-text
  (fn [db [id text]]
    (letfn [(update-by-id [todo]
              (if (= id (:id todo))
                (assoc todo :text text)
                todo))]
      (-> db
          (update :todos (partial mapv update-by-id))
          (merge {:todo-edit -1
                  :todo-edit-text ""})))))

(defmut update-todo-filter
  (fn [db filter]
    (assoc db :filter filter)))

(defmut begin-todo-edit
  (fn [db id]
    (let [text (->> (:todos db)
                    (filter #(= id (:id %)))
                    first
                    :text)]
      (assoc db
             :todo-edit id
             :todo-edit-text text))))

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

(defmut delete-todo
  (fn [db id]
    (update db :todos (partial filterv #(not= id (:id %))))))

(defmut toggle-all-todos
  (fn [db completed?]
    (update db :todos (partial mapv #(assoc % :completed? completed?)))))

;; -----------------------------------------------------------------------------
;; Reads
;; -----------------------------------------------------------------------------

(defread get-todo-edit-text
  (fn [db]
    (get db :todo-edit-text)))

(defread get-todo-edit
  (fn [db]
    (get db :todo-edit)))

(defread get-todo-input
  (fn [db]
    (get db :input)))

(defread get-todos
  (fn [db]
    (let [f-type (:filter db)
          f (condp = f-type
              :all identity
              :active (complement :completed?)
              :completed :completed?)]
      (->> (get db :todos)
           (filter f)))))

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

(defn on-delete-todo [id]
  (delete-todo id))

(defn on-edit-todo [id]
  (begin-todo-edit id))

(defn on-edit [e]
  (update-todo-edit-text (aget e "target" "value")))

(defn on-edit-blur [id ^js/Event e]
  (let [text (aget e "target" "value")]
    (if (str/blank? text)
      (let [^js/HTMLInputElement ele (aget e "target")]
        (.focus ele))
      (update-todo-text [id text]))))
  
(defn on-edit-keydown [id ^js/KeyboardEvent e]
  (when (= 13 (aget e "which"))
    (let [text (aget e "target" "value")]
      (when-not (str/blank? text)
        (update-todo-text [id text])))))

(defn on-input-keydown [^js/Event e]
  (when (= 13 (aget e "which"))
    (let [text  (aget e "target" "value")]
      (when-not (str/blank? text)
        (add-todo text)))))

(defn on-input-change [^js/Event e]
  (update-todo-input (aget e "target" "value")))

(defn on-toggle-todo [id ^js/Event e]
  (toggle-todo-completed id))

(defn on-toggle-all [^js/Event e]
  (let [completed? (aget e "target" "value")]
    (toggle-all-todos completed?)))

(defn on-change-filter [f]
  (update-todo-filter f))

;; -----------------------------------------------------------------------------
;; React Components
;; -----------------------------------------------------------------------------

(defcomp todo-item-edit [{:keys [id text]}]
  [dom/li {:className "editing"}
   [dom/input {:className "edit"
               :value (get-todo-edit-text)
               :onBlur (partial on-edit-blur id)
               :onKeyDown (partial on-edit-keydown id)
               :onChange on-edit
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
     text]
    [dom/button {:className "destroy"
                 :onClick (partial on-delete-todo id)}]]])

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
