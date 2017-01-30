(ns one.db)

(def ^:dynamic *new-state-val* nil)

(defonce app-state (atom {}))
        
