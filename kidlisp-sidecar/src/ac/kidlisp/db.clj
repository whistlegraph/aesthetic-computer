(ns ac.kidlisp.db
  (:require [datomic.api :as d]))

(defn connect
  "Create database if missing, return a connection.
   URI is the full Datomic URI including storage protocol + db name,
   e.g. datomic:sql://kidlisp?jdbc:postgresql://localhost:5432/datomic"
  [uri]
  (d/create-database uri)
  (d/connect uri))

(defn db [conn] (d/db conn))

(defn transact [conn tx-data]
  @(d/transact conn tx-data))
