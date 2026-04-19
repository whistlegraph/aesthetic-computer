(ns ac.kidlisp.admin
  "Read-only admin surface consumed by the silo dashboard via the silo server.
   v1 is strictly read-only: no transact, no retract, no write surface at all."
  (:require [datomic.api :as d]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn- ok  [body] {:status 200 :body body})
(defn- bad [msg] {:status 400 :body {:error msg}})

(defn schema
  "GET /admin/schema — attribute catalog (only :kidlisp/ :keep/ :tezos/
   :rebake/ :ipfs/ :user/ idents; filters out system attributes)."
  [conn]
  (fn [_req]
    (let [db (d/db conn)
          our? (fn [kw]
                 (contains? #{"kidlisp" "keep" "tezos" "rebake" "ipfs" "user" "mint"}
                            (namespace kw)))
          attrs (d/q '[:find [?ident ...]
                       :where [?e :db/ident ?ident]]
                     db)
          rows (for [ident attrs
                     :when (and (keyword? ident) (our? ident))
                     :let  [e (d/entity db ident)]]
                 {:ident       ident
                  :valueType   (:db/valueType e)
                  :cardinality (:db/cardinality e)
                  :unique      (:db/unique e)
                  :indexed     (boolean (:db/index e))
                  :isComponent (boolean (:db/isComponent e))
                  :noHistory   (boolean (:db/noHistory e))
                  :doc         (:db/doc e)})]
      (ok {:attributes (vec (sort-by :ident rows))}))))

(defn stats
  "GET /admin/stats — entity counts per type + recent tx rate."
  [conn]
  (fn [_req]
    (let [db       (d/db conn)
          kidlisp  (ffirst (d/q '[:find (count ?e) :where [?e :kidlisp/code]] db))
          users    (ffirst (d/q '[:find (count ?e) :where [?e :user/sub]]    db))
          keeps    (ffirst (d/q '[:find (count ?e) :where [?e :keep/token-id]] db))
          tx-last-hour
          (ffirst (d/q '[:find (count ?tx)
                         :in $ ?since
                         :where
                         [?tx :db/txInstant ?inst]
                         [(> ?inst ?since)]]
                       db
                       (java.util.Date. (- (System/currentTimeMillis) (* 60 60 1000)))))]
      (ok {:entityCounts {:kidlisp kidlisp
                          :users   users
                          :keeps   keeps}
           :txLastHour   tx-last-hour
           :basisT       (d/basis-t db)}))))

(defn list-entities
  "GET /admin/entities/:type?limit=&offset=
   Supports :type = kidlisp | user | keep."
  [conn]
  (fn [req]
    (let [typ    (get-in req [:path-params :type])
          {:strs [limit offset]} (:query-params req)
          limit  (max 1 (min 1000 (Integer/parseInt (or limit "50"))))
          offset (max 0 (Integer/parseInt (or offset "0")))
          db     (d/db conn)
          q      (case typ
                   "kidlisp" '[:find [?e ...] :where [?e :kidlisp/code]]
                   "user"    '[:find [?e ...] :where [?e :user/sub]]
                   "keep"    '[:find [?e ...] :where [?e :keep/token-id]]
                   nil)]
      (if-not q
        (bad (str "unknown type: " typ))
        (let [all (vec (sort (d/q q db)))
              page (->> all (drop offset) (take limit))
              rows (mapv (fn [eid]
                           (let [e (d/entity db eid)]
                             (into {:db/id eid} e)))
                         page)]
          (ok {:type   typ
               :total  (count all)
               :offset offset
               :limit  limit
               :items  rows}))))))

(defn entity
  "GET /admin/entity/:eid — current facts."
  [conn]
  (fn [req]
    (let [eid (Long/parseLong (get-in req [:path-params :eid]))
          db  (d/db conn)
          e   (d/entity db eid)]
      (if e
        (ok (into {:db/id eid} e))
        {:status 404 :body {:error "not-found"}}))))

(defn entity-history
  "GET /admin/entity/:eid/history — all facts ever asserted about this
   entity, with tx timestamp and add/retract flag."
  [conn]
  (fn [req]
    (let [eid (Long/parseLong (get-in req [:path-params :eid]))
          db  (d/db conn)
          rows (d/q '[:find ?a ?v ?tx ?inst ?added
                      :in $ ?e
                      :where
                      [?e ?a ?v ?tx ?added]
                      [?tx :db/txInstant ?inst]]
                    (d/history db) eid)
          by-tx (sort-by #(nth % 3) rows)]
      (ok {:eid     eid
           :history (mapv (fn [[a v tx inst added]]
                            {:attr  (d/ident db a)
                             :value (if (keyword? v) v (str v))
                             :tx    tx
                             :at    inst
                             :added added})
                          by-tx)}))))

(defn tx-log
  "GET /admin/tx-log?limit=&since=
   Recent transactions with summary of assertions/retractions."
  [conn]
  (fn [req]
    (let [{:strs [limit since]} (:query-params req)
          limit (max 1 (min 1000 (Integer/parseInt (or limit "50"))))
          db    (d/db conn)
          since-inst (when (seq since)
                       (java.util.Date/from (java.time.Instant/parse since)))
          rows (d/q (if since-inst
                      '[:find ?tx ?inst
                        :in $ ?since
                        :where
                        [?tx :db/txInstant ?inst]
                        [(> ?inst ?since)]]
                      '[:find ?tx ?inst
                        :where
                        [?tx :db/txInstant ?inst]])
                    db (or since-inst (java.util.Date. 0)))
          recent (->> rows (sort-by second #(compare %2 %1)) (take limit))]
      (ok {:transactions
           (mapv (fn [[tx inst]]
                   {:tx tx
                    :at inst})
                 recent)}))))

(defn- safe-query?
  "Ultra-conservative read-only check on a datalog query edn string.
   Rejects transact/retract/with/apply-tx/io-rsrc forms and anything
   mentioning `:db/add` or `:db/retract`. Intentionally strict."
  [q-str]
  (and (string? q-str)
       (not (re-find #"(?i)\b(transact|retract|with|d/with|db/add|db/retract|d/transact|io-rsrc)\b" q-str))))

(defn query
  "POST /admin/query — body: {query (edn string), args (vec)}
   Runs against (d/db conn) only. Results capped at 10k rows + 5s timeout."
  [conn]
  (fn [req]
    (let [{:keys [query args]} (:body-params req)]
      (if-not (safe-query? query)
        (bad "query rejected (read-only policy)")
        (try
          (let [parsed   (edn/read-string query)
                db       (d/db conn)
                fut      (future (d/q parsed db (or args [])))
                result   (deref fut 5000 ::timeout)]
            (cond
              (= result ::timeout)
              (do (future-cancel fut)
                  {:status 408 :body {:error "query timed out (5s)"}})

              :else
              (ok {:count (count result)
                   :rows  (->> result (take 10000) vec)
                   :truncated (> (count result) 10000)})))
          (catch Throwable t
            {:status 500 :body {:error (.getMessage t)}}))))))

(defn backups
  "GET /admin/backups — lists pg_dump files in /var/backups/datomic."
  [_conn]
  (fn [_req]
    (let [dir (io/file "/var/backups/datomic")
          files (when (.isDirectory dir)
                  (->> (.listFiles dir)
                       (filter #(str/ends-with? (.getName ^java.io.File %) ".sql.gz"))
                       (sort-by #(.lastModified ^java.io.File %) >)
                       (take 30)
                       (map (fn [^java.io.File f]
                              {:name (.getName f)
                               :size (.length f)
                               :at   (java.util.Date. (.lastModified f))}))))]
      (ok {:backups (vec files)}))))

(defn health
  "GET /admin/health — transactor reachable, schema present."
  [conn]
  (fn [_req]
    (try
      (let [db (d/db conn)
            schema-ok? (some? (d/entid db :kidlisp/code))]
        (ok {:transactor true
             :schema     schema-ok?
             :basisT     (d/basis-t db)}))
      (catch Throwable t
        {:status 500 :body {:transactor false
                            :error (.getMessage t)}}))))
