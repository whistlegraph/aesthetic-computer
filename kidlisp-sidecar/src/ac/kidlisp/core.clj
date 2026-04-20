(ns ac.kidlisp.core
  (:require [org.httpkit.server :as http]
            [reitit.ring :as ring]
            [reitit.ring.middleware.muuntaja :as muuntaja-mw]
            [muuntaja.core :as m]
            [ring.middleware.params :as params]
            [ac.kidlisp.db :as db]
            [ac.kidlisp.schema :as schema]
            [ac.kidlisp.handlers :as h]
            [ac.kidlisp.admin :as admin]
            [ac.kidlisp.auth :as auth])
  (:gen-class))

(defn- env [k default]
  (or (System/getenv k) default))

(defn- app-router [conn]
  (ring/ring-handler
   (ring/router
    [["/health"
      {:get (fn [_] {:status 200 :body {:ok true}})}]

     ;; ───── Public kidlisp API (called by AC backend) ─────
     ["/kidlisp"
      {:middleware [(auth/client-secret-middleware)]}
      ["" {:post (h/create conn)
           :get  (h/list-codes conn)}]
      ["/lookup"          {:post (h/batch-lookup conn)}]
      ["/stats/functions" {:get  (h/stats-functions conn)}]
      ["/hash/:hash"      {:get  (h/lookup-hash conn)}]
      ["/:code"           {:get  (h/lookup-code conn)}]
      ["/:code/mint"      {:post (h/record-mint conn)}]
      ["/:code/tezos-state"     {:post (h/set-tezos-state conn)}]
      ["/:code/pending-rebake"  {:post (h/set-pending-rebake conn)}]
      ["/:code/ipfs-media"      {:post (h/set-ipfs-media conn)}]
      ["/:code/atproto-rkey"    {:post (h/set-atproto-rkey conn)}]
      ["/:code/lineage"         {:get  (h/lineage conn)}]
      ["/:code/ast"             {:get  (h/get-ast conn)
                                 :post (h/set-ast conn)}]
      ["/structural/pieces-using" {:get (h/find-pieces-using-op conn)}]]

     ;; ───── Admin surface (silo-only, read-only in v1) ─────
     ["/admin"
      {:middleware [(auth/admin-secret-middleware)]}
      ["/schema"              {:get  (admin/schema conn)}]
      ["/stats"               {:get  (admin/stats conn)}]
      ["/entities/:type"      {:get  (admin/list-entities conn)}]
      ["/entity/:eid"         {:get  (admin/entity conn)}]
      ["/entity/:eid/history" {:get  (admin/entity-history conn)}]
      ["/tx-log"              {:get  (admin/tx-log conn)}]
      ["/query"               {:post (admin/query conn)}]
      ["/backups"             {:get  (admin/backups conn)}]
      ["/health"              {:get  (admin/health conn)}]]]

    ;; :conflicts nil disables reitit's strict conflict detector. Our
    ;; overlaps are resolved either by static-segment precedence
    ;; (e.g. /kidlisp/hash beats /kidlisp/:code) or by HTTP method
    ;; (e.g. POST /kidlisp/lookup vs GET /kidlisp/:code).
    {:conflicts nil
     :data {:muuntaja   m/instance
            :middleware [muuntaja-mw/format-middleware]}})
   (ring/create-default-handler)))

(defn -main [& _]
  (let [uri   (env "DATOMIC_URI"
                   "datomic:sql://kidlisp?jdbc:postgresql://localhost:5432/datomic")
        port  (Integer/parseInt (env "PORT" "8891"))
        conn  (db/connect uri)]
    (schema/ensure! conn)
    ;; wrap-params populates :query-params (string-keyed) from the URL.
    (http/run-server (params/wrap-params (app-router conn))
                     {:port port :ip "127.0.0.1"})
    (println (str "kidlisp-sidecar listening on 127.0.0.1:" port))))
