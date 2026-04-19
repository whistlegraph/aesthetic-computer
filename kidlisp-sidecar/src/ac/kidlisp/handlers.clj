(ns ac.kidlisp.handlers
  "Public kidlisp endpoints called by the AC backend (Netlify functions).
   The external Mongo-era response shape is preserved by the Node compat
   layer (system/netlify/functions/store-kidlisp.mjs); this sidecar returns
   clean, Datomic-native shapes."
  (:require [datomic.api :as d]
            [ac.kidlisp.db :as db]))

;; ─────────────────── helpers ───────────────────

(defn- ok     [body] {:status 200 :body body})
(defn- created [body] {:status 201 :body body})
(defn- bad    [msg] {:status 400 :body {:error msg}})
(defn- not-found [] {:status 404 :body {:error "not-found"}})

(defn- now-inst [] (java.util.Date.))

(defn- ->user-ref
  "Upserts a user entity by sub and returns a lookup ref usable in a
   following tx data map."
  [sub]
  (when sub [:user/sub sub]))

(defn- piece->map
  "Entity -> API response map. Pulls component children as well."
  [e]
  (let [m (into {} e)]
    (cond-> {:code         (:kidlisp/code m)
             :hash         (:kidlisp/hash m)
             :source       (:kidlisp/source m)
             :when         (:kidlisp/created-at m)
             :lastAccessed (:kidlisp/last-accessed m)
             :hits         (or (:kidlisp/hits m) 0)
             :user         (get-in m [:kidlisp/author :user/sub])
             :atproto      (when-let [rk (:kidlisp/atproto-rkey m)]
                             {:rkey rk})}
      (:kidlisp/ipfs-media m)
      (assoc :ipfsMedia
             (let [im (:kidlisp/ipfs-media m)]
               {:artifactUri  (:ipfs/artifact-uri im)
                :thumbnailUri (:ipfs/thumbnail-uri im)
                :sourceHash   (:ipfs/source-hash im)
                :createdAt    (:ipfs/created-at im)
                :authorHandle (:ipfs/author-handle im)
                :depCount     (:ipfs/dep-count im)
                :packDate     (:ipfs/pack-date im)}))

      (seq (:kidlisp/keeps m))
      (assoc :keeps
             (mapv (fn [k]
                     {:tokenId         (:keep/token-id k)
                      :network         (:keep/network k)
                      :txHash          (:keep/tx-hash k)
                      :contractAddress (:keep/contract-address k)
                      :contractProfile (:keep/contract-profile k)
                      :contractVersion (:keep/contract-version k)
                      :keptAt          (:keep/kept-at k)
                      :keptBy          (:keep/kept-by k)
                      :walletAddress   (:keep/wallet-address k)
                      :artifactUri     (:keep/artifact-uri k)
                      :thumbnailUri    (:keep/thumbnail-uri k)
                      :metadataUri     (:keep/metadata-uri k)
                      :source          (:keep/source k)})
                   (:kidlisp/keeps m)))

      (:kidlisp/tezos-state m)
      (assoc :tezos
             (let [t (:kidlisp/tezos-state m)]
               {:minted          (:tezos/minted t)
                :exists          (:tezos/exists t)
                :tokenId         (:tezos/token-id t)
                :txHash          (:tezos/tx-hash t)
                :creatorAddress  (:tezos/creator-address t)
                :codeHash        (:tezos/code-hash t)
                :network         (:tezos/network t)
                :mintedAt        (:tezos/minted-at t)
                :checkedAt       (:tezos/checked-at t)
                :attemptedAt     (:tezos/attempted-at t)
                :failedAt        (:tezos/failed-at t)
                :reason          (:tezos/reason t)
                :error           (:tezos/error t)}))

      (:kidlisp/pending-rebake m)
      (assoc :pendingRebake
             (let [r (:kidlisp/pending-rebake m)]
               {:artifactUri     (:rebake/artifact-uri r)
                :thumbnailUri    (:rebake/thumbnail-uri r)
                :metadataUri     (:rebake/metadata-uri r)
                :createdAt       (:rebake/created-at r)
                :contractAddress (:rebake/contract-address r)
                :contractProfile (:rebake/contract-profile r)
                :contractVersion (:rebake/contract-version r)})))))

(defn- entity-by-code [db code]
  (when-let [eid (d/entid db [:kidlisp/code code])]
    (d/entity db eid)))

;; ─────────────────── handlers ───────────────────

(defn- parse-inst [v]
  (cond
    (inst? v) v
    (string? v) (java.util.Date/from (java.time.Instant/parse v))
    (number? v) (java.util.Date. (long v))
    :else nil))

(defn create
  "POST /kidlisp
   body: {source, hash, code, user_sub?, forked_from?, when?, hits?}
   Dedup by hash: if an entity with the given hash exists, bump hits and
   return its existing code. Otherwise insert with the proposed code.

   `when` and `hits` are optional — present during backfill so historical
   timestamps and hit counts are preserved. Absent during normal writes,
   in which case server time / hits=1 is used."
  [conn]
  (fn [req]
    (let [{:keys [source hash code user_sub forked_from when hits]}
          (:body-params req)]
      (if (or (nil? source) (nil? hash) (nil? code))
        (bad "source, hash, code are required")
        (let [db (d/db conn)]
          (if-let [existing-eid (d/entid db [:kidlisp/hash hash])]
            (let [existing (d/entity db existing-eid)]
              (db/transact conn
                           [[:db/add existing-eid :kidlisp/hits
                             (inc (or (:kidlisp/hits existing) 0))]
                            [:db/add existing-eid :kidlisp/last-accessed
                             (now-inst)]])
              (ok {:code (:kidlisp/code existing) :cached true}))
            (let [created-at (or (parse-inst when) (now-inst))
                  piece-tx
                  (cond-> {:kidlisp/code         code
                           :kidlisp/hash         hash
                           :kidlisp/source       source
                           :kidlisp/created-at   created-at
                           :kidlisp/last-accessed created-at
                           :kidlisp/hits         (or hits 1)}
                    user_sub
                    (assoc :kidlisp/author {:user/sub user_sub})

                    forked_from
                    (assoc :kidlisp/forked-from [:kidlisp/code forked_from]))]
              (db/transact conn [piece-tx])
              (created {:code code :cached false}))))))))

(defn lookup-code
  "GET /kidlisp/:code — returns full entity, increments hits."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          db   (d/db conn)]
      (if-let [e (entity-by-code db code)]
        (do
          (db/transact conn
                       [[:db/add (:db/id e) :kidlisp/hits
                         (inc (or (:kidlisp/hits e) 0))]
                        [:db/add (:db/id e) :kidlisp/last-accessed
                         (now-inst)]])
          (ok (piece->map e)))
        (not-found)))))

(defn lookup-hash
  "GET /kidlisp/hash/:hash — dedup lookup (does not increment hits)."
  [conn]
  (fn [req]
    (let [hsh (get-in req [:path-params :hash])
          db  (d/db conn)]
      (if-let [eid (d/entid db [:kidlisp/hash hsh])]
        (ok (piece->map (d/entity db eid)))
        (not-found)))))

(defn batch-lookup
  "POST /kidlisp/lookup
   body: {codes: [...]}
   Returns {results: {code -> entity|null}, summary: {...}}
   Increments hits for found codes."
  [conn]
  (fn [req]
    (let [codes (get-in req [:body-params :codes])]
      (if-not (and (sequential? codes) (seq codes))
        (bad "codes must be a non-empty array")
        (let [db    (d/db conn)
              found (into {} (for [c codes
                                   :let [e (entity-by-code db c)]
                                   :when e]
                               [c (piece->map e)]))
              missing (vec (remove found codes))
              tx      (for [c (keys found)
                            :let [e (entity-by-code db c)]]
                        [:db/add (:db/id e) :kidlisp/hits
                         (inc (or (:kidlisp/hits e) 0))])]
          (when (seq tx) (db/transact conn (vec tx)))
          (ok {:results (merge (into {} (for [c missing] [c nil])) found)
               :summary {:requested    (count codes)
                         :found        (count found)
                         :missing      (count missing)
                         :foundCodes   (vec (keys found))
                         :missingCodes missing}}))))))

(defn list-codes
  "GET /kidlisp?since=...&limit=...&sort=recent|hits&handle=...&codes=...
   Returns {recent: [...], count, limit}. `handle` filter matched against
   the author's handle — but this sidecar does not store handles; the
   Node compat layer joins @handles from Mongo server-side."
  [conn]
  (fn [req]
    (let [{:strs [limit sort since]} (:query-params req)
          limit  (min 100000 (max 1 (Integer/parseInt (or limit "50"))))
          db     (d/db conn)
          since-inst (when (seq since) (java.util.Date/from (java.time.Instant/parse since)))
          q      (if since-inst
                   '[:find [?e ...]
                     :in $ ?since
                     :where
                     [?e :kidlisp/created-at ?when]
                     [(> ?when ?since)]]
                   '[:find [?e ...]
                     :where
                     [?e :kidlisp/code]])
          eids   (if since-inst (d/q q db since-inst) (d/q q db))
          pieces (->> eids
                      (map #(d/entity db %))
                      (sort-by (case sort
                                 "hits" #(- (or (:kidlisp/hits %) 0))
                                 #(- (.getTime ^java.util.Date (:kidlisp/created-at %)))))
                      (take limit)
                      (map piece->map))]
      (ok {:recent (vec pieces)
           :count  (count pieces)
           :limit  limit}))))

(defn stats-functions
  "GET /kidlisp/stats/functions?limit=5000
   Corpus-wide aggregation: scans top-N pieces by hits and tallies
   function-call usage. Implementation mirrors the logic in the existing
   store-kidlisp.mjs handler. For v1 we return raw (code, source, hits)
   tuples and let the Node compat layer do the tallying — keeps the
   sidecar simple and reuses the existing JS tokenizer regex."
  [conn]
  (fn [req]
    (let [{:strs [limit]} (:query-params req)
          limit (min 100000 (Integer/parseInt (or limit "5000")))
          db    (d/db conn)
          rows  (d/q '[:find ?src ?hits
                       :where
                       [?e :kidlisp/source ?src]
                       [?e :kidlisp/hits ?hits]]
                     db)]
      (ok {:docs (->> rows
                      (sort-by second >)
                      (take limit)
                      (map (fn [[src hits]] {:source src :hits hits})))}))))

(defn- update-piece
  "Shared utility: set multi-attribute component entity on a piece by code.
   `sub-attrs` is a map of sub-attribute (e.g. :keep/token-id) -> value.
   `parent-attr` is the ref attribute on the piece (e.g. :kidlisp/keeps).
   `cardinality` is :one or :many."
  [conn code parent-attr cardinality sub-attrs]
  (let [db (d/db conn)]
    (if-let [piece-eid (d/entid db [:kidlisp/code code])]
      (let [new-eid (d/tempid :db.part/user)
            ent-map (assoc sub-attrs :db/id new-eid)
            op      (if (= cardinality :many)
                      [:db/add piece-eid parent-attr new-eid]
                      [:db/add piece-eid parent-attr new-eid])]
        (db/transact conn [ent-map op])
        true)
      false)))

(defn record-mint
  "POST /kidlisp/:code/mint — appends a keep record."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          {:keys [tokenId network txHash contractAddress contractProfile
                  contractVersion keptAt keptBy walletAddress artifactUri
                  thumbnailUri metadataUri source]}
          (:body-params req)]
      (if (and tokenId contractAddress)
        (if (update-piece conn code :kidlisp/keeps :many
                          (cond-> {}
                            tokenId         (assoc :keep/token-id tokenId)
                            network         (assoc :keep/network network)
                            txHash          (assoc :keep/tx-hash txHash)
                            contractAddress (assoc :keep/contract-address contractAddress)
                            contractProfile (assoc :keep/contract-profile contractProfile)
                            contractVersion (assoc :keep/contract-version contractVersion)
                            keptAt          (assoc :keep/kept-at (java.util.Date/from
                                                                  (java.time.Instant/parse keptAt)))
                            keptBy          (assoc :keep/kept-by keptBy)
                            walletAddress   (assoc :keep/wallet-address walletAddress)
                            artifactUri     (assoc :keep/artifact-uri artifactUri)
                            thumbnailUri    (assoc :keep/thumbnail-uri thumbnailUri)
                            metadataUri     (assoc :keep/metadata-uri metadataUri)
                            source          (assoc :keep/source source)))
          (ok {:ok true})
          (not-found))
        (bad "tokenId and contractAddress required")))))

(defn- merge-component
  "Replaces-or-creates a cardinality/one component. Sub-attrs are keyed by
   Datomic keyword; nil values are skipped."
  [conn code parent-attr sub-attrs]
  (let [db (d/db conn)]
    (if-let [piece-eid (d/entid db [:kidlisp/code code])]
      (let [cleaned (into {} (remove (comp nil? val) sub-attrs))
            tx      (if (seq cleaned)
                      [(assoc cleaned :db/id "new")
                       [:db/add piece-eid parent-attr "new"]]
                      [])]
        (when (seq tx) (db/transact conn tx))
        true)
      false)))

(defn set-tezos-state
  "POST /kidlisp/:code/tezos-state — replaces the legacy tezos summary."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          b    (:body-params req)]
      (if (merge-component conn code :kidlisp/tezos-state
                           {:tezos/minted          (:minted b)
                            :tezos/exists          (:exists b)
                            :tezos/token-id        (:tokenId b)
                            :tezos/tx-hash         (:txHash b)
                            :tezos/creator-address (:creatorAddress b)
                            :tezos/code-hash       (:codeHash b)
                            :tezos/network         (:network b)
                            :tezos/reason          (:reason b)
                            :tezos/error           (:error b)})
        (ok {:ok true})
        (not-found)))))

(defn set-pending-rebake
  "POST /kidlisp/:code/pending-rebake — replaces the pending rebake blob."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          b    (:body-params req)]
      (if (merge-component conn code :kidlisp/pending-rebake
                           {:rebake/artifact-uri     (:artifactUri b)
                            :rebake/thumbnail-uri    (:thumbnailUri b)
                            :rebake/metadata-uri     (:metadataUri b)
                            :rebake/contract-address (:contractAddress b)
                            :rebake/contract-profile (:contractProfile b)
                            :rebake/contract-version (:contractVersion b)})
        (ok {:ok true})
        (not-found)))))

(defn set-ipfs-media
  "POST /kidlisp/:code/ipfs-media — replaces the IPFS bundle cache."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          b    (:body-params req)]
      (if (merge-component conn code :kidlisp/ipfs-media
                           {:ipfs/artifact-uri  (:artifactUri b)
                            :ipfs/thumbnail-uri (:thumbnailUri b)
                            :ipfs/source-hash   (:sourceHash b)
                            :ipfs/author-handle (:authorHandle b)
                            :ipfs/dep-count     (:depCount b)})
        (ok {:ok true})
        (not-found)))))

(defn set-atproto-rkey
  "POST /kidlisp/:code/atproto-rkey — records the Bluesky record key."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          rkey (get-in req [:body-params :rkey])
          db   (d/db conn)]
      (if-let [eid (d/entid db [:kidlisp/code code])]
        (do (db/transact conn [[:db/add eid :kidlisp/atproto-rkey rkey]])
            (ok {:ok true}))
        (not-found)))))

(defn lineage
  "GET /kidlisp/:code/lineage — returns {ancestors, descendants} as
   chains of {code, author}. Ancestors walks :kidlisp/forked-from up,
   descendants uses a reverse lookup."
  [conn]
  (fn [req]
    (let [code (get-in req [:path-params :code])
          db   (d/db conn)]
      (if-let [eid (d/entid db [:kidlisp/code code])]
        (let [ancestors
              (loop [cur eid acc []]
                (let [e (d/entity db cur)
                      parent (:kidlisp/forked-from e)]
                  (if parent
                    (recur (:db/id parent)
                           (conj acc {:code   (:kidlisp/code parent)
                                      :author (get-in parent [:kidlisp/author :user/sub])}))
                    acc)))
              descendants
              (vec (d/q '[:find ?code ?sub
                          :in $ ?root
                          :where
                          [?c :kidlisp/forked-from ?root]
                          [?c :kidlisp/code ?code]
                          [?c :kidlisp/author ?a]
                          [?a :user/sub ?sub]]
                        db eid))]
          (ok {:root        code
               :ancestors   ancestors
               :descendants (mapv (fn [[c s]] {:code c :author s}) descendants)}))
        (not-found)))))
