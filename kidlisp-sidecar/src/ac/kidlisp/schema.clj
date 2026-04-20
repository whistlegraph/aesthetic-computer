(ns ac.kidlisp.schema
  "Datomic schema for kidlisp v1. Covers every field that the existing
   store-kidlisp.mjs currently persists in the Mongo `kidlisp` collection,
   so that 'zero Mongo writes for kidlisp' is actually achievable.

   Keep records and mint/attempt events are modeled as their own entities
   referenced via :kidlisp/keeps and :kidlisp/mint-attempts. Legacy Tezos
   state lives alongside for migration fidelity."
  (:require [datomic.api :as d]))

(def schema-v1
  [;; ───────── user ─────────
   {:db/ident       :user/sub
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/doc         "Auth0 subject identifier for the author."}

   ;; ───────── kidlisp piece ─────────
   {:db/ident       :kidlisp/code
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/doc         "Nanoid slug — the $code a user types."}

   {:db/ident       :kidlisp/hash
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique      :db.unique/identity
    :db/doc         "SHA-256 of trimmed source. Unique: dedup."}

   {:db/ident       :kidlisp/source
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc         "Source code text."}

   {:db/ident       :kidlisp/author
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc         "Ref to :user/sub entity. Nullable (anonymous caches)."}

   {:db/ident       :kidlisp/created-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/index       true
    :db/doc         "Mirrors existing Mongo `when` field."}

   {:db/ident       :kidlisp/last-accessed
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one
    :db/noHistory   true
    :db/doc         "Updated on every GET. :db/noHistory prevents the tx
                     log from filling with access events — history queries
                     won't see past values, only the current one. v2 will
                     migrate this to Redis entirely."}

   {:db/ident       :kidlisp/hits
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/noHistory   true
    :db/doc         "Access counter. :db/noHistory for same reason as
                     :kidlisp/last-accessed."}

   {:db/ident       :kidlisp/forked-from
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc         "Parent piece ref — enables lineage queries."}

   ;; ───────── ATProto sync ─────────
   {:db/ident       :kidlisp/atproto-rkey
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc         "Bluesky record key after mirror-to-PDS completes."}

   ;; ───────── IPFS media (bundle cache) ─────────
   {:db/ident       :kidlisp/ipfs-media
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc         "Component entity: cached IPFS artifacts."}

   {:db/ident       :ipfs/artifact-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :ipfs/thumbnail-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :ipfs/source-hash
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :ipfs/created-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :ipfs/author-handle
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :ipfs/dep-count
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident       :ipfs/pack-date
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}

   ;; ───────── Keep records (plural; contract-keyed) ─────────
   {:db/ident       :kidlisp/keeps
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true
    :db/doc         "Zero or more on-chain keep/mint records, one per
                     contract+tokenId+network combination."}

   {:db/ident       :keep/token-id
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/index       true}
   {:db/ident       :keep/network
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/tx-hash
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/contract-address
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/index       true}
   {:db/ident       :keep/contract-profile
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/contract-version
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/kept-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/kept-by
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/wallet-address
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/artifact-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/thumbnail-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/metadata-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :keep/source
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc         "Origin of the record: kept | legacy_tezos | contract_keyed | unknown"}

   ;; ───────── Legacy Tezos summary (single-piece field) ─────────
   {:db/ident       :kidlisp/tezos-state
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc         "Mirrors the current `tezos` object on Mongo docs —
                     mint-attempted/exists/error/skipped summary."}

   {:db/ident       :tezos/minted
    :db/valueType   :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/exists
    :db/valueType   :db.type/boolean
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/token-id
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/tx-hash
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/creator-address
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/code-hash
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/network
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/minted-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/checked-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/attempted-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/failed-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/reason
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :tezos/error
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}

   ;; ───────── Pending rebake ─────────
   {:db/ident       :kidlisp/pending-rebake
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc         "State after a rebake that is not yet reflected on chain."}

   {:db/ident       :rebake/artifact-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :rebake/thumbnail-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :rebake/metadata-uri
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :rebake/created-at
    :db/valueType   :db.type/instant
    :db/cardinality :db.cardinality/one}
   {:db/ident       :rebake/contract-address
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :rebake/contract-profile
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident       :rebake/contract-version
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one}

   ;; ───────── AST (v2) ─────────
   ;; Parsed structure of a kidlisp piece. One entity per node — flat
   ;; list linked via :ast/parent refs. Source of truth is the
   ;; JS parser in system/backend/kidlisp-ast.mjs; the sidecar just
   ;; persists what it's told.
   ;;
   ;; :ast/piece is indexed so "all nodes for piece X" is fast.
   ;; :ast/op is indexed for structural queries like "all pieces using wipe".

   {:db/ident       :kidlisp/ast-nodes
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true
    :db/doc         "Component backref: all AST nodes that belong to this
                     piece. Retracting the piece retracts its AST."}

   {:db/ident       :ast/piece
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/index       true
    :db/doc         "The kidlisp piece this node belongs to. Every node
                     carries this so corpus-wide structural queries can
                     roll up to pieces cheaply."}

   {:db/ident       :ast/kind
    :db/valueType   :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/index       true
    :db/doc         ":call | :atom | :number | :string | :ref | :timing |
                     :fade | :comment"}

   {:db/ident       :ast/op
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/index       true
    :db/doc         "The head symbol of a :call node (e.g. \"wipe\",
                     \"repeat\"). Absent for atoms/literals."}

   {:db/ident       :ast/literal
    :db/valueType   :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc         "The raw literal for :atom, :number, :string, :ref,
                     :timing, :fade, :comment nodes. Stored as string
                     for simple text search; numbers keep their source
                     form (\"1.5\")."}

   {:db/ident       :ast/parent
    :db/valueType   :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/doc         "Parent node ref. Nil for the piece's top-level
                     sequence of forms."}

   {:db/ident       :ast/position
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc         "Zero-based index within the parent's children.
                     Enables ordered children queries."}

   {:db/ident       :ast/depth
    :db/valueType   :db.type/long
    :db/cardinality :db.cardinality/one
    :db/doc         "Nesting depth from the piece root. Useful for
                     quick `wiggle-inside-repeat` style patterns —
                     pair with op + ancestor walks."}])

(defn ensure!
  "Idempotently installs any schema entries not yet present. Per-attribute
   check so evolving the schema over time (e.g. v2 AST attrs) just adds
   the missing pieces on the next sidecar boot."
  [conn]
  (let [db      (d/db conn)
        missing (filter (fn [m]
                          (when-let [ident (:db/ident m)]
                            (nil? (d/entid db ident))))
                        schema-v1)]
    (when (seq missing)
      @(d/transact conn (vec missing))
      (println (str "schema: installed " (count missing) " new attribute(s)")))))
