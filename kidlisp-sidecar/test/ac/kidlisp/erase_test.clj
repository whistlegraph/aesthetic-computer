(ns ac.kidlisp.erase-test
  "The account-deletion route against an in-memory Datomic.
   Run: clojure -M:test"
  (:require [clojure.test :refer [deftest is run-tests]]
            [datomic.api :as d]
            [ac.kidlisp.schema :as schema]
            [ac.kidlisp.handlers :as h]))

(defn- fresh-conn []
  (let [uri (str "datomic:mem://erase-" (random-uuid))]
    (d/create-database uri)
    (let [conn (d/connect uri)]
      (schema/ensure! conn)
      conn)))

(defn- seed! [conn]
  @(d/transact conn
               [{:kidlisp/code "gone" :kidlisp/hash "h1" :kidlisp/source "(wipe)"
                 :kidlisp/author {:user/sub "auth0|me"}
                 :kidlisp/ipfs-media {:ipfs/author-handle "@me" :ipfs/artifact-uri "ipfs://a"}}
                {:kidlisp/code "kept" :kidlisp/hash "h2" :kidlisp/source "(ink)"
                 :kidlisp/author {:user/sub "auth0|me"}
                 :kidlisp/keeps [{:keep/token-id 7 :keep/kept-by "auth0|me"}]
                 :kidlisp/ipfs-media {:ipfs/author-handle "@me" :ipfs/artifact-uri "ipfs://b"}}
                {:kidlisp/code "theirs" :kidlisp/hash "h3" :kidlisp/source "(line)"
                 :kidlisp/author {:user/sub "auth0|them"}}]))

(defn- erase [conn body]
  ((h/erase-user conn) {:body-params body}))

;; In-memory Datomic records excision requests but never carries them out
;; (only a transactor's indexing job does), so these tests check the
;; requests the route files; the removal itself is Datomic's job.
(defn- excisions
  "Every excision request as {entity-code-or-kind, attrs-or-:all}."
  [conn]
  (let [db (d/db conn)
        label (fn [eid]
                (let [hist (d/history db)
                      attrs (set (map #(d/ident db (:a %)) (d/datoms hist :eavt eid)))]
                  (cond
                    (attrs :kidlisp/code) (:v (first (d/datoms hist :eavt eid :kidlisp/code)))
                    (attrs :user/sub)     (:v (first (d/datoms hist :eavt eid :user/sub)))
                    (attrs :keep/token-id) :keep
                    (attrs :ipfs/artifact-uri) (str "media:" (:v (first (d/datoms hist :eavt eid :ipfs/artifact-uri))))
                    :else eid)))]
    (set (for [[x] (d/q '[:find ?x :where [?x :db/excise]] db)
               :let [e (d/entity db x)]]
           [(label (:db/id (:db/excise e)))
            (if-let [attrs (seq (:db.excise/attrs e))] (set attrs) :all)]))))

(deftest erase-files-the-right-excisions
  (let [conn (fresh-conn)]
    (seed! conn)
    (let [res (erase conn {:sub "auth0|me" :delete_codes ["gone"] :anonymize_codes ["kept"]})]
      (is (= 200 (:status res)))
      (is (= {:deleted 1 :anonymized 1 :user true} (:body res))))
    (is (= #{["gone" :all]
             ["media:ipfs://a" :all]
             ["auth0|me" :all]
             ["kept" #{:kidlisp/author}]
             [:keep #{:keep/kept-by :ipfs/author-handle}]
             ["media:ipfs://b" #{:keep/kept-by :ipfs/author-handle}]}
           (excisions conn)))
    (let [db (d/db conn)]
      (is (nil? (:kidlisp/author (d/entity db [:kidlisp/code "kept"]))) "kept piece loses its author now")
      (is (= "auth0|them" (-> (d/entity db [:kidlisp/code "theirs"]) :kidlisp/author :user/sub))
          "other people's pieces are untouched"))))

(deftest erase-can-be-retried
  (let [conn (fresh-conn)]
    (seed! conn)
    (erase conn {:sub "auth0|me" :delete_codes ["gone"] :anonymize_codes ["kept"]})
    (let [again (erase conn {:sub "auth0|me" :delete_codes ["gone" "missing"] :anonymize_codes ["kept"]})]
      (is (= 200 (:status again)) "a retry, or an unknown code, is not an error"))))

(deftest erase-needs-a-sub
  (is (= 400 (:status (erase (fresh-conn) {:delete_codes ["x"]})))))
