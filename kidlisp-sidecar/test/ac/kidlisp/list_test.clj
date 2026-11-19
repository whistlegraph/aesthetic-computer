(ns ac.kidlisp.list-test
  (:require [clojure.test :refer [deftest is]]
            [datomic.api :as d]
            [ac.kidlisp.schema :as schema]
            [ac.kidlisp.handlers :as h]))

(defn- codes [res] (mapv :code (get-in res [:body :recent])))

(deftest lists-sort-by-recency-and-hits-and-honour-limit-and-since
  (let [uri (str "datomic:mem://list-" (random-uuid))]
    (d/create-database uri)
    (let [conn (d/connect uri)]
      (try
        (schema/ensure! conn)
        @(d/transact conn
           [{:kidlisp/code "old" :kidlisp/hash "h1" :kidlisp/source "red"
             :kidlisp/created-at #inst "2025-01-01" :kidlisp/hits 9}
            {:kidlisp/code "mid" :kidlisp/hash "h2" :kidlisp/source "blue"
             :kidlisp/created-at #inst "2025-06-01"}
            {:kidlisp/code "new" :kidlisp/hash "h3" :kidlisp/source "lime"
             :kidlisp/created-at #inst "2026-01-01" :kidlisp/hits 3}])
        (let [list (h/list-codes conn)]
          (is (= ["new" "mid" "old"] (codes (list {:query-params {}}))))
          (is (= ["old" "new" "mid"] (codes (list {:query-params {"sort" "hits"}}))))
          (is (= ["new" "mid"] (codes (list {:query-params {"limit" "2"}}))))
          (is (= ["new"] (codes (list {:query-params {"since" "2025-09-01T00:00:00Z"}}))))
          (let [mid (second (get-in (list {:query-params {}}) [:body :recent]))]
            (is (= 0 (:hits mid)))
            (is (= "blue" (:source mid)))))
        (finally (d/release conn) (d/delete-database uri))))))
