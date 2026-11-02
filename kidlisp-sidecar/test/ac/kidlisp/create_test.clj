(ns ac.kidlisp.create-test
  (:require [clojure.test :refer [deftest is]]
            [datomic.api :as d]
            [ac.kidlisp.schema :as schema]
            [ac.kidlisp.handlers :as h]))

(deftest concurrent-creates-keep-the-first-code
  (let [uri (str "datomic:mem://create-" (random-uuid))]
    (d/create-database uri)
    (let [conn (d/connect uri)]
      (try
        (schema/ensure! conn)
        (let [start (promise)
              create (h/create conn)
              jobs (doall (for [n (range 24)]
                            (future
                              @start
                              (create {:body-params {:source "(wipe blue)"
                                                     :hash "alice-source"
                                                     :code (str "code" n)
                                                     :user_sub "alice"}}))))]
          (deliver start true)
          (let [results (mapv deref jobs)
                codes (set (map #(get-in % [:body :code]) results))
                entity (d/entity (d/db conn) [:kidlisp/hash "alice-source"])]
            (is (= 1 (count codes)))
            (is (= 1 (count (filter #(= 201 (:status %)) results))))
            (is (= 23 (count (filter #(get-in % [:body :cached]) results))))
            (is (= codes #{(:kidlisp/code entity)}))
            (is (= "alice" (-> entity :kidlisp/author :user/sub)))
            (is (= "(wipe blue)" (:kidlisp/source entity)))))
        (finally (d/release conn) (d/delete-database uri))))))
