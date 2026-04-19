(ns ac.kidlisp.auth
  "Shared-secret middleware. Two separate secrets:
     CLIENT_SECRET — used by AC Netlify functions hitting /kidlisp/*
     ADMIN_SECRET  — used only by silo server hitting /admin/*
   Keeping them separate means compromising the client secret cannot also
   read admin-level data (history, tx log, arbitrary datalog).")

(defn- env [k]
  (System/getenv k))

(defn- check-header [req secret]
  (= (get-in req [:headers "x-sidecar-secret"]) secret))

(defn client-secret-middleware []
  (fn [handler]
    (fn [req]
      (if (check-header req (env "CLIENT_SECRET"))
        (handler req)
        {:status 401 :body {:error "unauthorized"}}))))

(defn admin-secret-middleware []
  (fn [handler]
    (fn [req]
      (if (check-header req (env "ADMIN_SECRET"))
        (handler req)
        {:status 401 :body {:error "unauthorized"}}))))
