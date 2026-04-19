-- Datomic Pro storage bootstrap on silo Postgres.
-- Run as the postgres superuser ONCE during first-time bring-up.
-- Replace :password below with the value from vault
-- (aesthetic-computer-vault/kidlisp-datomic/postgres.env.gpg).

CREATE ROLE datomic WITH LOGIN PASSWORD :'password';
CREATE DATABASE datomic OWNER datomic;

-- Datomic creates its own KV table (`datomic_kvs`) inside the `datomic`
-- database on first transactor start. No further schema needed here.

GRANT ALL PRIVILEGES ON DATABASE datomic TO datomic;
