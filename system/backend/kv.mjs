// Key Value, 23.08.15.20.29
// Implemented in `redis`.

import { createClient } from "redis";

const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

const dev = process.env.NETLIFY_DEV;
// 🔥 TODO: Make redis faster / switch out the instance.
// const dev = false;


let client;

async function connect() {
  // Connect to redis...
  client = !dev ? createClient({ url: redisConnectionString }) : createClient();
  client.on("error", (err) => console.log("🔴 Redis client error!", err));
  await client.connect();
}

async function disconnect() {
  await client?.quit();
}

async function set(collection, key, value) {
  await client.HSET(collection, key, value);
}

async function del(collection, key) {
  await client.HDEL(collection, key);
}

async function get(collection, key) {
  return await client.HGET(collection, key);
}

export { connect, get, set, del, disconnect };
