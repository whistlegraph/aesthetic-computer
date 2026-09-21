// Key Value, 23.08.15.20.29
// Implemented in `redis`.

import { createClient } from "redis";

const redisConnectionString = process.env.REDIS_CONNECTION_STRING;
const dev = process.env.NETLIFY_DEV;

let client;
let clientPromise;
let closingPromise;

function waitUntilReady(connection) {
  return new Promise((resolve, reject) => {
    const cleanup = () => {
      connection.off("ready", ready);
      connection.off("end", ended);
      connection.off("error", failed);
    };
    const ready = () => { cleanup(); resolve(); };
    const ended = () => {
      cleanup();
      reject(new Error("Redis connection closed before it was ready"));
    };
    // Transient errors are followed by automatic reconnection. Only reject
    // when Redis has stopped retrying, or the owner explicitly closes it.
    const failed = (error) => {
      if (!connection.isOpen) { cleanup(); reject(error); }
    };
    connection.on("ready", ready);
    connection.on("end", ended);
    connection.on("error", failed);
    if (connection.isReady) ready();
    else if (!connection.isOpen) ended();
  });
}

async function connect() {
  if (closingPromise) await closingPromise;
  if (clientPromise) return clientPromise;
  if (client?.isReady) return;

  const connecting = client?.isOpen ? client : (!dev
    ? createClient({ url: redisConnectionString })
    : createClient());
  const reconnecting = connecting === client;
  client = connecting;
  if (!reconnecting) {
    connecting.on("error", (err) => console.log("🔴 Redis client error!", err));
  }
  // isOpen becomes true before the first handshake, and stays true during
  // automatic reconnection. Both states must wait for readiness.
  const pending = Promise.resolve()
    .then(async () => {
      if (client !== connecting) throw new Error("Redis connection was closed");
      if (reconnecting) await waitUntilReady(connecting);
      else await connecting.connect();
      if (client !== connecting || !connecting.isReady) {
        throw new Error("Redis connection closed before it was ready");
      }
    })
    .catch((err) => {
      if (client === connecting) client = undefined;
      if (connecting.isOpen) connecting.destroy();
      throw err;
    })
    .finally(() => {
      if (clientPromise === pending) clientPromise = undefined;
    });
  clientPromise = pending;
  return pending;
}

// Request handlers share this singleton in Lith. Finishing one request must
// not close the connection underneath another (including handle lookups).
// One-shot scripts can call closeConnection(), or opt into AC_KV_CLOSE=1.
async function disconnect() {
  if (process.env.AC_KV_CLOSE === "1") await closeConnection();
}

async function closeConnection() {
  if (closingPromise) return closingPromise;
  const closing = client;
  client = undefined;
  clientPromise = undefined;
  if (!closing?.isOpen) return;
  // An unavailable server may retry indefinitely. Shutdown must not wait for
  // that handshake or send QUIT into its offline command queue.
  const pending = Promise.resolve()
    .then(() => closing.isReady ? closing.quit() : closing.destroy())
    .finally(() => {
      if (closingPromise === pending) closingPromise = undefined;
    });
  closingPromise = pending;
  return pending;
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

// Publish via redis.
async function pub(channel, message) {
  try {
    await client.publish(channel, message);
    console.log("Published:", channel, message);
  } catch (err) {
    console.log("🙅‍♂️ Could not publish:", channel, err);
  }
  return true;
}

export { connect, get, set, del, pub, disconnect, closeConnection };
