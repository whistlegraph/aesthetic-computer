// Session
// Produces a valid URL for a given session backend.

/* #region todo 📓 
+ Done
- [x] Add a "local" redis database also, once it's actually necessary...
  (It should work, just gotta make sure Redis is runnin')
  - https://redis.io/docs/getting-started
  - https://github.com/redis/node-redis follow these and setup a local server
- [x] Add a SAAS cache to replace "backends" maybe redis? 
  - [x] How to set a grouping / hashmap for "backends" so that they contain an
        association between the jamsocket URLs and a slug?
  - [x] How to view all keys in redis database / connect via terminal?
- [x] Produce a local URL when in development.
#endregion */

import { createClient } from "redis";

const dev = process.env.NETLIFY_DEV;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

async function fun(event, context) {
  let out,
    status = 200;
  if (dev && parseInt(event.queryStringParameters.forceProduction) !== 1) {
    const host = event.headers.host.split(":")[0];
    out = { url: `http://${host}:8889` };
  } else {

    const { got } = await import("got");
    const slug = event.path.replace("/session/", ""); // Take everything after the path.
    const jamSocketToken = process.env.JAMSOCKET_ACCESS_TOKEN;
    out = {};

    // rep.header("Access-Control-Allow-Origin", corsOrigin);

    // 1. Check to see if we actually should make a backend.
    if (slug.length === 0) {
      status = 500;
      out = { msg: "😇 Sorry. No backend could be spawned!" };
    }

    // Check to see if an "existing" backend for this slug is still alive.

    // Connect to redis...
    const client = !dev
      ? createClient({ url: redisConnectionString })
      : createClient();
    client.on("error", (err) => console.log("🔴 Redis client error!", err));
    await client.connect();

    // Check to see if a backend is already available...
    const currentBackend = await client.HGET("backends", slug);

    if (currentBackend) {
      out = await got(
        `https://api.jamsocket.com/backend/${currentBackend}/status`
      ).json();
      out.url = `https://${currentBackend}.jamsocket.run`; // Tack on the URL for the client.
    }

    console.log("Current backend:", out);

    if (out?.state !== "Ready") {
      // Make a new session backend if one doesn't already exist.
      const session = await got
        .post({
          url: "https://api.jamsocket.com/user/jas/service/session-server/spawn",
          json: { grace_period_seconds: 60 }, // jamsocket api settings
          headers: { Authorization: `Bearer ${jamSocketToken}` },
        })
        .json(); // Note: A failure will yield a 500 code here to the client.
      await client.HSET("backends", slug, session.name); // Store the session name in redis using the 'slug' key.
      out = session;
    }

    await client.quit(); // Disconnect from redis client.

    // TOOD: Pull from redis or database. 🔴
    // else return { ...backends[slug], preceding: true }; // Or return a cached one and mark it as preceding.
  }

  return {
    statusCode: status,
    body: JSON.stringify(out),
  };
}

export const handler = fun;
