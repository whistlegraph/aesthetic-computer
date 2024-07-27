// Session
// Produces a valid URL for a given session backend.

/* #region todo 📓 
+ Done
- [x] Fix simultaneous joins in this implementation and also
       implement jamsocket's locks function.
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

// import { createClient } from "redis";

const dev = process.env.NETLIFY_DEV;
// const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

const udpUrl = `https://udp.aesthetic.computer`;

async function fun(event, context) {
  let out,
    status = 200,
    forceProd = parseInt(event.queryStringParameters.forceProduction) === 1;

  if (dev && !forceProd) {

    let host = event.headers.host.split(":")[0];

    if (host === "local.aesthetic.computer") host = "session." + host;

    out = { url: `http://${host}:8889`, udp: `http://${host}:8889` };

    console.log("OUT:", out);

  } else if (event.queryStringParameters.service === "monolith") {
    out = {
      url: `https://session-server.aesthetic.computer`,
      udp: udpUrl,
      state: "Ready",
    };
  } else {
    const { got } = await import("got");
    const slug = event.path.replace("/session/", ""); // Take everything after the path.
    const jamSocketToken = process.env.JAMSOCKET_ACCESS_TOKEN;
    out = {};

    // rep.header("Access-Control-Allow-Origin", corsOrigin);

    // 1. Check to see if we actually should make a backend.
    if (slug.length === 1) {
      status = 500;
      out = { msg: "😇 Sorry. No backend could be spawned!" };
    }

    // Check to see if an "existing" backend for this slug is still alive.

    // Connect to redis...
    // const client = !dev
    // ? createClient({ url: redisConnectionString })
    // : createClient();
    // client.on("error", (err) => console.log("🔴 Redis client error!", err));
    // await client.connect();

    // Check to see if a backend is already available...
    // const currentBackend = await client.HGET("backends", slug);

    // console.log("🫂  Current backend:", currentBackend);

    // if (currentBackend) {
    // try {
    //   out = await got(
    //     `https://api.jamsocket.com/backend/${currentBackend}/status`,
    //   ).json();
    //   out.url = `https://${currentBackend}.jamsocket.run`; // Add URL for client.
    //   out.udp = udpUrl;
    //   // console.log("Out:", out);
    // } catch (err) {
    //   console.error("🔴 Error:", err);
    //   status = 500;
    //   out = err;
    // }
    // }

    // if (out?.state !== "Ready") {
    // Make a new session backend if one doesn't already exist.
    try {
      console.log("🟡 Spawning a new session for:", slug);
      const session = await got
        .post({
          url: "https://api.jamsocket.com/user/jas/service/session-server/spawn",
          json: { grace_period_seconds: 60, lock: slug }, // jamsocket api settings
          headers: { Authorization: `Bearer ${jamSocketToken}` },
        })
        .json(); // Note: A failure will yield a 500 code here to the client.

      // console.log("🫂 Session:", session);
      // await client.HSET("backends", slug, session.name); // Store the session name in redis using the 'slug' key.

      console.log("Session:", session);
      out = session;
      out.udp = udpUrl;
    } catch (err) {
      // console.error("🔴 Error:", err);
      status = 500;
      out = err;
    }
    // }

    // await client.quit(); // Disconnect from redis client.
  }

  return {
    statusCode: status,
    body: JSON.stringify(out),
    headers: { "Access-Control-Allow-Origin": "*" },
  };
}

export const handler = fun;
