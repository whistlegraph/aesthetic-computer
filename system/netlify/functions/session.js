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
    forceProd = parseInt(event.queryStringParameters?.forceProduction) === 1;

  if (dev && !forceProd) {
    let host = event.headers.host.split(":")[0];

    // Check if we're in GitHub Codespaces
    if (host.includes('github.dev') || host.includes('app.github.dev')) {
      // Extract the codespace name and construct the port-forwarded URL
      const codespaceUrlParts = host.split('.');
      const codespaceBase = codespaceUrlParts[0]; // e.g., "cautious-waffle-ppwqx5vgv5hgwj-8888"
      // Remove the port from the base and add 8889
      const baseWithoutPort = codespaceBase.replace(/-\d+$/, ''); // Remove trailing port
      const sessionUrl = `https://${baseWithoutPort}-8889.${codespaceUrlParts.slice(1).join('.')}`;
      out = { url: sessionUrl, udp: sessionUrl };
    } else if (host === "local.aesthetic.computer") {
      out = { url: `https://session.${host}`, udp: `https://session.${host}` };
    } else {
      // Dev session server uses HTTPS (with local SSL certs)
      out = { url: `https://${host}:8889`, udp: `https://${host}:8889` };
    }

  } else {
    out = {
      url: `https://session-server.aesthetic.computer`,
      udp: udpUrl,
      state: "Ready",
    };
  }

  return {
    statusCode: status,
    body: JSON.stringify(out),
    headers: { 
      "Access-Control-Allow-Origin": "*",
      "Cache-Control": "no-cache, no-store, must-revalidate",
      "Pragma": "no-cache",
      "Expires": "0"
    },
  };
}

export const handler = fun;
