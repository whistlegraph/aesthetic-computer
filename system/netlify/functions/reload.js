// Reload
// Sends a request to a user piece's current session backend to reload
// the code for all connected users. 

/* #region todo 📓 
#endregion */

import { createClient } from "redis";

const dev = process.env.NETLIFY_DEV;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

async function fun(event, context) {
  let status;
  let out;
  let result = { status: "unsent" };

  if (event.httpMethod !== "POST") {
    status = 405;
    out = { status: "Wrong request type!" };
  } else {
    status = 200;

    const piece = JSON.parse(event.body).piece;
    let reloading = piece.startsWith("@"); // Only reload prefixed user pieces.

    if (reloading) {
      // 1. Connect to redis...
      const client = !dev
        ? createClient({ url: redisConnectionString })
        : createClient();
      client.on("error", (err) => console.log("🔴 Redis client error!", err));
      await client.connect();

      // 2. Check to see if a backend is already available...
      const currentBackend = await client.HGET("backends", piece);

      // ❤️‍🔥
      // TODO: To make this faster I could just set a value in redis
      //       and then the jamsocket backend could sub to that message. 

      // 3. Send a reload request to the `currentBackend` (ignoring its status).
      if (currentBackend) {
        const { got } = await import("got");
        const base = `https://${currentBackend}.jamsocket.run`;

        result = await got
          .post({ url: base + "/reload", json: { piece } })
          .json();
      } else {
        // Or don't send a request if there is no backend on record in redis.
        reloading = false;
      }
    }

    out = { piece, reloading, result };
  }

  return {
    statusCode: status,
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      //"Access-Control-Allow-Origin": "*",
    },
    body: JSON.stringify(out),
  };
}

export const handler = fun;
