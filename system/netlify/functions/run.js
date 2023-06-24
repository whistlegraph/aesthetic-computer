// Run (Piece)
// Immediately runs an aesthetic.computer piece via a post-request.
// Designed to work alongside `vscode-extension`.

/* #region todo 📓 
  - [] Pieces to not reload twice after button change.
#endregion */

import { createClient } from "redis";

const dev = process.env.NETLIFY_DEV;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

async function fun(event) {
  let status;
  let out;

  if (event.httpMethod !== "POST") {
    status = 405;
    out = { status: "Wrong request type!" };
  } else if (event.httpMethod === "POST" && event.path === "/run") {
    try {
      const body = JSON.parse(event.body);

      // 🚗 TODO
      // Send a redis request or socket message containing the
      // piece code.
      const client = !dev
        ? createClient({ url: redisConnectionString })
        : createClient();
      client.on("error", (err) => console.log("🔴 Redis client error!", err));
      await client.connect();

      //try {
      // await client.SET("code", body);
      console.log("PIECE:", body.piece);
      await client.publish(
        "code",
        JSON.stringify({ piece: body.piece, source: body.source })
      );
      //await client.publish("code", { piece: body.piece, code: body.source });

      // } catch (err) {
      //console.log("Error setting message:", err);
      //}

      //await client.HSET("", "latest", session.name); // Store the session name in redis using the 'slug' key.

      // Log the source and the piece name.
      // TODO: - [] Timestamp the piece and return a number back...

      out = { result: "Piece code received!" };

      return {
        statusCode: 200,
        body: response,
      };
    } catch (err) {
      status = 500;
      out = {
        result: `Error receiving piece code: ${err.message}`,
      };
    }
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
