// Run (Piece)
// Immediately runs an aesthetic.computer piece via a post-request.
// Designed to work alongside `vscode-extension`.

/* #region todo 📓 
  + Done
  - [x] Add a "secret" string for receiving updates on a channel.
#endregion */

import { createClient } from "redis";

const dev = process.env.NETLIFY_DEV;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

async function fun(event) {
  let status;
  let out;

  console.log("Running a piece...");

  // Handle OPTIONS request
  if (event.httpMethod === "OPTIONS") {
    return {
      statusCode: 200,
      headers: {
        "Content-Type": "application/json; charset=utf-8",
        "Access-Control-Allow-Origin": "*", // Specify allowed origins or use '*' for all
        "Access-Control-Allow-Methods": "POST, OPTIONS", // Specify allowed methods
        "Access-Control-Allow-Headers": "Content-Type", // Specify allowed headers
      },
      body: JSON.stringify({ status: "OK" }),
    };
  }

  if (event.httpMethod !== "POST") {
    status = 405;
    out = { status: "Wrong request type!" };
  } else if (event.httpMethod === "POST" && event.path === "/run") {
    const params = event.queryStringParameters;

    console.log("POST running piece...", params);

    try {
      const body = JSON.parse(event.body);
      // Send a redis request or socket message containing the piece code.
      const client = !dev
        ? createClient({ url: redisConnectionString })
        : createClient();
      client.on("error", (err) => console.log("🔴 Redis client error!", err));
      await client.connect();
      await client.publish(
        "code",
        JSON.stringify({
          piece: body.piece,
          source: body.source,
          codeChannel: body.codeChannel,
        }),
      );
      out = { result: "Piece code received!" };
      console.log(out);
      return {
        statusCode: 200,
        headers: {
          "Access-Control-Allow-Origin": "*",
        },
        body: "Reloaded!",
      };
    } catch (err) {
      status = 500;
      out = { result: `Error receiving piece code: ${err.message}` };
      console.log(out);
    }
  }

  return {
    statusCode: status,
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      "Access-Control-Allow-Origin": "*",
    },
    body: JSON.stringify(out),
  };
}

export const handler = fun;
