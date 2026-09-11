// Run (Piece)
// Pushes a piece's source onto a code channel, so anything watching that
// channel runs it immediately.
//
// AUTHORIZATION.
//
// This endpoint used to take anyone's word for it. A push carried no token and
// `Access-Control-Allow-Origin: *`, so the only thing standing between a
// stranger and every phone watching a channel was the channel name itself —
// eight base64url characters, kept unguessable on purpose. That made the name a
// secret, and a secret cannot be printed on a scannable rock or live in a URL
// someone reads off a screen.
//
// So ownership moved from the name to the token. A channel written as
// `<handle>/<slug>` belongs to that handle, and only a request carrying that
// handle's access token may push to it. The name is then free to be public,
// which is what lets one address — aesthetic.computer/@handle/slug — be the
// piece, the live channel, and the thing a camera points at, all at once.
//
// Anonymous pushes are gone. The VS Code extension was the last caller making
// them and is retired rather than grandfathered: leaving the door open for it
// would leave it open for everyone, since an anonymous request cannot prove
// which tool sent it.

import { createClient } from "redis";
import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";

const dev = process.env.NETLIFY_DEV;
const redisConnectionString = process.env.REDIS_CONNECTION_STRING;

// Auth0 can be slow enough to feel like a hang on a save-triggered push, and a
// push that arrives late is worse than one that reports failure promptly.
const AUTH_TIMEOUT_MS = 3000;

const CORS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Methods": "POST, OPTIONS",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
};

function reply(statusCode, body, extra = {}) {
  return {
    statusCode,
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      ...CORS,
      ...extra,
    },
    body: JSON.stringify(body),
  };
}

// The handle a channel belongs to, or "" for a legacy opaque channel. Split on
// the first slash only — a slug may not contain one, so anything after it is
// malformed rather than nested.
function channelOwner(codeChannel) {
  const [handle, slug, ...rest] = String(codeChannel || "").split("/");
  if (!slug || rest.length) return "";
  return handle;
}

async function fun(event) {
  if (event.httpMethod === "OPTIONS") return reply(200, { status: "OK" });
  if (event.httpMethod !== "POST") return reply(405, { status: "Wrong request type!" });

  // A push with no credentials at all. Answer 410 rather than 401: there is no
  // way to retry this request as written, and the caller should stop making it.
  if (!event.headers?.authorization) {
    return reply(410, {
      result:
        "Anonymous pushes to /run are retired. Send an Aesthetic Computer access token as `Authorization: Bearer <token>`.",
    });
  }

  let user;
  try {
    user = await Promise.race([
      authorize(event.headers),
      new Promise((_, reject) =>
        setTimeout(() => reject(new Error("auth timeout")), AUTH_TIMEOUT_MS),
      ),
    ]);
  } catch (err) {
    return reply(503, { result: `Could not verify the token: ${err.message}` });
  }
  if (!user?.sub) return reply(401, { result: "That token is not valid." });

  let body;
  try {
    body = JSON.parse(event.body);
  } catch (err) {
    return reply(400, { result: `Malformed request body: ${err.message}` });
  }

  const owner = channelOwner(body.codeChannel);
  if (!body.codeChannel) return reply(400, { result: "No codeChannel given." });

  // An owned channel is the only kind whose name can be shown in public, so it
  // is the only kind this checks. A legacy opaque channel still needs a valid
  // token, but any signed-in account may push to one: its name is its lock, and
  // nothing in it says whose lock it is.
  if (owner) {
    const handleOrEmail = await getHandleOrEmail(user.sub);
    const handle =
      typeof handleOrEmail === "string" && handleOrEmail.startsWith("@")
        ? handleOrEmail.slice(1)
        : "";
    if (!handle) {
      return reply(403, {
        result: "This account has no @handle, so it cannot own a code channel.",
      });
    }
    if (handle !== owner) {
      return reply(403, {
        result: `@${owner}'s channel cannot be pushed to by @${handle}.`,
      });
    }
  }

  try {
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
    return reply(200, { result: "Piece code received!" });
  } catch (err) {
    return reply(500, { result: `Error receiving piece code: ${err.message}` });
  }
}

export const handler = fun;
