// unsubscribe.mjs, 2026.02.12
// Email blast unsubscribe / resubscribe endpoint.
// GET  ?email=X&token=Y  → confirmation page
// POST {email, token, action} → process unsubscribe/resubscribe

import { createHmac, timingSafeEqual } from "crypto";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";

const COLLECTION = "email-blast-unsubscribes";

let cachedSecret = null;

async function getUnsubscribeSecret(database) {
  if (cachedSecret) return cachedSecret;
  const secrets = await database.db.collection("secrets").findOne({ _id: "email-blast" });
  if (!secrets?.unsubscribeSecret) throw new Error("email-blast secrets not found in database");
  cachedSecret = secrets.unsubscribeSecret;
  return cachedSecret;
}

function generateToken(email, secret) {
  return createHmac("sha256", secret)
    .update(email.toLowerCase().trim())
    .digest("hex");
}

function verifyToken(email, token, secret) {
  try {
    const expected = generateToken(email, secret);
    if (expected.length !== token.length) return false;
    return timingSafeEqual(Buffer.from(expected), Buffer.from(token));
  } catch {
    return false;
  }
}

function html(title, body) {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>${title} — Aesthetic.Computer</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: monospace;
      max-width: 480px;
      margin: 60px auto;
      padding: 0 20px;
      background: #111;
      color: #eee;
      line-height: 1.6;
    }
    h1 { font-size: 1.1em; margin-bottom: 16px; }
    p { margin-bottom: 12px; }
    button {
      background: #222;
      color: #eee;
      border: 1px solid #444;
      padding: 12px 24px;
      font-family: monospace;
      font-size: 1em;
      cursor: pointer;
      margin-top: 8px;
    }
    button:hover { background: #333; border-color: #666; }
    .ok { color: #7c7; }
    .err { color: #c77; }
    a { color: #eee; }
    a:hover { color: #fff; }
    .dot { color: #0ff; }
    .footer { margin-top: 32px; font-size: 0.9em; color: #999; }
    .footer a { color: #999; }
    .footer a:hover { color: #eee; }
    .brand { font-size: 1.1em; margin-bottom: 6px; }
    .brand a { text-decoration: none; color: #eee; }
    .brand a:hover { color: #fff; }
    .tagline { color: #666; font-size: 0.85em; margin-bottom: 4px; }
  </style>
</head>
<body>
  ${body}
  <div class="footer">
    <p class="brand"><a href="https://aesthetic.computer">Aesthetic<span class="dot">.</span>Computer</a></p>
    <p class="tagline">a creative platform for anyone</p>
    <p><a href="https://give.aesthetic.computer">give<span class="dot">.</span>aesthetic<span class="dot">.</span>computer</a></p>
  </div>
</body>
</html>`;
}

function parseBody(event) {
  const contentType = (event.headers["content-type"] || "").toLowerCase();
  if (contentType.includes("application/json")) {
    return JSON.parse(event.body || "{}");
  }
  return Object.fromEntries(new URLSearchParams(event.body || ""));
}

export async function handler(event) {
  if (event.httpMethod === "OPTIONS") return respond(200, {});

  const H = { "Content-Type": "text/html; charset=utf-8" };
  let database;

  try {
    database = await connect();
    const secret = await getUnsubscribeSecret(database);
    const col = database.db.collection(COLLECTION);

    // --- GET: show confirmation page ---
    if (event.httpMethod === "GET") {
      const { email, token } = event.queryStringParameters || {};

      if (!email || !token) {
        return respond(400, html("Error", `<p class="err">Missing email or token.</p>`), H);
      }

      if (!verifyToken(email, token, secret)) {
        return respond(
          403,
          html("Invalid Link", `<h1>invalid link</h1><p class="err">This unsubscribe link is expired or invalid.</p><p>If you need help, email <a href="mailto:me@jas.life">me@jas.life</a></p>`),
          H,
        );
      }

      const existing = await col.findOne({ email: email.toLowerCase().trim() });

      if (existing) {
        return respond(
          200,
          html(
            "Unsubscribed",
            `<h1>you're unsubscribed</h1>
            <p>you won't receive email blasts from aesthetic.computer.</p>
            <p>changed your mind?</p>
            <form method="POST" action="/api/unsubscribe">
              <input type="hidden" name="email" value="${email}">
              <input type="hidden" name="token" value="${token}">
              <input type="hidden" name="action" value="resubscribe">
              <button type="submit">resubscribe</button>
            </form>`,
          ),
          H,
        );
      }

      return respond(
        200,
        html(
          "Unsubscribe",
          `<h1>unsubscribe</h1>
          <p>click below to stop receiving email blasts from aesthetic.computer.</p>
          <form method="POST" action="/api/unsubscribe">
            <input type="hidden" name="email" value="${email}">
            <input type="hidden" name="token" value="${token}">
            <input type="hidden" name="action" value="unsubscribe">
            <button type="submit">unsubscribe</button>
          </form>`,
        ),
        H,
      );
    }

    // --- POST: process action ---
    if (event.httpMethod === "POST") {
      const body = parseBody(event);
      const { email, token, action } = body;

      if (!email || !token || !action) {
        return respond(400, html("Error", `<p class="err">Missing required fields.</p>`), H);
      }

      if (!verifyToken(email, token, secret)) {
        return respond(
          403,
          html("Invalid Link", `<h1>invalid link</h1><p class="err">This link is expired or invalid.</p>`),
          H,
        );
      }

      const normalizedEmail = email.toLowerCase().trim();
      await col.createIndex({ email: 1 }, { unique: true });

      if (action === "unsubscribe") {
        try {
          await col.insertOne({
            email: normalizedEmail,
            unsubscribedAt: new Date(),
          });
        } catch (err) {
          if (err.code !== 11000) throw err;
        }

        return respond(
          200,
          html(
            "Unsubscribed",
            `<h1 class="ok">done!</h1>
            <p>you've been unsubscribed from aesthetic.computer emails.</p>
            <p>changed your mind? <a href="/api/unsubscribe?email=${encodeURIComponent(email)}&token=${token}">resubscribe</a></p>`,
          ),
          H,
        );
      }

      if (action === "resubscribe") {
        await col.deleteOne({ email: normalizedEmail });

        return respond(
          200,
          html(
            "Resubscribed",
            `<h1 class="ok">welcome back!</h1>
            <p>you've been resubscribed to aesthetic.computer emails.</p>`,
          ),
          H,
        );
      }

      return respond(400, html("Error", `<p class="err">Unknown action.</p>`), H);
    }

    return respond(405, html("Error", `<p class="err">Method not allowed.</p>`), H);
  } finally {
    if (database) await database.disconnect();
  }
}
