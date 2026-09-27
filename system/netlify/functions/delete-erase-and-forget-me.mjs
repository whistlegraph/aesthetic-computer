// delete-erase-and-forget-me, 23.12.15.13.44 (rebuilt 26.09.26)

// GET  ?preview (signed in) → what deletion would remove and keep.
// GET  ?export  (signed in) → a JSON copy of what the account made.
// POST (signed in)          → lock the account and schedule its deletion.
// GET  ?restore=TOKEN       → a page with one button that keeps the account.
// POST restore=TOKEN        → keep the account (the emailed link's button).
//
// The purge itself runs later, from lith's timer, through
// backend/account-deletion.mjs. A GET never changes anything, because mail
// scanners open links.

import { authorize } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import {
  exportAccount,
  previewDeletion,
  requestDeletion,
  restoreDeletion,
} from "../../backend/account-deletion.mjs";
import { productionDeps } from "../../backend/account-deletion-deps.mjs";

const page = (title, body) =>
  respond(
    200,
    `<!doctype html><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">` +
      `<title>${title}</title><body style="font:18px/1.5 monospace;max-width:32em;margin:12vh auto;padding:0 16px;background:#fff;color:#000">` +
      `${body}</body>`,
    { "Content-Type": "text/html; charset=utf-8", "Cache-Control": "no-store" },
  );

const attribute = (text) => String(text).replace(/[&<>"']/g, (c) => `&#${c.charCodeAt(0)};`);

function restoreToken(event) {
  const raw = event.body || "";
  try {
    return JSON.parse(raw)?.restore;
  } catch {
    return new URLSearchParams(raw).get("restore");
  }
}

export async function handler(event) {
  const query = event.queryStringParameters || {};
  if (event.httpMethod === "GET" && (query.preview !== undefined || query.export !== undefined)) {
    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "Authorization failure..." });
    let database;
    try {
      database = await connect();
      const deps = await productionDeps(database);
      if (query.export !== undefined) {
        return respond(200, await exportAccount(deps, { user }), {
          "Cache-Control": "no-store",
          "Content-Disposition": 'attachment; filename="aesthetic-computer-export.json"',
        });
      }
      return respond(200, await previewDeletion(deps, { user }), {
        "Cache-Control": "no-store",
      });
    } catch (error) {
      console.error("🪦 Account deletion preview failed:", error);
      return respond(500, { message: "Could not read the account. Please retry." });
    } finally {
      if (database) await database.disconnect();
    }
  }
  if (event.httpMethod === "GET") {
    const token = event.queryStringParameters?.restore;
    if (!token) return respond(405, { message: "Method Not Allowed" });
    return page(
      "Keep your account",
      `<p>Keep your Aesthetic Computer account?</p>` +
        `<form method="post" action="/api/delete-erase-and-forget-me">` +
        `<input type="hidden" name="restore" value="${attribute(token)}">` +
        `<button style="font:inherit;padding:10px 16px">Keep my account</button></form>`,
    );
  }
  if (event.httpMethod !== "POST") return respond(405, { message: "Method Not Allowed" });

  let database;
  try {
    const token = restoreToken(event);
    if (token) {
      database = await connect();
      const result = await restoreDeletion(await productionDeps(database), { token });
      return result.restored
        ? page("Account kept", `<p>Your account is kept. You can sign in again.</p>`)
        : page(
            "Link expired",
            `<p>This link has expired or was already used. If your account was not deleted yet, sign in to check.</p>`,
          );
    }

    const user = await authorize(event.headers);
    if (!user) return respond(401, { message: "Authorization failure..." });

    database = await connect();
    const schedule = await requestDeletion(await productionDeps(database), { user });
    // `result` stays "Deleted!" for clients released before the grace
    // period; to them the account is gone, and for them it is locked.
    return respond(200, { result: "Deleted!", ...schedule });
  } catch (error) {
    console.error("🪦 Account deletion request failed:", error);
    return respond(500, { message: "Account deletion failed. Please retry." });
  } finally {
    if (database) await database.disconnect();
  }
}
