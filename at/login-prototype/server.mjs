import { createServer } from "node:http";
import { randomBytes } from "node:crypto";
import { pathToFileURL } from "node:url";
import { LinkFlow } from "./link-flow.mjs";
import { makeOAuth } from "./oauth.mjs";

const random = () => randomBytes(32).toString("base64url");
const escape = (value) => String(value).replace(/[&<>"']/g, c => ({
  "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;",
})[c]);
const styles = `body{max-width:36rem;margin:10vh auto;padding:1.5rem;font:18px/1.5 system-ui;background:#171423;color:#f7efff}h1{font-size:2rem}input,button{font:inherit;padding:.65rem;border-radius:.35rem}input{display:block;width:90%;margin:.5rem 0 1rem}button{background:#cbffb8;color:#171423;border:0;cursor:pointer}a{color:#c6c6ff}code{overflow-wrap:anywhere}label{display:block}`;
function page(content) {
  return `<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width"><title>Connect Bluesky · AC prototype</title><style>${styles}</style><main><h1>Connect Bluesky</h1><p>Local prototype. Links use a disposable test account; your AC account stays unchanged.</p>${content}</main></html>`;
}

export function createPrototype({ port = 4287, oauth, now = Date.now } = {}) {
  const origin = `http://127.0.0.1:${port}`;
  const { client, stateStore, sessionStore } = oauth ?? makeOAuth(origin);
  const flow = new LinkFlow({ now });
  const browsers = new Map();

  function browser(req) {
    const id = (req.headers.cookie ?? "").split(";").map(s => s.trim()).find(s => s.startsWith("ac-bsky-demo="))?.slice(13);
    const session = browsers.get(id);
    if (session && now() - session.authenticatedAt < 5 * 60_000) return session;
    if (session) flow.disconnect({ ...session, authenticatedAt: now() });
    if (id) browsers.delete(id);
    return undefined;
  }

  const server = createServer(async (req, res) => {
    const html = (status, content) => {
      res.writeHead(status, { "Content-Type": "text/html; charset=utf-8" });
      res.end(page(content));
    };
    const redirect = (url) => { res.writeHead(303, { Location: url }); res.end(); };
    res.setHeader("Cache-Control", "no-store");
    // no-referrer makes Chrome send Origin: null for a native form POST.
    // Preserve same-origin form checks; OAuth callbacks get the stricter policy.
    res.setHeader("Referrer-Policy", "same-origin");
    res.setHeader("X-Content-Type-Options", "nosniff");
    res.setHeader("Content-Security-Policy", "default-src 'none'; style-src 'unsafe-inline'; form-action 'self' https://bsky.social https://at.aesthetic.computer; frame-ancestors 'none'; base-uri 'none'");
    if (req.headers.host !== `127.0.0.1:${port}`) return html(403, "<p>Use the loopback address shown by the server.</p>");
    try {
      const url = new URL(req.url, origin);
      if (url.pathname === "/callback") res.setHeader("Referrer-Policy", "no-referrer");
      let principal = browser(req);
      if (req.method === "GET" && url.pathname === "/") {
        if (!principal) {
          for (const [id, session] of browsers) {
            if (now() - session.authenticatedAt >= 5 * 60_000) {
              // Test identities and links live only for this local session.
              flow.disconnect({ ...session, authenticatedAt: now() });
              browsers.delete(id);
            }
          }
          if (browsers.size >= 100) return html(429, "<p>Too many local sessions.</p>");
          const id = random();
          principal = { subject: `prototype:${id}`, browser: id, csrf: random(), authenticatedAt: now(), locked: false };
          browsers.set(id, principal);
          res.setHeader("Set-Cookie", `ac-bsky-demo=${id}; HttpOnly; SameSite=Lax; Path=/; Max-Age=300`);
        }
        const csrf = `<input type="hidden" name="csrf" value="${principal.csrf}">`;
        const did = flow.bySubject.get(principal.subject);
        return html(200, did
          ? `<p>Connected to this test account:</p><p><code>${escape(did)}</code></p><form method="post" action="/disconnect">${csrf}<button>Disconnect</button></form>`
          : `<form method="post" action="/start">${csrf}<label>Bluesky handle<input name="handle" placeholder="you.bsky.social" required maxlength="253" autocomplete="username"></label><button>Continue to Bluesky</button></form>`);
      }
      if (!principal) return html(401, '<p>Your local session expired. <a href="/">Start again</a>.</p>');
      if (req.method === "GET" && url.pathname === "/callback") {
        const internal = await stateStore.get(url.searchParams.get("state"));
        if (!internal?.appState) throw new Error("Missing or expired OAuth state.");
        flow.pending(internal.appState, principal);
        const { session, state } = await client.callback(url.searchParams);
        try {
          if (state !== internal.appState) throw new Error("OAuth state mismatch.");
          flow.verify(state, principal, session.did);
        } finally {
          // Identity proof only. Do not keep a publishing/refresh session.
          try { await client.revoke(session.did); } catch { /* expired grant */ }
          await sessionStore.del(session.did);
        }
        principal.pending = state;
        return redirect("/confirm");
      }
      if (req.method === "GET" && url.pathname === "/confirm") {
        const tx = flow.pending(principal.pending, principal);
        return html(200, `<p>Bluesky verified this identity:</p><p><code>${escape(tx.did)}</code></p><form method="post" action="/confirm"><input type="hidden" name="csrf" value="${principal.csrf}"><button>Connect to test account</button></form>`);
      }
      if (req.method !== "POST") return html(404, "<p>Page not found.</p>");
      if (req.headers.origin !== origin || !req.headers["content-type"]?.startsWith("application/x-www-form-urlencoded")) {
        return html(403, "<p>Open the local prototype to submit this form.</p>");
      }
      let body = "";
      for await (const chunk of req) {
        body += chunk;
        if (Buffer.byteLength(body) > 4096) return html(413, "<p>Request too large.</p>");
      }
      const form = new URLSearchParams(body);
      if (form.get("csrf") !== principal.csrf) return html(403, "<p>Invalid form token.</p>");
      if (url.pathname === "/start") {
        const handle = (form.get("handle") ?? "").trim().replace(/^@/, "").toLowerCase();
        if (handle.length > 253 || !/^(aesthetic\.computer|[a-z0-9.-]+\.bsky\.social|[a-z0-9.-]+\.at\.aesthetic\.computer)$/.test(handle)) {
          return html(400, "<p>Use a Bluesky or AC PDS handle for this prototype.</p>");
        }
        const state = flow.begin(principal);
        const destination = await client.authorize(handle, { state, scope: "atproto" });
        return redirect(destination.toString());
      }
      if (url.pathname === "/confirm") {
        flow.confirm(principal.pending, principal);
        delete principal.pending;
        return redirect("/");
      }
      if (url.pathname === "/disconnect") {
        flow.disconnect(principal);
        delete principal.pending;
        return redirect("/");
      }
      return html(404, "<p>Page not found.</p>");
    } catch {
      // Callback errors may contain codes or provider details: never echo them.
      return html(400, '<p>Connection could not be completed. It may have expired, been cancelled, or already belong to another test account. <a href="/">Start again</a>.</p>');
    }
  });
  return { server, flow, origin };
}

if (process.argv[1] && import.meta.url === pathToFileURL(process.argv[1]).href) {
  const port = Number(process.env.PORT ?? 4287);
  if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error("Invalid PORT");
  const { server, origin } = createPrototype({ port });
  server.listen(port, "127.0.0.1", () => console.log(`Bluesky connection prototype: ${origin}`));
}
