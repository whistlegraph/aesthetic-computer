// ac-session.mjs — the shared Aesthetic Computer sign-in.
//
// Every AC desktop app reads one file, ~/.ac-token, minted by `ac-login` with
// Auth0 Authorization-Code + PKCE and a loopback callback. This module reads
// and watches that file, refreshes the access token, and can run the same
// sign-in flow itself so Aesthetic Code needs no other checkout. Only the
// handle is ever displayed; email and name stay in the file.
import { EventEmitter } from "node:events";
import { createHash, randomBytes } from "node:crypto";
import { spawn } from "node:child_process";
import { existsSync, mkdirSync, readFileSync, unlinkSync, watch, writeFileSync } from "node:fs";
import { createServer } from "node:http";
import { homedir } from "node:os";
import { basename, dirname, join } from "node:path";

export const AUTH_DOMAIN = "hi.aesthetic.computer";
export const CLIENT_ID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt";
export const CALLBACK_PORT = 44233;
export const SITE = "https://aesthetic.computer";
// Cloudflare fronts aesthetic.computer and rejects a bare user agent (1010).
export const USER_AGENT =
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 " +
  "(KHTML, like Gecko) Chrome/125.0 Safari/537.36 aesthetic-code";

const base64url = (buffer) =>
  buffer.toString("base64").replace(/\+/g, "-").replace(/\//g, "_").replace(/=/g, "");

const LANDING = `<!doctype html><html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1"><title>Signed in · Aesthetic Code</title>
<style>body{margin:0;min-height:100vh;display:flex;align-items:center;justify-content:center;
background:rgb(70,50,100);color:white;font-family:monospace}main{text-align:center;padding:2em}
h1{font-weight:normal;color:rgb(255,100,255)}p{color:rgb(220,180,255)}</style></head>
<body><main><h1>Signed in</h1><p>Return to Aesthetic Code. You can close this tab.</p></main></body></html>`;

export function openInBrowser(url) {
  const command =
    process.env.BROWSER ||
    (process.platform === "darwin" ? "open" : process.platform === "win32" ? "start" : "xdg-open");
  try {
    spawn(command, [url], { stdio: "ignore", detached: true }).unref();
    return true;
  } catch {
    return false;
  }
}

export class ACSession extends EventEmitter {
  constructor({
    file = join(homedir(), ".ac-token"),
    fetch = globalThis.fetch,
    openBrowser = openInBrowser,
    callbackPort = CALLBACK_PORT,
    site = SITE,
    authDomain = AUTH_DOMAIN,
    now = Date.now,
  } = {}) {
    super();
    this.file = file;
    this.fetch = fetch;
    this.openBrowser = openBrowser;
    this.callbackPort = callbackPort;
    this.site = site;
    this.authDomain = authDomain;
    this.now = now;
    this.watchers = [];
    this.debounce = null;
    this.signingIn = false;
  }

  read() {
    try {
      const record = JSON.parse(readFileSync(this.file, "utf8"));
      return record && typeof record === "object" ? record : null;
    } catch {
      return null;
    }
  }

  get handle() {
    return String(this.read()?.user?.handle || "").replace(/^@/, "");
  }

  get signedIn() {
    return Boolean(this.read()?.access_token);
  }

  get state() {
    const record = this.read();
    if (!record?.access_token) return "signed-out";
    return record.user?.handle ? "signed-in" : "no-handle";
  }

  label() {
    const state = this.state;
    if (state === "signed-in") return `@${this.handle}`;
    if (state === "no-handle") return "signed in · no handle";
    return "not signed in";
  }

  // Watch the file and its directory: ac-login rewrites in place (same inode),
  // logout deletes, and an atomic replace swaps the inode. Re-arm after every
  // change so a stale file watcher never goes quiet.
  watch() {
    this.unwatch();
    const fire = () => {
      clearTimeout(this.debounce);
      this.debounce = setTimeout(() => {
        this.watch();
        this.emit("change", this.label());
      }, 80);
      this.debounce.unref?.();
    };
    try {
      const directory = watch(dirname(this.file), (_event, name) => {
        if (!name || name === basename(this.file)) fire();
      });
      directory.unref?.();
      this.watchers.push(directory);
    } catch {}
    if (existsSync(this.file)) {
      try {
        const file = watch(this.file, fire);
        file.unref?.();
        this.watchers.push(file);
      } catch {}
    }
    return this;
  }

  unwatch() {
    for (const watcher of this.watchers) {
      try {
        watcher.close();
      } catch {}
    }
    this.watchers = [];
  }

  // A usable access token, refreshed a minute early through the refresh grant.
  async token() {
    const record = this.read();
    if (!record?.access_token) throw new Error("not signed in — run /login");
    const stale = record.expires_at && this.now() > record.expires_at - 60_000;
    if (!stale) return record.access_token;
    if (!record.refresh_token) throw new Error("session expired — run /login");
    const response = await this.fetch(`https://${this.authDomain}/oauth/token`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        grant_type: "refresh_token",
        client_id: CLIENT_ID,
        refresh_token: record.refresh_token,
      }),
    });
    if (!response.ok) throw new Error(`session refresh failed (HTTP ${response.status}) — run /login`);
    const next = await response.json();
    record.access_token = next.access_token;
    if (next.refresh_token) record.refresh_token = next.refresh_token;
    if (next.id_token) record.id_token = next.id_token;
    record.expires_at = this.now() + (next.expires_in || 3600) * 1000;
    this.#write(record);
    return record.access_token;
  }

  // Authorization-Code + PKCE against Auth0 with a loopback callback. Resolves
  // to the @handle (or null when the account has none yet).
  async login({ forcePrompt = false, timeoutMs = 5 * 60_000, onUrl } = {}) {
    if (this.signingIn) throw new Error("a sign-in is already waiting on the browser");
    this.signingIn = true;
    const verifier = base64url(randomBytes(32));
    const challenge = base64url(createHash("sha256").update(verifier).digest());
    const state = randomBytes(16).toString("hex");
    const server = createServer();
    try {
      const port = await new Promise((resolve, reject) => {
        server.once("error", (error) =>
          reject(
            error.code === "EADDRINUSE"
              ? new Error(`port ${this.callbackPort} is in use — close any running ac-login and retry`)
              : error,
          ),
        );
        server.listen(this.callbackPort, "127.0.0.1", () => resolve(server.address().port));
      });
      const redirectUri = `http://localhost:${port}/callback`;
      const authUrl = new URL(`https://${this.authDomain}/authorize`);
      authUrl.searchParams.set("response_type", "code");
      authUrl.searchParams.set("client_id", CLIENT_ID);
      authUrl.searchParams.set("redirect_uri", redirectUri);
      authUrl.searchParams.set("scope", "openid profile email offline_access");
      authUrl.searchParams.set("state", state);
      authUrl.searchParams.set("code_challenge", challenge);
      authUrl.searchParams.set("code_challenge_method", "S256");
      if (forcePrompt) authUrl.searchParams.set("prompt", "login");

      const code = await new Promise((resolve, reject) => {
        const timer = setTimeout(() => reject(new Error("sign-in timed out")), timeoutMs);
        timer.unref?.();
        const settle = (fn, value) => {
          clearTimeout(timer);
          fn(value);
        };
        server.on("request", (request, response) => {
          const url = new URL(request.url, redirectUri);
          if (url.pathname !== "/callback") {
            response.writeHead(404);
            response.end("Not found");
            return;
          }
          const failure = url.searchParams.get("error");
          if (failure) {
            response.writeHead(400, { "content-type": "text/plain" });
            response.end("Sign-in failed. Return to Aesthetic Code.");
            settle(reject, new Error(url.searchParams.get("error_description") || failure));
            return;
          }
          if (url.searchParams.get("state") !== state) {
            response.writeHead(400, { "content-type": "text/plain" });
            response.end("State mismatch. Return to Aesthetic Code and retry.");
            settle(reject, new Error("sign-in state mismatch — retry /login"));
            return;
          }
          const grant = url.searchParams.get("code");
          if (!grant) {
            response.writeHead(400, { "content-type": "text/plain" });
            response.end("Missing authorization code.");
            settle(reject, new Error("missing authorization code"));
            return;
          }
          response.writeHead(200, { "content-type": "text/html; charset=utf-8" });
          response.end(LANDING);
          settle(resolve, grant);
        });
        onUrl?.(authUrl.toString());
        this.openBrowser(authUrl.toString());
      });

      const exchange = await this.fetch(`https://${this.authDomain}/oauth/token`, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify({
          grant_type: "authorization_code",
          client_id: CLIENT_ID,
          code_verifier: verifier,
          code,
          redirect_uri: redirectUri,
        }),
      });
      if (!exchange.ok) throw new Error(`token exchange failed (HTTP ${exchange.status})`);
      const tokens = await exchange.json();

      const userResponse = await this.fetch(`https://${this.authDomain}/userinfo`, {
        headers: { Authorization: `Bearer ${tokens.access_token}` },
      });
      if (!userResponse.ok) throw new Error(`userinfo failed (HTTP ${userResponse.status})`);
      const user = await userResponse.json();

      let handle = null;
      try {
        const lookup = await this.fetch(`${this.site}/handle?for=${encodeURIComponent(user.sub)}`, {
          headers: { "User-Agent": USER_AGENT, Accept: "application/json" },
        });
        const data = await lookup.json();
        if (data?.handle) handle = String(data.handle).replace(/^@/, "");
      } catch {}

      this.#write({
        access_token: tokens.access_token,
        refresh_token: tokens.refresh_token,
        id_token: tokens.id_token,
        expires_at: this.now() + (tokens.expires_in || 3600) * 1000,
        user: { email: user.email, name: user.name, sub: user.sub, picture: user.picture, handle },
      });
      this.emit("change", this.label());
      return handle;
    } finally {
      this.signingIn = false;
      server.close();
    }
  }

  logout({ browser = false } = {}) {
    let removed = false;
    try {
      unlinkSync(this.file);
      removed = true;
    } catch {}
    if (browser) {
      const url = new URL(`https://${this.authDomain}/v2/logout`);
      url.searchParams.set("client_id", CLIENT_ID);
      url.searchParams.set("returnTo", this.site);
      this.openBrowser(url.toString());
    }
    this.emit("change", this.label());
    return removed;
  }

  #write(record) {
    mkdirSync(dirname(this.file), { recursive: true });
    writeFileSync(this.file, `${JSON.stringify(record, null, 2)}\n`, { mode: 0o600 });
  }
}
