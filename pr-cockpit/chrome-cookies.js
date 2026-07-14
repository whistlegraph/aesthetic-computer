// chrome-cookies.js — read the logged-in GitHub session out of the user's real
// Chrome and hand it to Electron as cookie-set params.
//
// Why this exists: the cockpit's <webview> tiles use a `persist:github` session
// partition that starts empty, so private-repo PRs 404 / demand sign-in. Rather
// than sign in again inside the app, we "pass the state in from Chrome" — copy
// Chrome's github.com cookies into our session so tiles load already authed.
//
// How it works (macOS only):
//   - Chrome stores cookies in a SQLite DB per profile; values are encrypted
//     with AES-128-CBC. The key is PBKDF2(HMAC-SHA1) over the "Chrome Safe
//     Storage" password held in the login keychain.
//   - Reading that keychain item triggers the standard macOS keychain consent
//     dialog (Allow / Always Allow) — that prompt IS the authorization. We never
//     store or transmit anything; cookies go straight into the local Electron
//     session and nowhere else.
//
// Read-only: we copy the DB to a temp file and query it; Chrome is untouched.

"use strict";

const { execFileSync } = require("node:child_process");
const crypto = require("node:crypto");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");

const CHROME_DIR = path.join(
  os.homedir(),
  "Library",
  "Application Support",
  "Google",
  "Chrome"
);

// AES params fixed by Chrome on macOS.
const SALT = "saltysalt";
const ITERATIONS = 1003;
const KEYLEN = 16;
const IV = Buffer.alloc(16, 0x20); // 16 spaces

// Pull the "Chrome Safe Storage" password from the login keychain and derive
// the AES key. Throws (with a friendly message) if the user denies the prompt.
function deriveKey() {
  let pw;
  try {
    pw = execFileSync(
      "security",
      ["find-generic-password", "-w", "-s", "Chrome Safe Storage"],
      { encoding: "utf8" }
    ).trim();
  } catch (err) {
    throw new Error(
      "Could not read the Chrome Safe Storage key from the keychain " +
        "(denied or not found). Allow the keychain prompt and try again."
    );
  }
  if (!pw) throw new Error("Empty Chrome Safe Storage key.");
  return crypto.pbkdf2Sync(pw, SALT, ITERATIONS, KEYLEN, "sha1");
}

const isPrintable = (buf) => buf.every((c) => c >= 0x20 && c < 0x7f);

// Decrypt one Chrome-encrypted cookie value (hex of the `encrypted_value` blob).
function decryptValue(hex, key) {
  if (!hex) return "";
  const enc = Buffer.from(hex, "hex");
  if (enc.length === 0) return "";
  // Only the v10 scheme is used on macOS; anything else we can't read.
  if (enc.subarray(0, 3).toString("latin1") !== "v10") return "";
  const decipher = crypto.createDecipheriv("aes-128-cbc", key, IV);
  let out = Buffer.concat([decipher.update(enc.subarray(3)), decipher.final()]);
  // Chrome ~v130+ on macOS prepends a 32-byte SHA-256(domain) to the plaintext.
  // Detect it: real cookie values are printable ASCII; the hash is not.
  if (
    out.length > 32 &&
    !isPrintable(out.subarray(0, 32)) &&
    isPrintable(out.subarray(32, Math.min(out.length, 64)))
  ) {
    out = out.subarray(32);
  }
  return out.toString("utf8");
}

// List candidate profile dirs (Default, Profile 1, Profile 2, …).
function listProfiles() {
  let entries;
  try {
    entries = fs.readdirSync(CHROME_DIR, { withFileTypes: true });
  } catch {
    return [];
  }
  return entries
    .filter(
      (e) =>
        e.isDirectory() &&
        (e.name === "Default" || /^Profile \d+$/.test(e.name))
    )
    .map((e) => e.name);
}

// Copy a profile's cookie DB (+ wal/shm) to a temp file and query github rows.
function readRawCookies(profile) {
  const src = path.join(CHROME_DIR, profile, "Cookies");
  if (!fs.existsSync(src)) return [];
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), "cockpit-cookies-"));
  const tmp = path.join(tmpDir, "Cookies");
  try {
    for (const suffix of ["", "-wal", "-shm"]) {
      const s = src + suffix;
      if (fs.existsSync(s)) fs.copyFileSync(s, tmp + suffix);
    }
    const sql =
      "SELECT host_key, name, path, is_secure, is_httponly, samesite, " +
      "expires_utc, value, hex(encrypted_value) AS enc " +
      "FROM cookies WHERE host_key LIKE '%github.com';";
    const out = execFileSync("sqlite3", ["-json", "-readonly", tmp, sql], {
      encoding: "utf8",
      maxBuffer: 50 * 1024 * 1024,
    });
    return out.trim() ? JSON.parse(out) : [];
  } catch {
    return [];
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
}

// Chrome samesite int → Electron sameSite string.
function sameSiteOf(n) {
  switch (n) {
    case 0:
      return "no_restriction";
    case 1:
      return "lax";
    case 2:
      return "strict";
    default:
      return "unspecified";
  }
}

// Chrome expires_utc (µs since 1601-01-01) → unix seconds, or undefined (session).
function expirationOf(expiresUtc) {
  if (!expiresUtc || expiresUtc === 0) return undefined;
  const unix = Number(expiresUtc) / 1e6 - 11644473600;
  return unix > 0 ? unix : undefined;
}

// Turn a decrypted raw row into Electron cookies.set() params.
function toElectronCookie(row, value) {
  const host = row.host_key; // may start with "."
  const hostNoDot = host.replace(/^\./, "");
  const cpath = row.path || "/";
  const url = `https://${hostNoDot}${cpath}`;
  const isHostPrefixed = row.name.startsWith("__Host-");
  let sameSite = sameSiteOf(row.samesite);
  let secure = row.is_secure === 1;
  // Electron rejects sameSite=no_restriction on a non-secure cookie.
  if (sameSite === "no_restriction") secure = true;

  const cookie = {
    url,
    name: row.name,
    value,
    path: cpath,
    secure,
    httpOnly: row.is_httponly === 1,
    sameSite,
  };
  // __Host- cookies must have no domain; others carry the stored host_key so
  // domain cookies (leading-dot) keep working.
  if (!isHostPrefixed) cookie.domain = host;
  const exp = expirationOf(row.expires_utc);
  if (exp !== undefined) cookie.expirationDate = exp;
  return cookie;
}

// Public: find the logged-in Chrome profile and return its github cookies as
// Electron cookies.set() params. Prefers a profile that actually has a
// `user_session` (i.e. is signed in); falls back to the one with the most
// github cookies.
function getGithubCookies() {
  const key = deriveKey();
  const profiles = listProfiles();
  if (profiles.length === 0) {
    throw new Error(`No Chrome profiles found under ${CHROME_DIR}.`);
  }

  let best = null; // { profile, cookies, loggedIn }
  for (const profile of profiles) {
    const rows = readRawCookies(profile);
    if (rows.length === 0) continue;
    const cookies = [];
    let loggedIn = false;
    for (const row of rows) {
      let value = row.value;
      if (!value && row.enc) {
        try {
          value = decryptValue(row.enc, key);
        } catch {
          value = "";
        }
      }
      if (!value) continue;
      if (row.name === "user_session") loggedIn = true;
      try {
        cookies.push(toElectronCookie(row, value));
      } catch {
        /* skip a cookie we can't shape */
      }
    }
    if (cookies.length === 0) continue;
    const candidate = { profile, cookies, loggedIn };
    if (
      !best ||
      (loggedIn && !best.loggedIn) ||
      (loggedIn === best.loggedIn && cookies.length > best.cookies.length)
    ) {
      best = candidate;
    }
  }

  if (!best) {
    throw new Error(
      "No github.com cookies found in any Chrome profile. Sign in to " +
        "github.com in Chrome first."
    );
  }
  return best;
}

module.exports = { getGithubCookies };
