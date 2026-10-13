import test from "node:test";
import assert from "node:assert/strict";
import { audience } from "./choir-push.mjs";

const db = (devices, prefs) => ({ collection: (name) => ({ find: () => ({ toArray: async () => (name === "app-devices" ? devices : prefs) }) }) });

test("a post reaches everyone but the poster (on any of their accounts) and anyone who opted out", async () => {
  const devices = [
    { user: "email|fia-phone" },   // Fía signed in by email code…
    { user: "auth0|jeffrey" },
    { user: "auth0|harry" },
    { user: "auth0|quiet" },
    { user: "auth0|stranger" },    // a device whose member row we have not seen yet
  ];
  const prefs = [
    { _id: "auth0|fifi", subs: ["auth0|fifi", "email|fia-phone"], handle: "@fifi" }, // …but posts as @fifi
    { _id: "auth0|jeffrey", subs: ["auth0|jeffrey"] },
    { _id: "auth0|harry", subs: ["auth0|harry"], notifications: true },
    { _id: "auth0|quiet", subs: ["auth0|quiet"], notifications: false },
  ];
  assert.deepEqual((await audience(db(devices, prefs), { except: "auth0|fifi" })).sort(), ["auth0|harry", "auth0|jeffrey", "auth0|stranger"]);
  assert.deepEqual((await audience(db(devices, prefs), { except: "auth0|jeffrey" })).sort(), ["auth0|harry", "auth0|stranger", "email|fia-phone"]);
  assert.equal((await audience(db(devices, prefs))).length, 4, "Discover news reaches everyone who has not opted out");
});
