#!/usr/bin/env node
// asc.mjs — App Store Connect API helper for Menu Band.
//
//   node bin/asc.mjs status                     version states + latest builds
//   node bin/asc.mjs get <path>                 raw GET, prints JSON
//   node bin/asc.mjs sales [YYYY-MM-DD]         Sales & Trends daily units (gzip TSV)
//   node bin/asc.mjs analytics                  ensure report requests; list reports
//   node bin/asc.mjs analytics <reportId>       list a report's instances/segments
//
// Auth: the Admin key (App Manager keys can't cloud-sign but CAN read reports).
// Signature must be ES256 in ieee-p1363 form — DER signatures are rejected.

import crypto from "node:crypto";
import fs from "node:fs";
import zlib from "node:zlib";

const KEY_ID = "S4TQKG6U99";
const ISSUER = "69a6de78-fa3c-47e3-e053-5b8c7c11a4d1";
const APP_ID = "6767311903"; // Menu Band
const VENDOR = process.env.ASC_VENDOR_NUMBER; // needed for sales reports
const API = "https://api.appstoreconnect.apple.com";

const key = fs.readFileSync(
  `${process.env.HOME}/.appstoreconnect/private_keys/AuthKey_${KEY_ID}.p8`,
  "utf8",
);

function token() {
  const b64 = (o) => Buffer.from(JSON.stringify(o)).toString("base64url");
  const now = Math.floor(Date.now() / 1000);
  const input = `${b64({ alg: "ES256", kid: KEY_ID, typ: "JWT" })}.${b64({
    iss: ISSUER,
    iat: now,
    exp: now + 1200,
    aud: "appstoreconnect-v1",
  })}`;
  const sig = crypto.sign("sha256", Buffer.from(input), {
    key,
    dsaEncoding: "ieee-p1363",
  });
  return `${input}.${sig.toString("base64url")}`;
}

async function get(path, raw = false) {
  const res = await fetch(`${API}${path}`, {
    headers: { Authorization: `Bearer ${token()}` },
  });
  if (raw) return res;
  const body = await res.json();
  if (!res.ok) throw new Error(`${res.status} ${JSON.stringify(body.errors ?? body)}`);
  return body;
}

const [cmd, arg] = process.argv.slice(2);

if (cmd === "get") {
  console.log(JSON.stringify(await get(arg), null, 2));
} else if (cmd === "status") {
  const versions = await get(
    `/v1/apps/${APP_ID}/appStoreVersions?limit=8&fields[appStoreVersions]=versionString,appStoreState,createdDate`,
  );
  console.log("— appStoreVersions —");
  for (const v of versions.data)
    console.log(
      `  ${v.attributes.versionString}  ${v.attributes.appStoreState}  (${v.attributes.createdDate})  id=${v.id}`,
    );
  const builds = await get(
    `/v1/builds?filter[app]=${APP_ID}&limit=8&sort=-version&fields[builds]=version,processingState,uploadedDate`,
  );
  console.log("— builds —");
  for (const b of builds.data)
    console.log(
      `  build ${b.attributes.version}  ${b.attributes.processingState}  (${b.attributes.uploadedDate})  id=${b.id}`,
    );
  const subs = await get(
    `/v1/reviewSubmissions?filter[app]=${APP_ID}&limit=5&fields[reviewSubmissions]=state,platform,submittedDate`,
  );
  console.log("— reviewSubmissions —");
  for (const s of subs.data)
    console.log(
      `  ${s.attributes.platform}  ${s.attributes.state}  (${s.attributes.submittedDate ?? "unsubmitted"})  id=${s.id}`,
    );
} else if (cmd === "sales") {
  if (!VENDOR) throw new Error("set ASC_VENDOR_NUMBER (Payments & Financial Reports → vendor #)");
  const date = arg ?? new Date(Date.now() - 2 * 864e5).toISOString().slice(0, 10);
  const q = new URLSearchParams({
    "filter[frequency]": "DAILY",
    "filter[reportDate]": date,
    "filter[reportSubType]": "SUMMARY",
    "filter[reportType]": "SALES",
    "filter[vendorNumber]": VENDOR,
  });
  const res = await get(`/v1/salesReports?${q}`, true);
  if (!res.ok) throw new Error(`${res.status} ${await res.text()}`);
  const tsv = zlib.gunzipSync(Buffer.from(await res.arrayBuffer())).toString("utf8");
  const rows = tsv.trim().split("\n").map((l) => l.split("\t"));
  const head = rows[0];
  const idx = (n) => head.indexOf(n);
  const mine = rows.slice(1).filter((r) => r[idx("Apple Identifier")] === APP_ID);
  console.log(`sales ${date}:`);
  if (!mine.length) console.log("  no rows for Menu Band (zero units that day)");
  for (const r of mine)
    console.log(
      `  ${r[idx("Product Type Identifier")]}  units=${r[idx("Units")]}  country=${r[idx("Country Code")]}  device=${r[idx("Device")] ?? ""}`,
    );
} else if (cmd === "analytics") {
  if (arg) {
    // List a specific report's instances, then each instance's segment URLs.
    const instances = await get(
      `/v1/analyticsReports/${arg}/instances?limit=10`,
    );
    for (const i of instances.data) {
      console.log(
        `instance ${i.attributes.granularity} ${i.attributes.processingDate} id=${i.id}`,
      );
      const segs = await get(
        `/v1/analyticsReportInstances/${i.id}/segments`,
      );
      for (const s of segs.data)
        console.log(`  segment: ${s.attributes.url}`);
    }
  } else {
    // Ensure both an ONGOING and a ONE_TIME_SNAPSHOT request exist, then
    // list every report they expose. Reports generate asynchronously —
    // a fresh request has none until Apple processes it (~1-2 days).
    const existing = await get(
      `/v1/apps/${APP_ID}/analyticsReportRequests?fields[analyticsReportRequests]=accessType,stoppedDueToInactivity`,
    );
    const have = new Set(existing.data.map((r) => r.attributes.accessType));
    for (const accessType of ["ONGOING", "ONE_TIME_SNAPSHOT"]) {
      if (have.has(accessType)) continue;
      const res = await fetch(`${API}/v1/analyticsReportRequests`, {
        method: "POST",
        headers: {
          Authorization: `Bearer ${token()}`,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          data: {
            type: "analyticsReportRequests",
            attributes: { accessType },
            relationships: {
              app: { data: { type: "apps", id: APP_ID } },
            },
          },
        }),
      });
      const body = await res.json();
      if (!res.ok) throw new Error(`${res.status} ${JSON.stringify(body.errors)}`);
      console.log(`created ${accessType} request ${body.data.id}`);
      existing.data.push(body.data);
    }
    for (const r of existing.data) {
      console.log(
        `request ${r.attributes.accessType} id=${r.id}${r.attributes.stoppedDueToInactivity ? " (STOPPED — recreate)" : ""}`,
      );
      const reports = await get(
        `/v1/analyticsReportRequests/${r.id}/reports?limit=50&fields[analyticsReports]=name,category`,
      );
      for (const rep of reports.data)
        console.log(`  ${rep.attributes.category}  ${rep.attributes.name}  id=${rep.id}`);
    }
  }
} else {
  console.log("usage: asc.mjs status | get <path> | sales [date] | analytics [reportId]");
}
