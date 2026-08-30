#!/usr/bin/env node
// sales-tally.mjs — count Menu Band paid copies via the Analytics API.
//
//   node bin/sales-tally.mjs
//
// Sales & Trends (asc.mjs sales) needs ASC_VENDOR_NUMBER, which nobody has
// read off App Store Connect yet, so this walks the ONGOING "App Store
// Purchases Standard" analytics report instead. Instances RESTATE earlier
// dates — for each (granularity, row date) only the instance with the latest
// processingDate counts. The ONGOING request's backfill horizon is
// 2026-07-13, so the 1.5.3 launch window (May 7 – Jul 12) is invisible here;
// days with zero sales produce no instance at all.

import crypto from "node:crypto";
import fs from "node:fs";
import zlib from "node:zlib";

const KEY_ID = "S4TQKG6U99";
const ISSUER = "69a6de78-fa3c-47e3-e053-5b8c7c11a4d1";
const REPORT = "r12-33b8b206-bbd6-4d87-9227-a7ef41fd1a56"; // ONGOING App Store Purchases Standard
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

async function get(path) {
  const res = await fetch(`${API}${path}`, {
    headers: { Authorization: `Bearer ${token()}` },
  });
  const body = await res.json();
  if (!res.ok) throw new Error(`${res.status} ${JSON.stringify(body.errors ?? body)}`);
  return body;
}

async function segmentRows(instanceId) {
  const segs = await get(`/v1/analyticsReportInstances/${instanceId}/segments?limit=200`);
  const rows = [];
  for (const seg of segs.data) {
    const res = await fetch(seg.attributes.url);
    const buf = Buffer.from(await res.arrayBuffer());
    const tsv = zlib.gunzipSync(buf).toString("utf8");
    const lines = tsv.trim().split("\n").map((l) => l.split("\t"));
    const header = lines.shift();
    for (const line of lines) {
      rows.push(Object.fromEntries(header.map((h, i) => [h, line[i]])));
    }
  }
  return rows;
}

const instances = await get(`/v1/analyticsReports/${REPORT}/instances?limit=200`);

for (const granularity of ["DAILY", "WEEKLY"]) {
  const of = instances.data
    .filter((i) => i.attributes.granularity === granularity)
    .sort((a, b) => a.attributes.processingDate.localeCompare(b.attributes.processingDate));

  // Later processingDates restate earlier row dates; last writer wins per date.
  const byDate = new Map();
  for (const inst of of) {
    const grouped = new Map();
    for (const row of await segmentRows(inst.id)) {
      if (!grouped.has(row.Date)) grouped.set(row.Date, []);
      grouped.get(row.Date).push(row);
    }
    for (const [date, rows] of grouped) byDate.set(date, rows);
  }

  let units = 0;
  let proceeds = 0;
  console.log(`\n${granularity}:`);
  for (const date of [...byDate.keys()].sort()) {
    let u = 0, p = 0;
    for (const row of byDate.get(date)) {
      u += Number(row["Purchases"] ?? 0);
      p += Number(row["Proceeds in USD"] ?? 0);
    }
    units += u;
    proceeds += p;
    console.log(`  ${date}  ${u} unit(s)  $${p.toFixed(2)}`);
  }
  console.log(`  total: ${units} unit(s), $${proceeds.toFixed(2)} proceeds`);
}
