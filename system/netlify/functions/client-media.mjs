// Capability-gated client media landing pages and fresh private downloads.
//
// Add a release to CLIENT_MEDIA with the SHA-256 of its unguessable access
// token. The raw token belongs only in the delivery message; it is never
// committed or logged. Assets remain private in Spaces. A successful download
// click receives a short-lived signed redirect generated at request time.

import { createHash, timingSafeEqual } from "node:crypto";
import { GetObjectCommand, S3Client } from "@aws-sdk/client-s3";
import { getSignedUrl } from "@aws-sdk/s3-request-presigner";

const CLIENT_MEDIA = new Map([
  ["fia/jeannette-montgomery-barron", {
    accessHash: "a3d63ea8a6fe63f3d3b093b46d75adfe451c0de05aa10c5891a1741e965aabb1",
    title: "Jeannette Montgomery Barron Archive",
    eyebrow: "Private archive",
    description: "A private copy of the Instagram archive, including posts, videos, tagged work, captions, and the follower list as it appeared when the archive was made.",
    version: "1.1.0",
    platform: "For Mac",
    size: "151.7 MB",
    filename: "Jeannette-Montgomery-Barron-Archive-1.1.0.dmg",
    bucket: "releases-aesthetic-computer",
    objectKey: "clients/fia/jeannette-montgomery-barron/Jeannette-Montgomery-Barron-Archive-1.1.0.dmg",
    ogImage: "https://releases.aesthetic.computer/clients/fia/jeannette-montgomery-barron/og-v1.png",
  }],
]);

let s3;

function client() {
  if (s3) return s3;
  const accessKeyId = process.env.SPACES_KEY || process.env.DO_SPACES_KEY || process.env.ART_KEY;
  const secretAccessKey = process.env.SPACES_SECRET || process.env.DO_SPACES_SECRET || process.env.ART_SECRET;
  const rawEndpoint = process.env.SPACES_ENDPOINT || process.env.ART_ENDPOINT || "sfo3.digitaloceanspaces.com";
  const endpoint = rawEndpoint.startsWith("http") ? rawEndpoint : `https://${rawEndpoint}`;
  if (!accessKeyId || !secretAccessKey) throw new Error("client-media storage credentials unavailable");
  s3 = new S3Client({
    endpoint,
    region: "us-east-1",
    credentials: { accessKeyId, secretAccessKey },
    requestChecksumCalculation: "WHEN_REQUIRED",
    responseChecksumValidation: "WHEN_REQUIRED",
  });
  return s3;
}

function escape(value) {
  return String(value).replace(/[&<>"']/g, (character) => ({
    "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;",
  })[character]);
}

function pathParts(path = "") {
  const marker = "/client/";
  const suffix = path.includes(marker) ? path.slice(path.indexOf(marker) + marker.length) : "";
  return suffix.split("/").filter(Boolean).map(decodeURIComponent);
}

function authorized(token, expectedHash) {
  if (!token || typeof token !== "string") return false;
  const actual = Buffer.from(createHash("sha256").update(token).digest("hex"));
  const expected = Buffer.from(expectedHash);
  return actual.length === expected.length && timingSafeEqual(actual, expected);
}

function response(statusCode, body, contentType = "text/plain; charset=utf-8", headers = {}) {
  return {
    statusCode,
    headers: {
      "Content-Type": contentType,
      "Cache-Control": "private, no-store",
      "X-Robots-Tag": "noindex, nofollow, noarchive",
      "Referrer-Policy": "no-referrer",
      ...headers,
    },
    body,
  };
}

function landing(entry, token, event, key) {
  const origin = event.headers?.["x-forwarded-proto"] && event.headers?.host
    ? `${event.headers["x-forwarded-proto"]}://${event.headers.host}`
    : "https://aesthetic.computer";
  const pagePath = `/client/${key}`;
  const pageURL = `${origin}${pagePath}?access=${encodeURIComponent(token)}`;
  const downloadURL = `${pagePath}/download?access=${encodeURIComponent(token)}`;
  const title = escape(entry.title);
  const description = escape(entry.description);
  return response(200, `<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>${title} — private download</title>
<meta name="description" content="${description}"><meta name="robots" content="noindex,nofollow,noarchive">
<meta property="og:type" content="website"><meta property="og:site_name" content="Aesthetic Computer Client Media">
<meta property="og:title" content="${title}"><meta property="og:description" content="${description}">
<meta property="og:url" content="${escape(pageURL)}"><meta property="og:image" content="${escape(entry.ogImage)}">
<meta property="og:image:width" content="1200"><meta property="og:image:height" content="630">
<meta name="twitter:card" content="summary_large_image"><meta name="twitter:title" content="${title}">
<meta name="twitter:description" content="${description}"><meta name="twitter:image" content="${escape(entry.ogImage)}">
<style>:root{color-scheme:light;--paper:#f1eadb;--ink:#1c1b18;--red:#a92420}*{box-sizing:border-box}body{margin:0;min-height:100vh;display:grid;place-items:center;background:var(--paper);color:var(--ink);font:16px/1.5 ui-sans-serif,system-ui,sans-serif}main{width:min(760px,calc(100% - 32px));margin:32px;background:#f8f3e9;border:1px solid #d8cebd;box-shadow:0 22px 70px #382f231f}.hero{display:block;width:100%;height:auto;border-bottom:1px solid #d8cebd}.copy{padding:clamp(24px,5vw,48px)}.eyebrow{margin:0 0 12px;color:var(--red);font-size:12px;font-weight:750;letter-spacing:.12em;text-transform:uppercase}h1{font:700 clamp(32px,6vw,58px)/.98 ui-serif,Georgia,serif;letter-spacing:-.035em;margin:0 0 18px;max-width:650px}p{margin:0 0 24px;color:#5e584f}.facts{display:flex;gap:8px;flex-wrap:wrap;margin:0 0 28px}.facts span{border:1px solid #cfc4b2;border-radius:999px;padding:6px 10px;font-size:12px}.download{display:inline-block;background:var(--ink);color:white;text-decoration:none;border-radius:999px;padding:13px 19px;font-weight:700}.download:hover{background:var(--red)}footer{padding:0 48px 30px;color:#81796e;font-size:11px}@media(max-width:520px){footer{padding:0 24px 24px}}</style></head>
<body><main><img class="hero" src="${escape(entry.ogImage)}" width="1200" height="630" alt="JMB archive mark"><div class="copy"><p class="eyebrow">${escape(entry.eyebrow)}</p><h1>${title}</h1><p>${description}</p><div class="facts"><span>${escape(entry.platform)}</span><span>${escape(entry.size)}</span></div><a class="download" href="${escape(downloadURL)}">Download for Mac</a></div><footer>Prepared for Jeannette Montgomery Barron.</footer></main></body></html>`, "text/html; charset=utf-8");
}

export async function handler(event) {
  if (event.httpMethod !== "GET") return response(405, "Method not allowed");
  const parts = pathParts(event.path);
  const key = parts.slice(0, 2).join("/");
  const entry = CLIENT_MEDIA.get(key);
  if (!entry) return response(404, "Client media not found");
  const token = event.queryStringParameters?.access || "";
  if (!authorized(token, entry.accessHash)) return response(404, "Client media not found");
  if (parts[2] !== "download") return landing(entry, token, event, key);
  try {
    const url = await getSignedUrl(client(), new GetObjectCommand({
      Bucket: entry.bucket,
      Key: entry.objectKey,
      ResponseContentType: "application/x-apple-diskimage",
      ResponseContentDisposition: `attachment; filename="${entry.filename}"`,
    }), { expiresIn: 15 * 60 });
    return response(302, "", "text/plain; charset=utf-8", { Location: url });
  } catch (error) {
    console.error("client-media download signing failed", error?.message || error);
    return response(503, "Download temporarily unavailable");
  }
}
