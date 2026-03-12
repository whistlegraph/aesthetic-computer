// os-release-upload.js — Netlify Edge Function for OS release uploads
// 1. Verifies AC token via Auth0
// 2. Receives vmlinuz binary
// 3. Uploads to DO Spaces using raw S3 API
// 4. Updates version/sha256/releases.json

export default async (request) => {
  if (request.method === "OPTIONS") {
    return new Response(null, {
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "POST",
        "Access-Control-Allow-Headers": "Authorization, Content-Type, X-Build-Name, X-Git-Hash, X-Build-Ts",
      },
    });
  }

  if (request.method !== "POST") {
    return Response.json({ error: "POST only" }, { status: 405 });
  }

  // Auth: verify AC token
  const authHeader = request.headers.get("authorization") || "";
  const token = authHeader.startsWith("Bearer ") ? authHeader.slice(7).trim() : "";
  if (!token) {
    return Response.json({ error: "Missing Authorization: Bearer <ac_token>" }, { status: 401 });
  }

  let user;
  try {
    const uiRes = await fetch("https://hi.aesthetic.computer/userinfo", {
      headers: { Authorization: `Bearer ${token}` },
    });
    if (!uiRes.ok) throw new Error(`Auth0 ${uiRes.status}`);
    user = await uiRes.json();
  } catch (err) {
    return Response.json({ error: `Auth failed: ${err.message}` }, { status: 401 });
  }

  const userSub = user.sub || "unknown";
  const userName = user.name || user.nickname || userSub;

  // Read binary body
  const vmlinuzBuf = await request.arrayBuffer();
  const vmlinuz = new Uint8Array(vmlinuzBuf);
  if (vmlinuz.byteLength < 1_000_000) {
    return Response.json({ error: `Too small: ${vmlinuz.byteLength} bytes` }, { status: 400 });
  }

  // Metadata
  const buildName = request.headers.get("x-build-name") || `upload-${Date.now()}`;
  const gitHash = request.headers.get("x-git-hash") || "unknown";
  const buildTs = request.headers.get("x-build-ts") || new Date().toISOString().slice(0, 16);
  const version = `${buildName} ${gitHash}-${buildTs}`;

  // SHA256
  const hashBuf = await crypto.subtle.digest("SHA-256", vmlinuz);
  const sha256 = [...new Uint8Array(hashBuf)].map(b => b.toString(16).padStart(2, "0")).join("");

  // DO Spaces creds
  const accessKey = Deno.env.get("DO_SPACES_KEY");
  const secretKey = Deno.env.get("DO_SPACES_SECRET");
  if (!accessKey || !secretKey) {
    return Response.json({ error: "Spaces creds not configured" }, { status: 503 });
  }

  const bucket = "releases-aesthetic-computer";
  const host = `${bucket}.sfo3.digitaloceanspaces.com`;

  // AWS Sig v2 upload helper (same as upload-release.sh)
  async function s3Put(key, body, contentType) {
    const dateStr = new Date().toUTCString();
    const bodyBytes = typeof body === "string" ? new TextEncoder().encode(body) : body;

    // MD5
    const md5Buf = await crypto.subtle.digest("MD5", bodyBytes).catch(() => null);
    // MD5 not available in all Deno versions, fall back to empty
    const md5B64 = md5Buf
      ? btoa(String.fromCharCode(...new Uint8Array(md5Buf)))
      : "";

    // String to sign (AWS Sig v2)
    const stringToSign = `PUT\n${md5B64}\n${contentType}\n${dateStr}\nx-amz-acl:public-read\n/${bucket}/${key}`;

    // HMAC-SHA1
    const keyData = new TextEncoder().encode(secretKey);
    const cryptoKey = await crypto.subtle.importKey(
      "raw", keyData, { name: "HMAC", hash: "SHA-1" }, false, ["sign"],
    );
    const sigBuf = await crypto.subtle.sign("HMAC", cryptoKey, new TextEncoder().encode(stringToSign));
    const sig = btoa(String.fromCharCode(...new Uint8Array(sigBuf)));

    const headers = {
      Date: dateStr,
      "Content-Type": contentType,
      "x-amz-acl": "public-read",
      Authorization: `AWS ${accessKey}:${sig}`,
    };
    if (md5B64) headers["Content-MD5"] = md5B64;

    const res = await fetch(`https://${host}/${key}`, {
      method: "PUT",
      headers,
      body: bodyBytes,
    });
    if (!res.ok) {
      const text = await res.text();
      throw new Error(`S3 PUT ${key}: ${res.status} ${text.slice(0, 200)}`);
    }
  }

  try {
    // Upload canary files + vmlinuz in parallel for speed
    await Promise.all([
      s3Put("os/native-notepat-latest.version", version, "text/plain"),
      s3Put("os/native-notepat-latest.sha256", sha256, "text/plain"),
      s3Put("os/native-notepat-latest.vmlinuz", vmlinuz, "application/octet-stream"),
    ]);

    // Update releases.json
    let releases = { releases: [] };
    try {
      const existing = await fetch(`https://${host}/os/releases.json`);
      if (existing.ok) releases = await existing.json();
    } catch { /* first release */ }

    releases.releases = releases.releases || [];
    releases.releases.unshift({
      version, name: buildName, sha256, size: vmlinuz.byteLength,
      git_hash: gitHash, build_ts: buildTs, user: userSub,
      url: `https://${host}/os/native-notepat-latest.vmlinuz`,
    });
    releases.releases = releases.releases.slice(0, 50);
    releases.latest = version;
    releases.latest_name = buildName;
    await s3Put("os/releases.json", JSON.stringify(releases, null, 2), "application/json");

    return Response.json({
      ok: true, name: buildName, version, sha256,
      size: vmlinuz.byteLength,
      url: `https://${host}/os/native-notepat-latest.vmlinuz`,
      user: userSub, userName,
    });
  } catch (err) {
    return Response.json({ error: `Upload failed: ${err.message}` }, { status: 500 });
  }
};

export const config = { path: "/api/os-release-upload" };
