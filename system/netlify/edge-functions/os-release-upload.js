// os-release-upload.js — Netlify Edge Function for OS release uploads
// Two-step flow:
//   Step 1 (POST without body): Auth + return presigned S3 URLs for direct upload
//   Step 2 (POST with X-Finalize): Update releases.json after CLI uploads to S3

export default async (request) => {
  if (request.method === "OPTIONS") {
    return new Response(null, {
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "POST",
        "Access-Control-Allow-Headers":
          "Authorization, Content-Type, X-Build-Name, X-Git-Hash, X-Build-Ts, X-Sha256, X-Size, X-Finalize, X-Template-Upload",
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
    return Response.json(
      { error: "Missing Authorization: Bearer <ac_token>" },
      { status: 401 },
    );
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

  // DO Spaces creds (edge functions may see different env var names)
  const accessKey = Deno.env.get("DO_SPACES_KEY") || Deno.env.get("ART_KEY");
  const secretKey = Deno.env.get("DO_SPACES_SECRET") || Deno.env.get("ART_SECRET");
  if (!accessKey || !secretKey) {
    return Response.json({ error: "Spaces creds not configured" }, { status: 503 });
  }

  const bucket = "releases-aesthetic-computer";
  const host = `${bucket}.sfo3.digitaloceanspaces.com`;

  // Metadata from headers
  const buildName = request.headers.get("x-build-name") || `upload-${Date.now()}`;
  const gitHash = request.headers.get("x-git-hash") || "unknown";
  const buildTs =
    request.headers.get("x-build-ts") || new Date().toISOString().slice(0, 16);
  const version = `${buildName} ${gitHash}-${buildTs}`;

  // Helper: generate AWS Sig v2 presigned URL
  function presignUrl(key, contentType, expiresSec = 900) {
    const expires = Math.floor(Date.now() / 1000) + expiresSec;
    // String to sign for presigned URL (AWS Sig v2 query string auth)
    const stringToSign = `PUT\n\n${contentType}\n${expires}\nx-amz-acl:public-read\n/${bucket}/${key}`;

    // HMAC-SHA1 (sync via WebCrypto not possible, use manual approach)
    // For presigned URLs we need to compute sig synchronously-ish
    // Actually we need async, so return a promise
    return (async () => {
      const keyData = new TextEncoder().encode(secretKey);
      const cryptoKey = await crypto.subtle.importKey(
        "raw",
        keyData,
        { name: "HMAC", hash: "SHA-1" },
        false,
        ["sign"],
      );
      const sigBuf = await crypto.subtle.sign(
        "HMAC",
        cryptoKey,
        new TextEncoder().encode(stringToSign),
      );
      const sig = btoa(String.fromCharCode(...new Uint8Array(sigBuf)));

      return (
        `https://${host}/${key}` +
        `?AWSAccessKeyId=${encodeURIComponent(accessKey)}` +
        `&Expires=${expires}` +
        `&Signature=${encodeURIComponent(sig)}` +
        `&x-amz-acl=public-read`
      );
    })();
  }

  // Helper: server-side S3 PUT (for small files only)
  async function s3Put(key, body, contentType) {
    const dateStr = new Date().toUTCString();
    const bodyBytes = typeof body === "string" ? new TextEncoder().encode(body) : body;
    const stringToSign = `PUT\n\n${contentType}\n${dateStr}\nx-amz-acl:public-read\n/${bucket}/${key}`;

    const keyData = new TextEncoder().encode(secretKey);
    const cryptoKey = await crypto.subtle.importKey(
      "raw",
      keyData,
      { name: "HMAC", hash: "SHA-1" },
      false,
      ["sign"],
    );
    const sigBuf = await crypto.subtle.sign(
      "HMAC",
      cryptoKey,
      new TextEncoder().encode(stringToSign),
    );
    const sig = btoa(String.fromCharCode(...new Uint8Array(sigBuf)));

    const res = await fetch(`https://${host}/${key}`, {
      method: "PUT",
      headers: {
        Date: dateStr,
        "Content-Type": contentType,
        "x-amz-acl": "public-read",
        Authorization: `AWS ${accessKey}:${sig}`,
      },
      body: bodyBytes,
    });
    if (!res.ok) {
      const text = await res.text();
      throw new Error(`S3 PUT ${key}: ${res.status} ${text.slice(0, 200)}`);
    }
  }

  const isFinalize = request.headers.get("x-finalize") === "true";

  if (isFinalize) {
    // Step 2: CLI has uploaded vmlinuz directly to S3. Update metadata files.
    const sha256 = request.headers.get("x-sha256") || "unknown";
    const size = parseInt(request.headers.get("x-size") || "0", 10);

    try {
      // Upload version + sha256 text files (tiny, server-side is fine)
      await Promise.all([
        s3Put("os/native-notepat-latest.version", version, "text/plain"),
        s3Put("os/native-notepat-latest.sha256", sha256, "text/plain"),
      ]);

      // Update releases.json
      let releases = { releases: [] };
      try {
        const existing = await fetch(`https://${host}/os/releases.json`);
        if (existing.ok) releases = await existing.json();
      } catch {
        /* first release */
      }

      releases.releases = releases.releases || [];
      releases.releases.unshift({
        version,
        name: buildName,
        sha256,
        size,
        git_hash: gitHash,
        build_ts: buildTs,
        user: userSub,
        url: `https://${host}/os/native-notepat-latest.vmlinuz`,
      });
      releases.releases = releases.releases.slice(0, 50);
      releases.latest = version;
      releases.latest_name = buildName;
      await s3Put(
        "os/releases.json",
        JSON.stringify(releases, null, 2),
        "application/json",
      );

      return Response.json({
        ok: true,
        name: buildName,
        version,
        sha256,
        size,
        url: `https://${host}/os/native-notepat-latest.vmlinuz`,
        user: userSub,
        userName,
      });
    } catch (err) {
      return Response.json(
        { error: `Finalize failed: ${err.message}` },
        { status: 500 },
      );
    }
  }

  // Template .img presigned URL (separate from vmlinuz)
  const isTemplate = request.headers.get("x-template-upload") === "true";
  if (isTemplate) {
    try {
      const imgUrl = await presignUrl(
        "os/native-notepat-latest.img",
        "application/octet-stream",
      );
      return Response.json({
        step: "template-upload",
        img_put_url: imgUrl,
        user: userSub,
      });
    } catch (err) {
      return Response.json(
        { error: `Template presign failed: ${err.message}` },
        { status: 500 },
      );
    }
  }

  // Step 1: Return presigned URL for vmlinuz upload
  try {
    const vmlinuzUrl = await presignUrl(
      "os/native-notepat-latest.vmlinuz",
      "application/octet-stream",
    );

    return Response.json({
      step: "upload",
      vmlinuz_put_url: vmlinuzUrl,
      version,
      user: userSub,
      userName,
    });
  } catch (err) {
    return Response.json(
      { error: `Presign failed: ${err.message}` },
      { status: 500 },
    );
  }
};

export const config = { path: "/api/os-release-upload" };
