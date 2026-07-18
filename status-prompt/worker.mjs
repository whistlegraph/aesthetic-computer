const AC = "https://aesthetic.computer";

const escapeHtml = (value) => String(value)
  .replaceAll("&", "&amp;")
  .replaceAll("<", "&lt;")
  .replaceAll(">", "&gt;")
  .replaceAll('"', "&quot;");

async function timedFetch(fetcher, url, options = {}) {
  const started = Date.now();
  try {
    const response = await fetcher(url, {
      redirect: options.redirect || "follow",
      signal: AbortSignal.timeout(options.timeout || 8000),
      headers: options.headers,
    });
    return { response, ms: Date.now() - started };
  } catch (error) {
    return { error: error?.message || String(error), ms: Date.now() - started };
  }
}

export async function collectStatus(fetcher = fetch, handle = null) {
  const checks = [];

  const check = async (id, label, url, validate) => {
    const result = await timedFetch(fetcher, url);
    let detail = result.error || `HTTP ${result.response?.status || 0}`;
    let data;
    if (result.response) {
      try { data = await result.response.clone().json(); } catch {}
    }
    const ok = Boolean(result.response?.ok && (!validate || validate(data, result.response)));
    if (ok) detail = `HTTP ${result.response.status}`;
    checks.push({ id, label, url, ok, ms: result.ms, detail, data });
    return { ...result, data, ok };
  };

  await check("front-door", "Aesthetic Computer", AC);
  await check(
    "api-docs",
    "API abstraction",
    `${AC}/api`,
  );
  await check(
    "database-read",
    "Database read path",
    `${AC}/api/chat-messages?instance=system&limit=1`,
    (data) => Array.isArray(data?.messages),
  );
  await check(
    "asset-delivery",
    "DigitalOcean asset delivery",
    "https://assets.aesthetic.computer/aesthetic-inc/pals.png",
    (_, response) => (response.headers.get("content-type") || "").startsWith("image/"),
  );
  await check(
    "upload-presign",
    "DigitalOcean upload signing",
    `${AC}/presigned-upload-url/png`,
    (data) => Boolean(data?.uploadURL && data?.slug),
  );

  if (handle) {
    const encodedHandle = encodeURIComponent(handle);
    await check(
      "profile",
      `${handle} profile`,
      `${AC}/api/profile/${encodedHandle}`,
      (data) => Boolean(data?.sub),
    );
    const collection = await check(
      "portfolio",
      `${handle} painting index`,
      `${AC}/media-collection?for=${encodedHandle}/painting`,
      (data) => Array.isArray(data?.files),
    );
    const latestUrl = collection.data?.files?.at(-1);
    if (latestUrl) {
      await check(
        "latest-painting",
        `${handle} latest painting`,
        latestUrl,
        (_, response) => (response.headers.get("content-type") || "").startsWith("image/"),
      );
    }
  }

  return {
    ok: checks.every((item) => item.ok),
    checkedAt: new Date().toISOString(),
    ...(handle ? { handle } : {}),
    checks: checks.map(({ data, ...item }) => item),
  };
}

function render(status) {
  const cards = status.checks.map((item) => `
    <article class="check ${item.ok ? "ok" : "bad"}">
      <span class="lamp"></span>
      <div><a href="${escapeHtml(item.url)}">${escapeHtml(item.label)}</a>
      <small>${escapeHtml(item.detail)} · ${item.ms}ms</small></div>
    </article>`).join("");
  return `<!doctype html><html lang="en"><head><meta charset="utf-8">
  <meta name="viewport" content="width=device-width,initial-scale=1">
  <meta http-equiv="refresh" content="60"><title>prompt.ac status</title>
  <style>html{color-scheme:dark}body{margin:0;background:#111;color:#eee;font:16px ui-monospace,monospace}main{max-width:720px;margin:8vh auto;padding:24px}h1{font-size:1.35rem}p,small{color:#999}.summary{color:${status.ok ? "#70e49b" : "#ff7a8a"}}.check{display:flex;gap:14px;align-items:center;border-top:1px solid #292929;padding:16px 4px}.lamp{width:10px;height:10px;border-radius:50%;background:#ff596e;box-shadow:0 0 14px #ff596e}.ok .lamp{background:#55df88;box-shadow:0 0 14px #55df88}a{color:inherit;text-decoration:none}.check div{display:flex;flex:1;justify-content:space-between;gap:12px}footer{margin-top:30px;color:#666;font-size:.8rem}@media(max-width:560px){.check div{display:block}.check small{display:block;margin-top:5px}}</style></head>
  <body><main><h1>status.prompt.ac</h1><p class="summary">${status.ok ? "All watched paths are operational." : "One or more watched paths need attention."}</p>${cards}<footer>Checked ${escapeHtml(status.checkedAt)} from Cloudflare · <a href="/api/status">JSON</a></footer></main></body></html>`;
}

async function saveSnapshot(env, status) {
  if (!env?.STATUS_HISTORY) return;
  const key = `snapshot:${status.checkedAt}`;
  await env.STATUS_HISTORY.put(key, JSON.stringify(status), { expirationTtl: 60 * 60 * 24 * 30 });
  await env.STATUS_HISTORY.put("latest", JSON.stringify(status));
}

export default {
  async fetch(request, env) {
    const url = new URL(request.url);
    const status = await collectStatus(fetch, url.searchParams.get("handle"));
    await saveSnapshot(env, status);
    if (url.pathname === "/api/status") {
      return Response.json(status, {
        status: status.ok ? 200 : 503,
        headers: { "cache-control": "public, max-age=30" },
      });
    }
    return new Response(render(status), {
      status: 200,
      headers: { "content-type": "text/html; charset=utf-8", "cache-control": "public, max-age=30" },
    });
  },
  async scheduled(_controller, env) {
    const status = await collectStatus();
    await saveSnapshot(env, status);
  },
};
