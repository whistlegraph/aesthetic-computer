// Thin HTTP client for the kidlisp Datomic sidecar.
// Used by system/netlify/functions/store-kidlisp.mjs behind the
// KIDLISP_DATOMIC feature flag (default off — see that file).
//
// The sidecar exposes a clean Datomic-native API; this module does not
// reshape responses. The caller is responsible for mapping sidecar
// shapes to the external Mongo-era response shape that existing clients
// depend on.
//
// Env:
//   DATOMIC_SIDECAR_URL            default http://127.0.0.1:8891
//   DATOMIC_SIDECAR_CLIENT_SECRET  required when KIDLISP_DATOMIC=on

const URL_BASE = () =>
  process.env.DATOMIC_SIDECAR_URL || "http://127.0.0.1:8891";

function headers() {
  const s = process.env.DATOMIC_SIDECAR_CLIENT_SECRET;
  if (!s) throw new Error("DATOMIC_SIDECAR_CLIENT_SECRET not set");
  return { "content-type": "application/json", "x-sidecar-secret": s };
}

async function req(method, path, body) {
  const res = await fetch(`${URL_BASE()}${path}`, {
    method,
    headers: headers(),
    body: body != null ? JSON.stringify(body) : undefined,
  });
  const text = await res.text();
  let json = null;
  try { json = text ? JSON.parse(text) : null; } catch { /* not JSON */ }
  return { ok: res.ok, status: res.status, body: json ?? text };
}

export const sidecar = {
  // ── public API surface used by store-kidlisp.mjs ──

  async lookupByHash(hash) {
    const r = await req("GET", `/kidlisp/hash/${encodeURIComponent(hash)}`);
    if (r.status === 404) return null;
    if (!r.ok) throw new Error(`sidecar lookupByHash failed: ${r.status}`);
    return r.body;
  },

  async lookupByCode(code) {
    const r = await req("GET", `/kidlisp/${encodeURIComponent(code)}`);
    if (r.status === 404) return null;
    if (!r.ok) throw new Error(`sidecar lookupByCode failed: ${r.status}`);
    return r.body;
  },

  async batchLookup(codes) {
    const r = await req("POST", `/kidlisp/lookup`, { codes });
    if (!r.ok) throw new Error(`sidecar batchLookup failed: ${r.status}`);
    return r.body;
  },

  async listCodes({ limit, sort, since } = {}) {
    const qs = new URLSearchParams();
    if (limit) qs.set("limit", String(limit));
    if (sort)  qs.set("sort", sort);
    if (since) qs.set("since", since);
    const r = await req("GET", `/kidlisp?${qs.toString()}`);
    if (!r.ok) throw new Error(`sidecar listCodes failed: ${r.status}`);
    return r.body;
  },

  async statsFunctions({ limit } = {}) {
    const qs = new URLSearchParams();
    if (limit) qs.set("limit", String(limit));
    const r = await req("GET", `/kidlisp/stats/functions?${qs.toString()}`);
    if (!r.ok) throw new Error(`sidecar statsFunctions failed: ${r.status}`);
    return r.body;
  },

  // Create-or-find. Body fields:
  //   source, hash, code, user_sub?, forked_from?, when?, hits?
  // Returns { code, cached }.
  async create(piece) {
    const r = await req("POST", "/kidlisp", piece);
    if (!r.ok) throw new Error(`sidecar create failed: ${r.status} ${r.body}`);
    return r.body;
  },

  async recordMint(code, keep) {
    const r = await req("POST", `/kidlisp/${encodeURIComponent(code)}/mint`, keep);
    if (!r.ok) throw new Error(`sidecar recordMint failed: ${r.status}`);
    return r.body;
  },

  async setTezosState(code, state) {
    const r = await req("POST", `/kidlisp/${encodeURIComponent(code)}/tezos-state`, state);
    if (!r.ok) throw new Error(`sidecar setTezosState failed: ${r.status}`);
    return r.body;
  },

  async setPendingRebake(code, rebake) {
    const r = await req("POST", `/kidlisp/${encodeURIComponent(code)}/pending-rebake`, rebake);
    if (!r.ok) throw new Error(`sidecar setPendingRebake failed: ${r.status}`);
    return r.body;
  },

  async setIpfsMedia(code, media) {
    const r = await req("POST", `/kidlisp/${encodeURIComponent(code)}/ipfs-media`, media);
    if (!r.ok) throw new Error(`sidecar setIpfsMedia failed: ${r.status}`);
    return r.body;
  },

  async setAtprotoRkey(code, rkey) {
    const r = await req("POST", `/kidlisp/${encodeURIComponent(code)}/atproto-rkey`, { rkey });
    if (!r.ok) throw new Error(`sidecar setAtprotoRkey failed: ${r.status}`);
    return r.body;
  },

  async lineage(code) {
    const r = await req("GET", `/kidlisp/${encodeURIComponent(code)}/lineage`);
    if (r.status === 404) return null;
    if (!r.ok) throw new Error(`sidecar lineage failed: ${r.status}`);
    return r.body;
  },
};

// Feature flag helper — read once per request since Netlify functions
// reuse the module across invocations.
export function kidlispDatomicEnabled() {
  return process.env.KIDLISP_DATOMIC === "on";
}
