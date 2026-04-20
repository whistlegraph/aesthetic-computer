// Datomic-backed implementation of store-kidlisp.
// Invoked by store-kidlisp.mjs when KIDLISP_DATOMIC=on.
//
// Responsibilities:
// - Speak the SAME external request/response shape as store-kidlisp.mjs
//   (callers cannot tell which backend is live).
// - Translate Datomic-native sidecar responses to the Mongo-era shape.
// - Join @handles from Mongo for display names (handles stay in Mongo).
//
// Writes NEVER touch the Mongo `kidlisp` collection.

import { authorize, getHandleOrEmail } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { respond } from "../../backend/http.mjs";
import { generateUniqueCode } from "../../backend/generate-short-code.mjs";
import { createMediaRecord, MediaTypes } from "../../backend/media-atproto.mjs";
import { publishProfileEvent } from "../../backend/profile-stream.mjs";
import { sidecar } from "../../backend/kidlisp-sidecar.mjs";
import { extractAst } from "../../backend/kidlisp-ast.mjs";
import crypto from "crypto";

const TEZOS_ENABLED = process.env.TEZOS_ENABLED === "true";
const NO_CACHE_HEADERS = {
  "Cache-Control": "no-store, no-cache, must-revalidate, max-age=0",
  Pragma: "no-cache",
  Expires: "0",
};

// ── handle join (the one thing we still need Mongo for) ──

async function handlesBySub(database, subs) {
  if (!subs?.length) return new Map();
  const rows = await database.db
    .collection("@handles")
    .find({ _id: { $in: [...new Set(subs.filter(Boolean))] } })
    .project({ _id: 1, handle: 1 })
    .toArray();
  return new Map(rows.map((r) => [r._id, `@${r.handle}`]));
}

// ── shape translators (sidecar entity → mongo-era response shape) ──

function selectPrimaryKeep(keeps, preferredContract) {
  if (!Array.isArray(keeps) || keeps.length === 0) return null;
  const byRecency = [...keeps].sort((a, b) => {
    const at = a?.keptAt ? new Date(a.keptAt).getTime() : 0;
    const bt = b?.keptAt ? new Date(b.keptAt).getTime() : 0;
    return bt - at;
  });
  if (!preferredContract) return byRecency[0];
  const want = preferredContract.toLowerCase();
  const pref = byRecency.find(
    (k) => (k.contractAddress || "").toLowerCase() === want
  );
  return pref || byRecency[0];
}

function filterKeeps(keeps, { contract, contractProfile, contractVersion }) {
  if (!Array.isArray(keeps)) return [];
  const c = contract?.toLowerCase() || null;
  const p = contractProfile?.toLowerCase() || null;
  const v = contractVersion || null;
  return keeps.filter((k) => {
    if (c && (k.contractAddress || "").toLowerCase() !== c) return false;
    if (p && k.contractProfile && k.contractProfile !== p) return false;
    if (v && k.contractVersion && k.contractVersion !== v) return false;
    return true;
  });
}

function toMongoShape(entity, opts = {}) {
  if (!entity) return null;
  const out = {
    source: entity.source,
    when: entity.when,
    hits: entity.hits ?? 0,
    user: entity.user || null,
    handle: opts.handle || null,
  };
  if (entity.ipfsMedia) out.ipfsMedia = entity.ipfsMedia;
  const keeps = filterKeeps(entity.keeps, {
    contract: opts.contract,
    contractProfile: opts.contractProfile,
    contractVersion: opts.contractVersion,
  });
  if (keeps.length > 0) {
    out.kept = selectPrimaryKeep(keeps, opts.contract);
    if (keeps.length > 1) out.keptRecords = keeps;
  }
  if (entity.pendingRebake) out.pendingRebake = entity.pendingRebake;
  return out;
}

// ── main handler ──

export async function handler(event, context) {
  if (event.httpMethod === "OPTIONS") return respond(204, "");

  let database;
  try {
    database = await connect();
  } catch (err) {
    console.error("❌ Mongo connect failed (needed for @handles join):", err.message);
    return respond(503, { error: "Database temporarily unavailable" });
  }

  try {
    if (event.httpMethod === "POST") {
      const { source } = JSON.parse(event.body || "{}");
      if (!source || typeof source !== "string" || source.length > 50000) {
        await database.disconnect();
        return respond(400, { error: "Invalid source" });
      }

      // Authorize (best-effort, anonymous allowed)
      let user;
      try {
        const authPromise = authorize(event.headers);
        const timeout = new Promise((_, rej) =>
          setTimeout(() => rej(new Error("Auth timeout")), 3000)
        );
        user = await Promise.race([authPromise, timeout]);
      } catch {
        /* anonymous */
      }

      let profileHandle = null;
      if (user?.sub) {
        try {
          const h = await getHandleOrEmail(user.sub);
          if (typeof h === "string" && h.startsWith("@")) profileHandle = h;
        } catch {}
      }

      const hash = crypto.createHash("sha256").update(source.trim()).digest("hex");

      // Dedup: fast path via sidecar hash lookup
      const existing = await sidecar.lookupByHash(hash);
      if (existing) {
        await database.disconnect();
        return respond(200, { code: existing.code, cached: true });
      }

      // Generate a code. The Mongo `collection` previously passed to
      // generateUniqueCode is used for collision checks, but with Datomic
      // authoritative we check via sidecar instead. Keep the inference
      // logic by passing a minimal shim.
      const codeCheckShim = {
        findOne: async ({ code }) => {
          const hit = await sidecar.lookupByCode(code);
          return hit ? { code } : null;
        },
      };
      const code = await generateUniqueCode(codeCheckShim, {
        mode: "inferred",
        sourceText: source,
        type: "kidlisp",
      });

      // Insert via sidecar
      const created = await sidecar.create({
        source: source.trim(),
        hash,
        code,
        user_sub: user?.sub || null,
      });
      // sidecar returned {code, cached} — if cached (raced with another write),
      // the returned code may differ from our proposed `code`. Honor it.
      const finalCode = created.code;

      // AST index (fire-and-forget): parse source structure and POST to
      // sidecar so corpus-wide datalog queries can find it. If the piece
      // was cached (hash dedup), its AST is already stored — skip.
      if (!created.cached) {
        try {
          const astNodes = extractAst(source.trim());
          sidecar.setAst(finalCode, astNodes).catch((err) => {
            console.warn("⚠️  AST index failed:", err?.message || err);
          });
        } catch (err) {
          console.warn("⚠️  AST extract failed:", err?.message || err);
        }
      }

      // Profile stream event (fire-and-forget)
      if (profileHandle) {
        publishProfileEvent({
          handle: profileHandle,
          event: {
            type: "kidlisp",
            when: Date.now(),
            label: `KidLisp $${finalCode}`,
            ref: finalCode,
          },
          countsDelta: { kidlisp: 1 },
        }).catch(() => {});
      }

      // ATProto sync (fire-and-forget). We synthesize a savedRecord shape
      // matching what createMediaRecord expects.
      const savedRecord = {
        code: finalCode,
        source: source.trim(),
        hash,
        when: new Date(),
        user: user?.sub || null,
      };
      createMediaRecord(database, MediaTypes.KIDLISP, savedRecord, { userSub: user?.sub })
        .then((result) => {
          if (result?.rkey) {
            sidecar.setAtprotoRkey(finalCode, result.rkey).catch(() => {});
          }
        })
        .catch(() => {});

      // Tezos integration (optional, fire-and-forget)
      let tezosResult = null;
      if (TEZOS_ENABLED && user?.sub) {
        try {
          const { integrateWithKidLispCache } = await import(
            "../../../tezos/src/integration.js"
          );
          tezosResult = await integrateWithKidLispCache(
            source.trim(),
            user,
            finalCode
          );

          if (tezosResult?.minted) {
            await sidecar.setTezosState(finalCode, {
              minted: true,
              tokenId: tezosResult.tokenId,
              txHash: tezosResult.txHash,
              creatorAddress: tezosResult.creatorAddress,
              codeHash: tezosResult.codeHash,
              network: tezosResult.network,
            });
          } else if (tezosResult?.exists) {
            await sidecar.setTezosState(finalCode, {
              minted: false,
              exists: true,
              tokenId: tezosResult.tokenId,
              codeHash: tezosResult.codeHash,
              network: tezosResult.network,
              reason: tezosResult.reason,
            });
          } else if (tezosResult) {
            await sidecar.setTezosState(finalCode, {
              minted: false,
              exists: false,
              reason: tezosResult.reason,
              error: tezosResult.error,
            });
          }
        } catch (err) {
          await sidecar.setTezosState(finalCode, {
            minted: false,
            error: err.message,
          });
        }
      }

      await database.disconnect();
      return respond(201, {
        code: finalCode,
        cached: false,
        ...(tezosResult && { tezos: tezosResult }),
      });
    }

    if (event.httpMethod === "GET") {
      const q = event.queryStringParameters || {};
      const { code, codes, recent, stats } = q;
      const requestedContract = q.contract || null;
      const requestedContractVersion = q.contractVersion || null;
      const requestedContractProfile = q.contractProfile || null;

      // ── stats=functions (aggregation delegated to Node via sidecar raw docs) ──
      if (stats === "functions") {
        const limit = parseInt(q.limit) || 5000;
        const { docs } = await sidecar.statsFunctions({ limit });

        const rawCounts = {};
        const weightedCounts = {};
        let totalHits = 0;

        const funcPattern = /\(\s*([a-zA-Z_+\-*/%?][a-zA-Z0-9_]*)/g;
        const bareCommands = new Set([
          "wipe","ink","line","box","circle","plot","point","flood",
          "scroll","spin","zoom","blur","contrast","suck","sort",
          "bake","fill","outline","stroke","nofill","nostroke",
          "resolution","mask","unmask","steal","putback",
          "rainbow","zebra","noise","unpan","resetSpin",
        ]);
        const bareColors = new Set([
          "red","green","blue","yellow","orange","purple","pink",
          "cyan","magenta","black","white","gray","grey","brown",
          "lime","navy","teal","olive","maroon","aqua","fuchsia",
          "silver","gold","coral","salmon","khaki","indigo","violet",
          "turquoise","tomato","crimson","lavender","beige","plum",
          "orchid","tan","chocolate","sienna","peru","wheat",
          "deepskyblue","hotpink","springgreen","darkslategray",
        ]);

        for (const d of docs || []) {
          const src = d.source || "";
          const hits = d.hits || 1;
          totalHits += hits;
          const seen = new Set();
          let m;
          funcPattern.lastIndex = 0;
          while ((m = funcPattern.exec(src)) !== null) seen.add(m[1]);
          const tokens = src.split(/[,\n]/).map((t) => t.trim().split(/\s+/)[0]);
          for (const t of tokens) {
            if (bareCommands.has(t)) seen.add(t);
            if (bareColors.has(t)) seen.add("wipe");
          }
          if (/\$[a-zA-Z0-9]+/.test(src)) seen.add("embed");
          if (/\d+\.?\d*s[.!]?/.test(src)) seen.add("timing");
          if (/fade:/.test(src)) seen.add("fade");

          for (const fn of seen) {
            rawCounts[fn] = (rawCounts[fn] || 0) + 1;
            weightedCounts[fn] = (weightedCounts[fn] || 0) + hits;
          }
        }

        const sorted = Object.entries(weightedCounts)
          .sort((a, b) => b[1] - a[1])
          .map(([name, weighted]) => ({
            name,
            pieces: rawCounts[name] || 0,
            weighted,
          }));

        await database.disconnect();
        return respond(200, {
          functions: sorted,
          total_pieces: (docs || []).length,
          total_hits: totalHits,
        });
      }

      // ── recent ──
      if (recent) {
        const clientLimit = Math.min(parseInt(q.limit) || 50, 100000);
        const sort = q.sort || "recent";
        const since = q.since;
        const filterHandle = q.handle;

        // When the caller narrows by handle, pull the full corpus from
        // the sidecar before the JS-side filter. Otherwise a 2000-row
        // "recent" window drops all of the user's older pieces (e.g.
        // @jeffrey has 700+ pieces scattered through ~17K docs by date).
        const sidecarLimit = filterHandle ? 100000 : clientLimit;
        const res = await sidecar.listCodes({ limit: sidecarLimit, sort, since });
        const pieces = res.recent || [];

        const handles = await handlesBySub(database, pieces.map((p) => p.user));

        const shaped = pieces
          .map((p) => {
            const handle = p.user ? handles.get(p.user) || null : null;
            const shape = toMongoShape(p, {
              handle,
              contract: requestedContract,
              contractProfile: requestedContractProfile,
              contractVersion: requestedContractVersion,
            }) || {};
            return {
              ...shape,
              code: p.code,
              preview:
                p.source && p.source.length > 40
                  ? p.source.substring(0, 37) + "..."
                  : p.source,
            };
          })
          .filter((p) => {
            if (!filterHandle) return true;
            const want = filterHandle.startsWith("@")
              ? filterHandle
              : `@${filterHandle}`;
            return p.handle === want;
          });

        const trimmed = filterHandle ? shaped.slice(0, clientLimit) : shaped;
        await database.disconnect();
        return respond(
          200,
          { recent: trimmed, count: trimmed.length, limit: clientLimit },
          NO_CACHE_HEADERS
        );
      }

      // ── batch lookup ──
      if (codes) {
        let codeList;
        try {
          codeList = codes.startsWith("[")
            ? JSON.parse(codes)
            : codes.split(",").map((c) => c.trim()).filter(Boolean);
        } catch {
          await database.disconnect();
          return respond(400, {
            error: "Invalid codes format. Use comma-separated or JSON array.",
          });
        }
        if (!Array.isArray(codeList) || !codeList.length) {
          await database.disconnect();
          return respond(400, { error: "Codes must be a non-empty array" });
        }
        if (codeList.length > 50) {
          await database.disconnect();
          return respond(400, {
            error: "Too many codes. Maximum 50 per request.",
          });
        }

        const { results: rawResults } = await sidecar.batchLookup(codeList);
        const foundSubs = Object.values(rawResults)
          .filter(Boolean)
          .map((r) => r.user)
          .filter(Boolean);
        const handles = await handlesBySub(database, foundSubs);

        const results = {};
        const found = [];
        const missing = [];
        for (const c of codeList) {
          const r = rawResults[c];
          if (r) {
            results[c] = toMongoShape(r, {
              handle: r.user ? handles.get(r.user) || null : null,
              contract: requestedContract,
              contractProfile: requestedContractProfile,
              contractVersion: requestedContractVersion,
            });
            found.push(c);
          } else {
            results[c] = null;
            missing.push(c);
          }
        }

        await database.disconnect();
        return respond(
          200,
          {
            results,
            summary: {
              requested: codeList.length,
              found: found.length,
              missing: missing.length,
              foundCodes: found,
              missingCodes: missing,
            },
          },
          NO_CACHE_HEADERS
        );
      }

      // ── single-code lookup ──
      if (!code) {
        await database.disconnect();
        return respond(400, { error: "Code or codes parameter required" });
      }

      const entity = await sidecar.lookupByCode(code);
      if (!entity) {
        await database.disconnect();
        return respond(404, { error: "Not found" });
      }

      const handle = entity.user
        ? (await handlesBySub(database, [entity.user])).get(entity.user)
        : null;

      await database.disconnect();
      return respond(
        200,
        toMongoShape(entity, {
          handle: handle || null,
          contract: requestedContract,
          contractProfile: requestedContractProfile,
          contractVersion: requestedContractVersion,
        }),
        NO_CACHE_HEADERS
      );
    }

    await database.disconnect();
    return respond(405, { error: "Method not allowed" });
  } catch (err) {
    console.error("❌ Datomic kidlisp error:", err);
    try { await database.disconnect(); } catch {}
    return respond(500, { error: "Internal server error", details: err.message });
  }
}
