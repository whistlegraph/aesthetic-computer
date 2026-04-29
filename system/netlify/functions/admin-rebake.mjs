// admin-rebake.mjs — Admin-only piece media rebake endpoint.
//
// POST /api/admin-rebake
//   Body: { piece }
//   Auth: AC Bearer token (must be admin)
//   Returns: { jobId, status, statusUrl }
//
// Triggers the keep-prepare-background pipeline in rebake mode for any
// piece — minted or not. Skips wallet/Tezos checks since admin is the
// caller. Use this to refresh broken oven artifacts (bundle/thumbnail)
// without walking the keep-mint UI. Poll /api/keep-status?jobId=… for
// progress; the resulting artifactUri/thumbnailUri land in
// kidlisp.<code>.pendingRebake (and ipfsMedia for first-bake pieces).
//
// Companion CLI: kidlisp-cli/rebake.mjs

import { authorize, hasAdmin, handleFor } from "../../backend/authorization.mjs";
import { connect } from "../../backend/database.mjs";
import { loadKidlispPiece } from "../../backend/kidlisp-read.mjs";
import { upsertJob, formatJobForClient, getJob } from "../../backend/keep-job.mjs";

const dev = process.env.CONTEXT === "dev";
if (dev) process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Content-Type": "application/json",
};

function jsonResponse(statusCode, body) {
  return { statusCode, headers: CORS_HEADERS, body: JSON.stringify(body) };
}

export const handler = async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers: CORS_HEADERS, body: "" };
  }
  if (event.httpMethod !== "POST") {
    return jsonResponse(405, { error: "Method not allowed" });
  }

  let body;
  try {
    body = JSON.parse(event.body || "{}");
  } catch {
    return jsonResponse(400, { error: "Invalid JSON body" });
  }

  const pieceName = body.piece?.replace(/^\$/, "").trim();
  if (!pieceName) return jsonResponse(400, { error: "Missing 'piece'" });

  const user = await authorize(event.headers);
  if (!user) return jsonResponse(401, { error: "Unauthorized" });
  if (!(await hasAdmin(user))) return jsonResponse(403, { error: "Admin only" });

  const database = await connect();
  const piece = await loadKidlispPiece(database, pieceName);
  if (!piece) return jsonResponse(404, { error: `Piece '$${pieceName}' not found` });

  const usersCol = database.db.collection("users");
  const userDoc = await usersCol.findOne({ _id: user.sub });
  // keep-job uniqueness key is (piece, wallet); use the admin's linked
  // wallet when available so jobs are scoped per-admin, else fall back
  // to a sentinel that's clearly an admin rebake.
  const wallet = userDoc?.tezos?.address || `admin:${user.sub}`;
  const userHandle = await handleFor(user.sub);

  // Clear any prior in-flight job so the rebake always runs fresh.
  const existingJob = await getJob(pieceName, wallet);
  if (existingJob) {
    try {
      await database.db
        .collection("keep-jobs")
        .deleteOne({ _id: existingJob._id });
    } catch (err) {
      console.warn("admin-rebake: failed to clear stale job:", err.message);
    }
  }

  const job = await upsertJob({
    piece: pieceName,
    wallet,
    user: user.sub,
    handle: userHandle,
    isRebake: true,
    regenerate: true,
  });

  const siteUrl =
    process.env.URL ||
    process.env.DEPLOY_URL ||
    (dev ? "http://localhost:8888" : "https://aesthetic.computer");

  const bgPayload = {
    jobId: job._id.toString(),
    pieceName,
    isRebake: true,
    regenerate: true,
    creatorWalletAddress: wallet,
    userHandle,
    walletAddress: wallet,
  };

  try {
    const bgRes = await fetch(`${siteUrl}/.netlify/functions/keep-prepare-background`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(bgPayload),
    });
    const bgOk = bgRes.status === 202 || bgRes.ok;
    if (!bgOk) {
      console.error(`admin-rebake: background HTTP ${bgRes.status}`);
      return jsonResponse(502, {
        error: `Background pipeline failed to launch (HTTP ${bgRes.status})`,
        jobId: job._id.toString(),
      });
    }
  } catch (err) {
    console.error("admin-rebake: background unreachable:", err.message);
    return jsonResponse(502, {
      error: `Background pipeline unreachable: ${err.message}`,
      jobId: job._id.toString(),
    });
  }

  return jsonResponse(200, {
    ...formatJobForClient(job),
    statusUrl: `/api/keep-status?jobId=${job._id.toString()}`,
  });
};
