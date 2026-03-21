// keep-status.mjs — Lightweight polling endpoint for keep job progress.
//
// GET /api/keep-status?jobId=xxx
// GET /api/keep-status?piece=xxx&wallet=tz1xxx
//
// Returns current job state from MongoDB. ~100ms response time.
// Client polls every 2-3s to track preparation progress.

import { getJobById, getJob, formatJobForClient } from "../../backend/keep-job.mjs";

const CORS_HEADERS = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "Content-Type, Authorization",
  "Content-Type": "application/json",
  "Cache-Control": "no-cache",
};

function jsonResponse(statusCode, body) {
  return { statusCode, headers: CORS_HEADERS, body: JSON.stringify(body) };
}

export const handler = async (event) => {
  if (event.httpMethod === "OPTIONS") {
    return { statusCode: 200, headers: CORS_HEADERS, body: "" };
  }

  if (event.httpMethod !== "GET") {
    return jsonResponse(405, { error: "Method not allowed" });
  }

  const params = event.queryStringParameters || {};
  let job;

  if (params.jobId) {
    try {
      job = await getJobById(params.jobId);
    } catch {
      return jsonResponse(400, { error: "Invalid jobId" });
    }
  } else if (params.piece && params.wallet) {
    const piece = params.piece.replace(/^\$/, "").trim();
    job = await getJob(piece, params.wallet);
  } else {
    return jsonResponse(400, { error: "Provide jobId or piece+wallet" });
  }

  if (!job) {
    return jsonResponse(404, { error: "Job not found" });
  }

  return jsonResponse(200, formatJobForClient(job));
};
