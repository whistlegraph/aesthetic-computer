import { existsSync, readFileSync } from "node:fs";
import { join } from "node:path";

const DIRECTIVE = /^([A-Z][A-Z0-9_]+):\s*(.+)$/gm;

export function missionDirectives(notes = "") {
  const values = {};
  for (const match of String(notes).matchAll(DIRECTIVE)) {
    values[match[1]] = match[2].trim();
  }
  return {
    missionId: values.MISSION_ID || "",
    missionKind: values.MISSION_KIND || "",
    priority: values.MISSION_PRIORITY?.toLowerCase() || "normal",
    executionHost: values.EXECUTION_HOST?.toLowerCase() || "",
    approvedBy: values.APPROVED_BY || "",
    originalAssetId: values.ORIGINAL_ASSET_ID || "",
  };
}

export function classifyAssignedTask(task) {
  const directives = missionDirectives(task.notes);
  const tags = new Set((task.tags || []).map((tag) => String(tag).toLowerCase()));
  const kind = directives.missionKind === "fuser-recovery"
    ? "mission"
    : tags.has("mission") && tags.has("captutor") ? "captutor" : "pr";
  return { ...task, ...directives, kind };
}

export function prioritySort(tasks) {
  return [...tasks].sort((a, b) => {
    const ap = a.priority === "blocking" ? 0 : 1;
    const bp = b.priority === "blocking" ? 0 : 1;
    return ap - bp;
  });
}

export function unresolvedBlockingTask(tasks, done = {}) {
  return tasks.find((task) => task.priority === "blocking"
    && done?.[task.gid]?.status !== "done") || null;
}

export function nextRunnableTask(tasks, done = {}, active = null) {
  const blocking = unresolvedBlockingTask(tasks, done);
  if (blocking) return done?.[blocking.gid] ? null : blocking;
  // Captutor assignments are deliverables, not best-effort queue items. If an
  // assigned render failed, stop here until its guarded recovery clears the
  // tombstone or a human explicitly unassigns it. Advancing to another video
  // would hide the missing docs/vertical pair behind unrelated progress.
  const failedCaptutor = tasks.find((task) => task.kind === "captutor"
    && done?.[task.gid]?.status === "failed");
  if (failedCaptutor) return null;
  return tasks.find((task) => !done?.[task.gid]
    && (!active || task.gid !== active.taskGid)) || null;
}

export function shouldPreempt(active, blocking) {
  return Boolean(active && blocking && active.taskGid !== blocking.gid
    && active.priority !== "blocking");
}

export function missionReceiptPath(receiptDir, taskGid) {
  return join(receiptDir, `${taskGid}.json`);
}

export function validateMissionReceipt(receipt, task) {
  if (!receipt || receipt.schema !== "iris-mission-receipt/v1") {
    throw new Error("missing iris-mission-receipt/v1 receipt");
  }
  if (String(receipt.taskGid || "") !== String(task.gid)) {
    throw new Error(`receipt task mismatch: ${receipt.taskGid || "missing"}`);
  }
  if (receipt.status !== "complete") throw new Error("mission receipt is not complete");
  if (task.missionId && receipt.missionId !== task.missionId) {
    throw new Error(`receipt mission mismatch: ${receipt.missionId || "missing"}`);
  }
  if (task.executionHost && receipt.executionHost !== task.executionHost) {
    throw new Error(`mission ran on ${receipt.executionHost || "unknown"}, expected ${task.executionHost}`);
  }
  if (task.originalAssetId && receipt.originalAssetId !== task.originalAssetId) {
    throw new Error("receipt original asset mismatch");
  }
  if (receipt.originalPreserved !== true) throw new Error("original preservation is unverified");
  if (receipt.chunkedResourceVerified !== true) throw new Error("chunked resource path is unverified");
  if (!receipt.candidateAssetId || receipt.candidateAssetId === receipt.originalAssetId) {
    throw new Error("fresh repaired candidate is unverified");
  }
  if (!receipt.ownerHandoff?.projectId || receipt.ownerHandoff?.approved !== true) {
    throw new Error("reviewed owner handoff is unverified");
  }
  return receipt;
}

export function readMissionReceipt(receiptDir, task) {
  const path = missionReceiptPath(receiptDir, task.gid);
  if (!existsSync(path)) return null;
  return validateMissionReceipt(JSON.parse(readFileSync(path, "utf8")), task);
}

export function validateMissionApproval(approval, task) {
  if (!approval || approval.schema !== "iris-mission-approval/v1") {
    throw new Error("missing iris-mission-approval/v1 approval");
  }
  if (String(approval.taskGid || "") !== String(task.gid)) {
    throw new Error("approval task mismatch");
  }
  if (approval.missionId !== task.missionId) throw new Error("approval mission mismatch");
  if (approval.executionHost !== task.executionHost) throw new Error("approval execution host mismatch");
  if (approval.originalAssetId !== task.originalAssetId) throw new Error("approval original asset mismatch");
  if (!approval.approvedBy || !approval.approvedAt) throw new Error("approval attribution is incomplete");
  return approval;
}

export function readMissionApproval(path, task) {
  if (!existsSync(path)) return null;
  const ledger = JSON.parse(readFileSync(path, "utf8"));
  if (ledger.schema !== "iris-mission-approvals/v1" || !Array.isArray(ledger.approvals)) {
    throw new Error("invalid mission approval ledger");
  }
  const approval = ledger.approvals.find((entry) => String(entry.taskGid) === String(task.gid));
  return approval ? validateMissionApproval(approval, task) : null;
}
