import { VISIT_ACTIONS, visitProperty, visitSurface, visitReferrer, visitGroup } from "./visit-model.mjs";
import { LAKLOK_ACTIONS, LAKLOK_PIECES, LAKLOK_FEATURE_VERSION } from "./laklok-activity.mjs";

export const ACCOUNT_ACTIVITY_COLLECTION = "account-activity";
export const SOTCE_ACTIONS = Object.freeze(["sotce_page_viewed", "sotce_page_visible_30s", "sotce_page_touched", "sotce_question_submitted"]);
export const ACCOUNT_ACTIONS = Object.freeze(["piece_opened", ...VISIT_ACTIONS, ...SOTCE_ACTIONS, ...LAKLOK_ACTIONS]);

export function accountActivityRoute(host, path, action) {
  if (visitSurface(path) === null) return false;
  if (visitProperty(host) !== "sotce.net") return true;
  // A saved-question milestone is the sole event allowed in the ask editor.
  if (path === "/ask" && action === "sotce_question_submitted") return true;
  return !/^\/(?:write|ask|respond|comment)(?:\/|$)/.test(path);
}
export const UUID = /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i;

export function activityPiece(path) {
  const builtIn = /^aesthetic\.computer\/disks\/([a-z0-9-]{1,64})$/i.exec(path || "");
  if (!builtIn) return "published-or-code";
  const piece = builtIn[1].toLowerCase();
  return visitSurface(`/${piece}`) === null ? null : piece;
}

export function validateAccountActivity(body, origin) {
  let url;
  try { url = new URL(origin); } catch { return null; }
  const property = visitProperty(url.hostname);
  if (!property || visitGroup(property) !== "studio" || url.protocol !== "https:" || url.port ||
      body?.version !== 1 || !UUID.test(body.id || "") || !UUID.test(body.session || "") ||
      !Number.isSafeInteger(body.sequence) || body.sequence < 1 || body.sequence > 1000000 ||
      !ACCOUNT_ACTIONS.includes(body.action) || typeof body.piece !== "string" ||
      !/^[a-z0-9-]{1,64}$/.test(body.piece) || visitSurface(`/${body.piece}`) === null ||
      typeof body.automated !== "boolean") return null;
  if (SOTCE_ACTIONS.includes(body.action) && (property !== "sotce.net" || body.piece !== "sotce")) return null;
  const laklok = property !== "sotce.net" && LAKLOK_PIECES.includes(body.piece) && body.featureVersion === LAKLOK_FEATURE_VERSION;
  if (LAKLOK_ACTIONS.includes(body.action) && !laklok) return null;
  return { id: body.id.toLowerCase(), session: body.session.toLowerCase(), sequence: body.sequence, property,
    ...(laklok ? { featureVersion: LAKLOK_FEATURE_VERSION } : {}),
    tenant: property === "sotce.net" ? "sotce" : "aesthetic", piece: body.piece,
    action: body.action, automated: body.automated,
    referrerHost: typeof body.referrerHost === "string" ? visitReferrer(body.referrerHost) : null };
}
