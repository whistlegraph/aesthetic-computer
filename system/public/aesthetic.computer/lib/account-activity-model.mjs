import { VISIT_ACTIONS, visitProperty, visitSurface, visitReferrer, visitGroup } from "./visit-model.mjs";

export const ACCOUNT_ACTIVITY_COLLECTION = "account-activity";
export const ACCOUNT_ACTIONS = Object.freeze(["piece_opened", ...VISIT_ACTIONS]);
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
  return { id: body.id.toLowerCase(), session: body.session.toLowerCase(), sequence: body.sequence, property,
    tenant: property === "sotce.net" ? "sotce" : "aesthetic", piece: body.piece,
    action: body.action, automated: body.automated,
    referrerHost: typeof body.referrerHost === "string" ? visitReferrer(body.referrerHost) : null };
}
