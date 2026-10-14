// Separate, aggregate-only AC session checks. Never join these IDs to visits.
import { automatedVisit, visitSurface } from "./visit-model.mjs";

export const RECOGNITION_ORIGIN = "https://aesthetic.computer";
export const RECOGNITION_PARENTS = Object.freeze(["https://sotce.net", "https://www.sotce.net"]);
export const RECOGNITION_COLLECTION = "network-recognition";
export const RECOGNITION_UUID = /^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i;
export const RECOGNITION_TIMEOUT = 10000;

export function recognitionEnabled(win) {
  return win.navigator.doNotTrack !== "1" && win.doNotTrack !== "1" &&
    win.navigator.globalPrivacyControl !== true && !win.acVisitTrackingDisabled &&
    !win.acPACK_MODE && !automatedVisit(win.navigator, win.location.search, win.acAutomation === true);
}

export function recognitionPage(win, doc) {
  let path;
  try { path = decodeURIComponent(win.location.pathname); } catch { return false; }
  return win === win.top && RECOGNITION_PARENTS.includes(win.location.origin) &&
    recognitionEnabled(win) && doc.visibilityState === "visible" &&
    visitSurface(win.location.pathname) !== null &&
    !/^\/(?:write|ask|respond|comment)(?:[/.~:_-]|$)/i.test(path);
}

export function validateRecognition(body, origin) {
  if (!body || body.version !== 1 || !RECOGNITION_UUID.test(body.id || "") ||
      Object.keys(body).some(key => !["version", "id", "phase"].includes(key))) return null;
  if (body.phase === "started" && RECOGNITION_PARENTS.includes(origin) ||
      body.phase === "checked" && origin === RECOGNITION_ORIGIN)
    return { id: body.id.toLowerCase(), phase: body.phase, property: "sotce.net" };
  return null;
}
