// Shared validation and authorization rules for the Whistlegraph archive desk.

export const DEFAULT_WHISTLEGRAPH_ADMIN_SUBS = Object.freeze([
  "auth0|63effeeb2a7d55f8098d62f9", // @jeffrey
  "auth0|6414a4fb936cc041cfc1f011", // @minanimals
]);

const POST_KINDS = new Set(["performance", "talk", "other"]);
const CODE_RE = /^[A-Za-z0-9]{1,24}$/;
const COLOR_RE = /^#[0-9a-f]{6}$/i;

export function whistlegraphAdminSubs(env = process.env) {
  const configured = String(env.WHISTLEGRAPH_ADMIN_SUBS || "")
    .split(",")
    .map((value) => value.trim())
    .filter(Boolean);
  return new Set(configured.length ? configured : DEFAULT_WHISTLEGRAPH_ADMIN_SUBS);
}
export function isWhistlegraphAdmin(user, allowed = whistlegraphAdminSubs()) {
  return Boolean(user?.sub && user.email_verified && allowed.has(user.sub));
}

function own(object, key) {
  return Object.prototype.hasOwnProperty.call(object || {}, key);
}

function cleanText(value, label, max, { empty = true } = {}) {
  if (typeof value !== "string") throw new Error(`${label} must be text.`);
  const text = value.trim();
  if (!empty && !text) throw new Error(`${label} cannot be empty.`);
  if (text.length > max) throw new Error(`${label} is too long (max ${max}).`);
  return text;
}

function cleanStringList(value, label, { maxItems = 24, maxLength = 80 } = {}) {
  if (!Array.isArray(value)) throw new Error(`${label} must be a list.`);
  if (value.length > maxItems) throw new Error(`${label} has too many items (max ${maxItems}).`);
  const items = value.map((item) => cleanText(item, label, maxLength, { empty: false }));
  return [...new Set(items)];
}

export function normalizePostPatch(input, validWorkCodes = new Set()) {
  if (!input || typeof input !== "object" || Array.isArray(input)) {
    throw new Error("Post patch must be an object.");
  }
  const patch = {};
  if (own(input, "kind")) {
    if (!POST_KINDS.has(input.kind)) throw new Error("Unknown post type.");
    patch.kind = input.kind;
  }
  if (own(input, "desc")) patch.desc = cleanText(input.desc, "Description", 4000);
  if (own(input, "works")) {
    const works = cleanStringList(input.works, "Works", { maxItems: 32, maxLength: 24 });
    for (const code of works) {
      if (!CODE_RE.test(code) || !validWorkCodes.has(code)) {
        throw new Error(`Unknown Whistlegraph code: ${code}`);
      }
    }
    patch.works = works;
  }
  if (own(input, "plots")) {
    patch.plots = cleanStringList(input.plots, "Plots", { maxItems: 24, maxLength: 100 });
  }
  if (!Object.keys(patch).length) throw new Error("No editable post fields supplied.");
  return patch;
}

export function normalizeWorkPatch(input) {
  if (!input || typeof input !== "object" || Array.isArray(input)) {
    throw new Error("Whistlegraph patch must be an object.");
  }
  const patch = {};
  if (own(input, "title")) patch.title = cleanText(input.title, "Title", 180, { empty: false });
  if (own(input, "by")) patch.by = cleanText(input.by, "Attribution", 240, { empty: false });
  if (own(input, "year")) {
    const year = Number(input.year);
    if (!Number.isInteger(year) || year < 1900 || year > 2200) throw new Error("Year is invalid.");
    patch.year = year;
  }
  if (own(input, "c")) {
    const color = cleanText(input.c, "Color", 7, { empty: false });
    if (!COLOR_RE.test(color)) throw new Error("Color must be a six-digit hex value.");
    patch.c = color.toLowerCase();
  }
  if (!Object.keys(patch).length) throw new Error("No editable Whistlegraph fields supplied.");
  return patch;
}

export function curationPayload(documents = []) {
  const works = {};
  const posts = {};
  let revision = null;
  for (const document of documents) {
    if (!document?.key || !document?.patch) continue;
    if (document.type === "work") works[document.key] = document.patch;
    if (document.type === "post") posts[document.key] = document.patch;
    const stamp = document.updatedAt ? new Date(document.updatedAt).toISOString() : null;
    if (stamp && (!revision || stamp > revision)) revision = stamp;
  }
  return { revision, works, posts };
}
