// Public property and event allowlists. No wildcard hosts or arbitrary labels.
export const CLIENT_VISIT_PROPERTIES = Object.freeze(["false.work", "danzballet.studio", "regarde.io", "drvkforlife.com"]);
export const VISIT_PROPERTIES = Object.freeze({
  "false.work": ["www.false.work"],
  "danzballet.studio": ["www.danzballet.studio"],
  "regarde.io": ["www.regarde.io"],
  "drvkforlife.com": ["www.drvkforlife.com"],
  "aesthetic.computer": ["www.aesthetic.computer", "p5.aesthetic.computer"],
  "oskiewar.com": ["www.oskiewar.com", "midi.oskiewar.com"],
  "nopaint.art": ["www.nopaint.art"],
  "whistlegraph.org": ["www.whistlegraph.org", "tv.whistlegraph.org"],
  "jas.life": ["www.jas.life", "rdp.jas.life"],
  "kidlisp.com": ["www.kidlisp.com", "learn.kidlisp.com", "keep.kidlisp.com", "buy.kidlisp.com", "pj.kidlisp.com", "top.kidlisp.com", "calm.kidlisp.com"],
  "notepat.com": ["www.notepat.com"],
  "laklok.com": ["www.laklok.com"],
  "sotce.net": ["www.sotce.net"],
  "prompt.ac": ["www.prompt.ac", "l5.prompt.ac", "p5.prompt.ac", "processing.prompt.ac"],
  "menuband.app": ["www.menuband.app"],
  "aesel.app": ["www.aesel.app"],
  "mime.ac": ["www.mime.ac"],
  "justanothersystem.org": ["www.justanothersystem.org"],
  "quiltnet.org": ["www.quiltnet.org"],
  "wipppps.world": ["www.wipppps.world"],
  "aesthetic.direct": ["www.aesthetic.direct"],
  "digitpain.com": ["www.digitpain.com"],
  "papers.aesthetic.computer": ["papers.prompt.ac"],
  "give.aesthetic.computer": [],
  "bills.aesthetic.computer": [],
  "pop.aesthetic.computer": [],
  "nft.aesthetic.computer": [],
  "rdp.aesthetic.computer": [],
  "l5.aesthetic.computer": [],
  "processing.aesthetic.computer": [],
});

export const VISIT_ACTIONS = Object.freeze([
  "link_followed", "download_clicked", "canvas_interacted", "media_started",
  "round_started", "round_completed", "match_completed",
  "mime_interact", "mime_scroll_feed", "mime_original_open",
]);
export const ACTIVE_BUCKETS = Object.freeze([0, 10, 30, 60, 180, 600]);
export const SURFACES = Object.freeze(["home", "play", "gallery", "read", "support", "other"]);
export const INPUTS = Object.freeze(["pointer", "touch", "keyboard", "scroll", "gamepad"]);
export const RETENTION_DAYS = 35;
export const VISIT_COLLECTION = "network-visits";

export function visitGroup(property) {
  return CLIENT_VISIT_PROPERTIES.includes(property) ? "clients" : "studio";
}

export function visitScopeMatch(scope = "studio") {
  if (!["studio", "clients", "all"].includes(scope)) throw new Error("Use --scope studio, clients, or all");
  return { property: { $in: Object.keys(VISIT_PROPERTIES).filter(property =>
    scope === "all" || visitGroup(property) === scope) } };
}

export function visitProperty(hostname) {
  const host = String(hostname || "").toLowerCase();
  return Object.entries(VISIT_PROPERTIES).find(([name, aliases]) =>
    name === host || aliases.includes(host))?.[0] || null;
}

export function visitSurface(pathname = "/") {
  let path;
  try { path = decodeURIComponent(pathname).toLowerCase(); } catch { return null; }
  // Private tools, chats, account/payment forms and explicit opt-out pages.
  if (/(?:^|\/)(?:admin|desk|mail|email|chat|aa|auth|login|signin|account|callback|checkout|wallet|device|crm|help|machines|session|consent)(?:[/.~:_-]|$)/.test(path)) return null;
  if (path === "/" || path === "/index.html") return "home";
  if (/privacy|support|terms/.test(path)) return "support";
  if (/gallery|painting|stars|wild/.test(path)) return "gallery";
  if (/paper|about|learn|read/.test(path)) return "read";
  if (/oskiewar|nopaint|notepat|kidlisp|laer-klokken|wipe|play|workshop/.test(path)) return "play";
  return "other";
}

export function automatedVisit(navigator, search = "", marker = false) {
  const params = new URLSearchParams(search);
  return Boolean(marker || navigator.webdriver ||
    /bot|crawler|spider|headless|lighthouse|curl|python|wget|monitor/i.test(navigator.userAgent || "") ||
    ["social-preview", "offline-render", "jev-vs-jev", "ac-automation"].some(key => params.has(key)));
}

// A cumulative snapshot makes retries and out-of-order delivery idempotent.
export function validateVisit(body, origin, userAgent = "") {
  let url;
  try { url = new URL(origin); } catch { return null; }
  const property = visitProperty(url.hostname);
  if (!property || url.protocol !== "https:" || url.port ||
      !body || body.version !== 1 ||
      !/^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89ab][a-f0-9]{3}-[a-f0-9]{12}$/i.test(body.id || "") ||
      !SURFACES.includes(body.surface) || !ACTIVE_BUCKETS.includes(body.activeSeconds) ||
      typeof body.interacted !== "boolean" || typeof body.automated !== "boolean" ||
      !Array.isArray(body.inputs) || body.inputs.length > INPUTS.length ||
      body.inputs.some(value => !INPUTS.includes(value)) ||
      !Array.isArray(body.actions) || body.actions.length > VISIT_ACTIONS.length ||
      body.actions.some(value => !VISIT_ACTIONS.includes(value))) return null;
  if (!body.interacted && (body.inputs.length || body.actions.length)) return null;
  return {
    id: body.id.toLowerCase(), property, surface: body.surface,
    activeSeconds: body.activeSeconds, interacted: body.interacted,
    automated: body.automated || automatedVisit({ userAgent }),
    inputs: [...new Set(body.inputs)], actions: [...new Set(body.actions)],
  };
}

export function visitUpdate(visit, now = new Date()) {
  const max = { activeSeconds: visit.activeSeconds, interacted: visit.interacted,
    automated: visit.automated, engaged: visit.interacted && visit.activeSeconds >= 10 };
  for (const input of visit.inputs) max[`inputs.${input}`] = true;
  for (const action of visit.actions) max[`actions.${action}`] = true;
  return {
    $setOnInsert: { property: visit.property, group: visitGroup(visit.property), surface: visit.surface,
      expiresAt: new Date(now.getTime() + RETENTION_DAYS * 86400000) },
    $min: { startedAt: now }, $max: { ...max, lastSeenAt: now },
  };
}

export function visitReportPipeline(start, end, byPeriod = false, scope = "studio") {
  const count = field => ({ $sum: { $cond: [`$${field}`, 1, 0] } });
  return [
    { $match: { ...visitScopeMatch(scope), startedAt: { $gte: start, $lt: end } } },
    { $group: {
      _id: { property: "$property", automated: "$automated", ...(byPeriod
        ? { period: { $floor: { $divide: [{ $subtract: ["$startedAt", start] }, 86400000] } } }
        : { surface: "$surface" }) },
      visits: { $sum: 1 }, interacted: count("interacted"), engaged: count("engaged"),
      activeSecondsLowerBound: { $sum: "$activeSeconds" },
      ...Object.fromEntries(VISIT_ACTIONS.map(action => [action, count(`actions.${action}`)])),
    } },
    { $sort: { "_id.property": 1, "_id.automated": 1, visits: -1 } },
  ];
}
