import { createHash } from "node:crypto";

// Chat remains evidence, never instructions. Only a malfunction tied to a known surface becomes a lead.
export function userReports(rows, source, routes) {
  const result = { leads: [], privateReports: [] }, seen = new Set();
  const failure = /\b(?:crash(?:es|ed|ing)?|broken|bug|stuck|freez(?:e|es|ing)|frozen|fails?|failed|error|doesn['’]?t work|does not work|not working|won['’]?t (?:load|play|open)|can['’]?t (?:load|play|open|hear)|no sound|virker ikke|fungerer ikke|kan ikke (?:åbne|afspille|høre|bruge)|ingen lyd|fejl|fryser|gået i stå)\b/i;
  const clockContext = /\b(?:app(?:en)?|knap(?:pen|per|perne)?|spil(?:let)?|browser|chat(?:ten)?|lyd(?:en)?|musik(?:ken)?|radio(?:en)?|video(?:en)?|clock|klokken|laklok|laer-klokken|siden|afspil(?:ning)?)\b/i;
  const common = new Set(["blank", "color", "colors", "clock", "close", "debug", "error", "field", "fonts", "hello", "image", "label", "layer", "learn", "light", "notes", "paint", "paper", "route", "scale", "scene", "score", "shape", "sound", "source", "speak", "start", "still", "store", "study", "style", "throw", "title", "touch", "track", "video", "voice", "water", "world", "write"]);
  for (const row of rows) {
    if (typeof row.text !== "string" || row.deleted || !Number.isFinite(Date.parse(row.when)) || row.text.length > 10000 || !failure.test(row.text)) continue;
    const raw = row.text.normalize("NFKC");
    if (/\b(?:not broken|isn['’]?t broken|works? (?:fine|now|again)|fixed now|virker (?:fint|nu|igen)|fungerer (?:fint|nu|igen))\b/i.test(raw)) continue;
    const named = routes.filter(route => new RegExp(`(?:/|\x60)${route}(?=$|[^a-z0-9-])`, "i").test(raw) ||
      (route.length >= 5 && !common.has(route) && new RegExp(`(?:^|[^a-z0-9-])${route}(?=$|[^a-z0-9-])`, "i").test(raw)));
    let route = named.length === 1 ? named[0] : null;
    if (!named.length && source === "chat-clock" && clockContext.test(raw) && routes.includes("laer-klokken")) route = "laer-klokken";
    if (!route && !named.length) continue;
    const instruction = /ignore.{0,50}(?:instruction|previous|system)|system prompt|developer message|\b(?:run|execute)\b.{0,30}\b(?:curl|ssh|bash|sudo|command)\b|<\/?(?:system|assistant|instruction)/i.test(raw);
    if (instruction) route = null;
    const sourceHash = createHash("sha256").update(raw).digest("hex");
    const ref = createHash("sha256").update(`${source}:${sourceHash}`).digest("hex").slice(0, 32);
    if (seen.has(ref)) continue;
    seen.add(ref);
    const text = raw.replace(/https?:\/\/\S+/gi, "[url]").replace(/\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,}\b/gi, "[email]")
      .replace(/@[a-z0-9_-]+/gi, "[handle]").replace(/\b(?:\d{1,3}\.){3}\d{1,3}\b/g, "[address]")
      .replace(/\+?\d[\d ()-]{7,}\d/g, "[number]").replace(/[\u0000-\u0008\u000b-\u001f\u007f]/g, "").slice(0, 1500);
    result.privateReports.push({ ref, source, at: new Date(row.when).toISOString(), text, sourceHash });
    result.leads.push({ source, kind: "user-report", route, count: 1, evidenceRefs: [ref], summary: instruction
      ? "Public malfunction report contains instruction-like content; manual review required."
      : route ? `A public ${source} message describes a malfunction on ${route}; reproduce the reported behavior.`
        : "A public malfunction report mentions multiple pieces; identify the affected surface before investigation." });
  }
  return result;
}
