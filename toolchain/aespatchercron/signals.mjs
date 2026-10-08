const sources = ["boots", "lith-journal", "lith-errors", "lith-access", "cloudflare", "chat-clock", "chat-system", "posthog"];
export const emptySignals = () => ({ format: "aespatcher.supplement.v1", coverage: [], leads: [], privateReports: [], metrics: [] });

export function validateSignals(data, routes) {
  const object = (row, required, optional = []) => row && typeof row === "object" && !Array.isArray(row) && required.every(k => Object.hasOwn(row, k)) && Object.keys(row).every(k => [...required, ...optional].includes(k));
  const count = n => Number.isSafeInteger(n) && n >= 0;
  const label = s => typeof s === "string" && /^[a-z0-9][a-z0-9._/-]{0,100}$/i.test(s);
  if (!object(data, ["format", "coverage", "leads", "privateReports", "metrics"]) || data.format !== "aespatcher.supplement.v1" ||
      ["coverage", "leads", "privateReports", "metrics"].some(k => !Array.isArray(data[k]) || data[k].length > 1000) ||
      data.coverage.some(r => !object(r, ["source", "status"], ["reason", "scanned", "truncated"]) || !sources.includes(r.source) ||
        !["available", "unavailable", "disabled"].includes(r.status) || (r.reason !== undefined && (typeof r.reason !== "string" || r.reason.length > 300)) ||
        (r.scanned !== undefined && !count(r.scanned)) || (r.truncated !== undefined && typeof r.truncated !== "boolean")) ||
      data.leads.some(r => !object(r, ["source", "kind", "route", "summary", "count", "evidenceRefs"]) || !sources.includes(r.source) || !label(r.kind) ||
        (r.route !== null && !routes.includes(r.route)) || typeof r.summary !== "string" || r.summary.length > 300 || /[\r\n<>{}@]/.test(r.summary) ||
        !count(r.count) || r.count < 1 || !Array.isArray(r.evidenceRefs) || r.evidenceRefs.length > 20 || r.evidenceRefs.some(ref => !/^[a-f0-9]{32}$/.test(ref))) ||
      data.privateReports.some(r => !object(r, ["ref", "source", "at", "text", "sourceHash"]) || !/^[a-f0-9]{32}$/.test(r.ref) || !["chat-clock", "chat-system"].includes(r.source) ||
        !Number.isFinite(Date.parse(r.at)) || typeof r.text !== "string" || r.text.length > 1500 || !/^[a-f0-9]{64}$/.test(r.sourceHash)) ||
      data.metrics.some(r => !object(r, ["source", "metric", "target", "count"]) || !sources.includes(r.source) || !label(r.metric) || (r.target !== null && !label(r.target)) || !count(r.count)))
    throw new Error("Rejected supplementary signal schema");
  const refs = new Set(data.privateReports.map(r => r.ref));
  if (data.leads.some(r => r.evidenceRefs.some(ref => !refs.has(ref)))) throw new Error("Missing private report evidence");
  return data;
}

export function mergeSignals(...groups) {
  const result = emptySignals();
  for (const group of groups) for (const key of ["coverage", "leads", "privateReports", "metrics"]) result[key].push(...group[key]);
  return result;
}

export function assertReportPrivacy(text, reports = []) {
  const words = s => s.toLowerCase().match(/[a-zæøå0-9]+/g) || [];
  const publicText = words(text).join(" ");
  for (const report of reports) {
    const source = words(report.text);
    for (let i = 0; i <= source.length - 6; i++) {
      if (publicText.includes(source.slice(i, i + 6).join(" "))) throw new Error("Public PR copies report wording; describe the reproduced source-level issue without chat quotations");
    }
  }
}
