#!/usr/bin/env node
import { VISIT_PROPERTIES, visitGroup } from "../../system/public/aesthetic.computer/lib/visit-model.mjs";
const domains = Object.keys(VISIT_PROPERTIES);
const rows = [];
for (let i = 0; i < domains.length; i += 4) {
  await Promise.all(domains.slice(i, i + 4).map(async property => {
    try {
      const response = await fetch(`https://${property}/`, { signal: AbortSignal.timeout(15000) });
      const html = await response.text();
      rows.push({ property, group: visitGroup(property), status: response.status, destination: new URL(response.url).hostname,
        installed: response.ok && (/\/visit-(?:tracker|shopify)\.mjs/.test(html)) });
    } catch { rows.push({ property, group: visitGroup(property), installed: false, error: "HTTPS probe failed" }); }
  }));
}
console.log(JSON.stringify({ checkedAt: new Date(), rows: rows.sort((a, b) => a.property.localeCompare(b.property)) }, null, 2));
