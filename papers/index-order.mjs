import { statSync } from 'node:fs';
import { join } from 'node:path';

export function sortPaperRows(rows, siteDir) {
  const available = new Set(rows.filter(row => {
    if (!row.href?.startsWith('/') || !row.href.endsWith('.pdf')) return false;
    try {
      const file = statSync(join(siteDir, row.href.slice(1)));
      return file.isFile() && file.size > 0;
    } catch { return false; }
  }));
  return rows.sort((a, b) => Number(available.has(b)) - Number(available.has(a))
    || String(b.created || '').localeCompare(String(a.created || ''))
    || a.title.localeCompare(b.title));
}
