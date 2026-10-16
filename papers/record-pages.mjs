import { readFileSync, writeFileSync, mkdirSync } from 'node:fs';
import { resolve, join, sep } from 'node:path';

const escape = text => String(text || '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));

export function generateRecordPages(papersDir, siteDir, records) {
  mkdirSync(join(siteDir, 'records'), { recursive: true });
  for (const record of records) {
    if (!/^[a-z0-9-]+$/.test(record.id)) throw new Error('Invalid record id');
    let brief = '';
    if (record.publicBrief) {
      const path = resolve(papersDir, record.publicBrief);
      if (!path.startsWith(resolve(papersDir) + sep)) throw new Error('Brief outside papers directory');
      const source = readFileSync(path, 'utf8');
      // These public briefs use headings, paragraphs and simple lists.
      brief = source.split(/\n\s*\n/).map(block => {
        const text = escape(block.trim()).replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
        if (/^# /.test(text)) return ''; // The record title already names the document.
        if (/^## /.test(text)) return `<h2>${text.slice(3)}</h2>`;
        if (/^- /.test(text)) return `<ul>${text.split(/\n(?=- )/).map(item => `<li>${item.slice(2)}</li>`).join('')}</ul>`;
        if (/^\d+\. /.test(text)) return `<ol>${text.split(/\n(?=\d+\. )/).map(item => `<li>${item.replace(/^\d+\. /, '')}</li>`).join('')}</ol>`;
        return `<p>${text}</p>`;
      }).join('\n');
    }
    const availability = brief ? 'Working brief · PDF not yet built' : 'Submission record · No public document available';
    writeFileSync(join(siteDir, 'records', `${record.id}.html`), `<!doctype html>
<html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>${escape(record.title)}</title><link rel="stylesheet" href="/records.css">
<body class="record-document"><main><h1>${escape(record.title)}</h1><p>${escape(availability)}</p>
<p>${escape(record.venue)} · ${escape(record.track)} · ${escape(record.date)} · ${escape(record.status)}</p>
${brief || `<p>${escape(record.note)}</p>`}
</main></body></html>`);
  }
}
