import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, mkdirSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { sortPaperRows } from '../index-order.mjs';

test('actual PDFs come before missing, empty, and submission-only documents', () => {
  const dir = mkdtempSync(join(tmpdir(), 'paper-order-'));
  try {
    for (const name of ['old.pdf', 'new.pdf', 'undated.pdf']) writeFileSync(join(dir, name), '%PDF-1.7\n');
    writeFileSync(join(dir, 'empty.pdf'), '');
    mkdirSync(join(dir, 'directory.pdf'));
    const rows = [
      { title: 'Missing PDF', href: '/missing.pdf', created: '2026-10-16' },
      { title: 'Old PDF', href: '/old.pdf', created: '2026-03-03' },
      { title: 'Record', href: '/records/record.html', created: '2026-10-15' },
      { title: 'New PDF', href: '/new.pdf', created: '2026-09-20' },
      { title: 'Empty PDF', href: '/empty.pdf', created: '2026-10-14' },
      { title: 'Directory', href: '/directory.pdf', created: '2026-10-13' },
      { title: 'Undated PDF', href: '/undated.pdf' },
    ];
    assert.deepEqual(sortPaperRows(rows, dir).map(row => row.title),
      ['New PDF', 'Old PDF', 'Undated PDF', 'Missing PDF', 'Record', 'Empty PDF', 'Directory']);
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
