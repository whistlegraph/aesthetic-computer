import test from 'node:test';
import assert from 'node:assert/strict';
import { mkdtempSync, writeFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { installPlan } from './pnpm-install.mjs';

test('frozen installs use the exact manager and preserve production mode', () => {
  const dir = mkdtempSync(path.join(tmpdir(), 'ac-pnpm-install-'));
  try {
    writeFileSync(path.join(dir, 'package.json'), JSON.stringify({ packageManager: 'pnpm@12.11.2' }));
    assert.throws(() => installPlan(dir), /missing pnpm-lock/);
    writeFileSync(path.join(dir, 'pnpm-lock.yaml'), 'lockfileVersion: 9.0\n');
    assert.deepEqual(installPlan(dir, true, '12.11.2\n'), {
      command: 'pnpm', args: ['install', '--frozen-lockfile', '--prod'], cwd: dir,
    });
    assert.deepEqual(installPlan(dir, false, '10.0.0').args,
      ['--yes', 'pnpm@12.11.2', 'install', '--frozen-lockfile']);
    writeFileSync(path.join(dir, 'package.json'), JSON.stringify({ packageManager: 'pnpm@latest' }));
    assert.throws(() => installPlan(dir), /exact pnpm version/);
  } finally { rmSync(dir, { recursive: true, force: true }); }
});
