import assert from "node:assert/strict";
import { execFileSync } from "node:child_process";
import { mkdtempSync, writeFileSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join, delimiter } from "node:path";
import test from "node:test";

function portal(mode, run) {
  const dir = mkdtempSync(join(tmpdir(), "xbox-live-test-"));
  const state = join(dir, "state.json");
  writeFileSync(state, JSON.stringify({ published: false, calls: [] }));
  writeFileSync(join(dir, "diagnostic.js"), "function boot() {}\n");
  writeFileSync(join(dir, "curl"), `#!/usr/bin/env node
const fs = require('node:fs');
const path = process.env.TEST_PORTAL_STATE;
const state = JSON.parse(fs.readFileSync(path, 'utf8'));
const args = process.argv.slice(2);
const url = args.at(-1);
state.calls.push(args);
if (args.includes('POST')) state.published = true;
fs.writeFileSync(path, JSON.stringify(state));
if (url.includes('/api/app/packagemanager/packages')) {
  process.stdout.write(JSON.stringify({ InstalledPackages: [{
    PackageFamilyName: 'AestheticComputer.NativeBios',
    PackageFullName: 'AestheticComputer.NativeBios_1.0.0.41_x64__test',
    Version: { Revision: 41 }
  }] }));
} else if (url.includes('/api/filesystem/apps/file')) {
  if (args.includes('POST')) {
    if (process.env.TEST_PORTAL_MODE === 'upload-error') process.exit(22);
  } else {
    process.stdout.write('AC_NATIVE_LIVE_READY bytes=10 generation=7\\n');
    if (state.published && process.env.TEST_PORTAL_MODE !== 'pending') process.stdout.write(process.env.TEST_PORTAL_MODE === 'reject'
      ? 'AC_NATIVE_LIVE_REJECT reason=syntax-error\\n'
      : 'AC_NATIVE_LIVE_READY bytes=19 generation=' + (process.env.TEST_PORTAL_MODE === 'reset' ? 2 : 8) + '\\n');
  }
} else process.exit(99);
`, { mode: 0o755 });
  try {
    run(() => execFileSync(process.execPath,
      [new URL('./live.mjs', import.meta.url).pathname, 'hot-deploy', join(dir, 'diagnostic.js')], {
        encoding: 'utf8', stdio: 'pipe', env: { ...process.env,
          PATH: dir + delimiter + process.env.PATH,
          TEST_PORTAL_STATE: state, TEST_PORTAL_MODE: mode,
          XBOX_DEVICE_PORTAL_ENV: join(dir, 'unused.env'),
          XBOX_DEVICE_PORTAL_HOST: 'test.invalid',
          XBOX_DEVICE_PORTAL_USERNAME: 'test', XBOX_DEVICE_PORTAL_PASSWORD: 'test' },
      }), () => JSON.parse(readFileSync(state, 'utf8')));
  } finally { rmSync(dir, { recursive: true, force: true }); }
}

test('hot deploy verifies native activation without launching the app', () => {
  portal('ready', (deploy, state) => {
    assert.match(deploy(), /live reload verified: generation 8, bytes 19; no Xbox restart needed/);
    const calls = state().calls;
    assert.equal(calls.filter(args => args.includes('POST')).length, 1);
    assert.ok(calls.every(args => !args.at(-1).includes('/api/taskmanager/app')));
  });
});

test('hot deploy verifies activation after the application generation resets', () => {
  portal('reset', (deploy) => {
    assert.match(deploy(), /live reload verified: generation 2, bytes 19/);
  });
});

test('hot deploy reports a rejected update instead of claiming it loaded', () => {
  portal('reject', (deploy) => {
    assert.throws(deploy, error => /Live update rejected.*syntax-error/.test(error.stderr));
  });
});

test('hot deploy fails on an upload error', () => {
  portal('upload-error', (deploy) => {
    assert.throws(deploy, error => /curl exited 22/.test(error.stderr));
  });
});

test('unconfirmed hot deploy tells the player to reopen the app, not reboot the console', () => {
  portal('pending', (deploy) => {
    assert.throws(deploy, error => /Quit and reopen oskiewar on the Xbox.*no console reboot is needed/.test(error.stderr));
  });
});
