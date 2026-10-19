#!/usr/bin/env node
// Install one migrated package with its pinned pnpm and frozen lockfile.
import { existsSync, readFileSync } from 'node:fs';
import { spawn, spawnSync } from 'node:child_process';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

export function installPlan(directory, production = false, installedVersion = '') {
  const cwd = path.resolve(directory);
  const pkg = JSON.parse(readFileSync(path.join(cwd, 'package.json'), 'utf8'));
  const match = /^pnpm@(\d+\.\d+\.\d+)$/.exec(pkg.packageManager || '');
  if (!match) throw new Error(`${cwd}: packageManager must pin an exact pnpm version`);
  if (!existsSync(path.join(cwd, 'pnpm-lock.yaml')))
    throw new Error(`${cwd}: missing pnpm-lock.yaml; refusing an unlocked install`);
  const args = ['install', '--frozen-lockfile', ...(production ? ['--prod'] : [])];
  return installedVersion.trim() === match[1]
    ? { command: 'pnpm', args, cwd }
    : { command: 'npx', args: ['--yes', `pnpm@${match[1]}`, ...args], cwd };
}

export async function install(directory, production = false) {
  // Probe outside the project so another pnpm version cannot auto-switch first.
  const probe = spawnSync('pnpm', ['--version'], { cwd: tmpdir(), encoding: 'utf8' });
  const plan = installPlan(directory, production, probe.status === 0 ? probe.stdout : '');
  await new Promise((resolve, reject) => {
    const child = spawn(plan.command, plan.args, { cwd: plan.cwd, stdio: 'inherit' });
    child.on('error', reject);
    child.on('exit', (code, signal) => code === 0 ? resolve() :
      reject(new Error(`pnpm install failed (${signal || code}); npm fallback is disabled`)));
  });
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const args = process.argv.slice(2);
  const unknown = args.filter(a => a.startsWith('-') && a !== '--prod');
  const dirs = args.filter(a => !a.startsWith('-'));
  if (unknown.length || dirs.length > 1) {
    console.error('Usage: node toolchain/pnpm-install.mjs [directory] [--prod]');
    process.exitCode = 1;
  } else {
    try { await install(dirs[0] || '.', args.includes('--prod')); }
    catch (error) { console.error(error.message); process.exitCode = 1; }
  }
}
