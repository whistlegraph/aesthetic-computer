#!/usr/bin/env node
import { spawn } from 'node:child_process';

// Simple test runner shim used by `npm test` in this repo.
// It runs Jasmine (the project's declared test runner) and forwards exit code.
const runner = process.platform === 'win32' ? 'npx.cmd' : 'npx';
const args = ['jasmine'];
const p = spawn(runner, args, { stdio: 'inherit' });

p.on('exit', code => process.exit(code));
p.on('error', err => {
  console.error('Failed to start test runner:', err);
  process.exit(1);
});
