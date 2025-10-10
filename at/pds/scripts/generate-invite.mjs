#!/usr/bin/env node
// generate-invite.mjs
// Generate an invite code on the PDS

import { spawn } from 'child_process';

console.log('🎫 Generating invite code on PDS...\n');

const ssh = spawn('ssh', [
  '-o', 'StrictHostKeyChecking=no',
  '-i', process.env.HOME + '/.ssh/aesthetic_pds',
  'root@138.197.35.160',
  'pdsadmin create-invite-code'
]);

let output = '';

ssh.stdout.on('data', (data) => {
  output += data.toString();
  process.stdout.write(data);
});

ssh.stderr.on('data', (data) => {
  process.stderr.write(data);
});

ssh.on('close', (code) => {
  if (code === 0) {
    // Extract invite code from output
    const match = output.match(/([a-z0-9]{4,}-[a-z0-9]{4,}-[a-z0-9]{4,}-[a-z0-9]{4,})/);
    if (match) {
      console.log('\n✨ Invite code generated!');
      console.log(`\nTo create jeffrey's account, run:`);
      console.log(`  node test-create-jeffrey.mjs ${match[1]}`);
    }
  }
  process.exit(code);
});
