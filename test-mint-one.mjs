#!/usr/bin/env node
import fs from 'fs';
import path from 'path';
import os from 'os';

const authPath = path.join(os.homedir(), '.ac-token');
const auth = JSON.parse(fs.readFileSync(authPath, 'utf8'));

process.env.NODE_TLS_REJECT_UNAUTHORIZED = '0';

const piece = process.argv[2] || '64i';
console.log(`Testing mint of $${piece}...`);

const response = await fetch('https://localhost:8888/api/keep-mint', {
  method: 'POST',
  headers: {
    'Authorization': `Bearer ${auth.access_token}`,
    'Content-Type': 'application/json',
    'Accept': 'text/event-stream',
  },
  body: JSON.stringify({ piece, mode: 'mint' }),
});

console.log('Status:', response.status);
const text = await response.text();
console.log('Response:', text);
