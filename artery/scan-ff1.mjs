#!/usr/bin/env node
// Scan local network for FF1 devices (SSH on port 22, CDP on port 9222)
import net from 'net';

const subnets = ['192.168.1', '192.168.0', '192.168.86', '10.0.0', '10.0.1'];

async function scan(host, port) {
  return new Promise(r => {
    const s = new net.Socket();
    s.setTimeout(200);
    s.on('connect', () => { s.destroy(); r(true); });
    s.on('timeout', () => { s.destroy(); r(false); });
    s.on('error', () => { s.destroy(); r(false); });
    s.connect(port, host);
  });
}

console.log('🔍 Scanning for FF1 devices (ports 22, 9222)...\n');

for (const subnet of subnets) {
  process.stdout.write(`  ${subnet}.x `);
  for (let i = 1; i <= 254; i++) {
    const ip = `${subnet}.${i}`;
    const [ssh, cdp] = await Promise.all([scan(ip, 22), scan(ip, 9222)]);
    if (ssh || cdp) {
      console.log(`\n  ✅ FOUND: ${ip} - SSH:${ssh} CDP:${cdp}`);
    }
    if (i % 50 === 0) process.stdout.write('.');
  }
  console.log(' ✓');
}
console.log('\n🏁 Scan complete!');
