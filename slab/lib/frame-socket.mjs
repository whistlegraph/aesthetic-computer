import net from 'node:net';
import { randomUUID } from 'node:crypto';

// Missing/refused endpoint before connect is the only safe legacy fallback.
// Every failure after connecting is an unknown outcome: never replay input.
export function socketFrame(path, mode, { timeoutMs = 15000 } = {}) {
  return new Promise((resolve, reject) => {
    const id = randomUUID(), socket = net.createConnection(path);
    let connected = false, settled = false, size = 0, header, expected;
    const chunks = [];
    const finish = (error, value) => {
      if (settled) return;
      settled = true; clearTimeout(timer); socket.destroy();
      error ? reject(error) : resolve(value);
    };
    const unknown = reason => finish(new Error(`native frame ${reason}; outcome unknown, request not retried`));
    const timer = setTimeout(() => unknown('timeout'), timeoutMs);
    socket.on('connect', () => {
      connected = true;
      socket.write(JSON.stringify({ id, mode }) + '\n');
    });
    socket.on('error', error => {
      if (!connected && ['ENOENT', 'ECONNREFUSED'].includes(error.code)) finish(null, null);
      else unknown(error.message);
    });
    socket.on('end', () => { if (!settled) unknown('truncated response'); });
    socket.on('close', () => { if (!settled) unknown('disconnected'); });
    socket.on('data', chunk => {
      size += chunk.length; chunks.push(chunk);
      if (size > 16 * 1024 * 1024 + 12) return unknown('oversize response');
      if (!header && size >= 12) {
        header = Buffer.concat(chunks, size).subarray(0, 12);
        if (header.toString('ascii', 0, 4) !== 'ACF2') return unknown('invalid header');
        expected = 12 + header.readUInt32BE(4) + header.readUInt32BE(8);
        if (expected > 16 * 1024 * 1024 + 12) return unknown('oversize response');
      }
      if (!header || size < expected) return;
      if (size !== expected) return unknown('trailing response bytes');
      try {
        const packet = Buffer.concat(chunks, size), end = 12 + header.readUInt32BE(4);
        const response = JSON.parse(packet.toString('utf8', 12, end));
        if (response.id !== id || !response.frame || typeof response.frame !== 'object') return unknown('mismatched response');
        finish(null, { json: JSON.stringify(response.frame), jpg: packet.subarray(end) });
      } catch { unknown('invalid JSON'); }
    });
  });
}
