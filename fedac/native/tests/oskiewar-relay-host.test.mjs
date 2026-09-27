import test from 'node:test';
import assert from 'node:assert/strict';
import { oskiewarRelayBase } from '../lib/oskiewar-host.mjs';
const cloud = 'wss://session-server.aesthetic.computer/oskiewar-live';
test('only explicit private IPv4 LAN relay overrides cloud transport', () => {
  for (const ip of ['192.168.1.235', '10.0.0.5', '172.16.1.9', '172.31.255.2']) {
    const url = `ws://${ip}:7793/oskiewar-live`;
    assert.equal(oskiewarRelayBase(url+'\n'), url);
  }
  for (const bad of [null, '', 'ws://example.com:7793/oskiewar-live',
    'ws://8.8.8.8:7793/oskiewar-live', 'ws://172.32.0.1:7793/oskiewar-live',
    'ws://192.168.1.999:7793/oskiewar-live', 'ws://192.168.1.1:0/oskiewar-live',
    'ws://192.168.1.1:65536/oskiewar-live', 'ws://192.168.1.1:7793/oskiewar-live?secret=oops'])
    assert.equal(oskiewarRelayBase(bad), cloud);
});
