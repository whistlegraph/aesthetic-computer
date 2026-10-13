import test from 'node:test';
import assert from 'node:assert/strict';
import puppeteer from 'puppeteer';
import {RenderPool} from './whistlegraph-render.mjs';

test('silent renderer is replaced once, replaying the exact candidate; runtime errors are not retried', async () => {
  const launch = puppeteer.launch;
  try {
    for (const scenario of ['recover', 'silent', 'error']) {
      const sources = [], closed = []; let opened = 0;
      puppeteer.launch = async () => ({
        async createBrowserContext() {
          const index = opened++, context = {async close() {closed.push(index);}};
          context.newPage = async () => ({
            setViewport: async () => {}, evaluateOnNewDocument: async () => {}, goto: async () => {}, waitForFunction: async () => {}, browserContext: () => context,
            async evaluate(fn, source) {
              if (source !== undefined) {sources.push(source); return 'candidate-hash';}
              if (scenario === 'recover' && index > 0) return [{sourceHash: 'candidate-hash', kind: 'painted'}];
              if (scenario === 'error') return [{sourceHash: 'candidate-hash', kind: 'console', event: {level: 'warn', message: 'TypeError: broken piece at paint ('}}];
              return [];
            },
            screenshot: async () => 'cGl4ZWxz',
          });
          return context;
        }, close: async () => {},
      });
      const pool = new RenderPool({size: 1});
      await pool.start();
      try {
        const proof = await pool.render({source: 'exact candidate', renderID: 23}, {timeoutMs: 5, spanMs: 0, frames: 2});
        assert.equal(proof.rendered, scenario === 'recover');
        assert.deepEqual(sources, scenario === 'error' ? ['exact candidate'] : ['exact candidate', 'exact candidate']);
        assert.ok(closed.includes(0), 'the stalled tab cannot poison the next job');
        if (scenario === 'recover') {assert.equal(proof.frames.length, 2); assert.equal(proof.renderID, 23);}
      } finally {await pool.close();}
    }
  } finally {puppeteer.launch = launch;}
});
