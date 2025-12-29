// web.mjs - Core CDP wrapper for webmeister browser automation
// Uses host.docker.internal:9333 for container -> VS Code CDP access

import WebSocket from 'ws';
import http from 'http';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const CDP_HOST = 'host.docker.internal';
const CDP_PORT = 9333;
const VAULT_PATH = path.join(__dirname, '../aesthetic-computer-vault');

export class Web {
  constructor(options = {}) {
    this.host = options.host || CDP_HOST;
    this.port = options.port || CDP_PORT;
    this.ws = null;
    this.msgId = 1;
    this.verbose = options.verbose || false;
  }

  log(...args) {
    if (this.verbose) console.log('[web]', ...args);
  }

  async listTargets() {
    return new Promise((resolve, reject) => {
      const req = http.get({
        hostname: this.host,
        port: this.port,
        path: '/json',
        headers: { 'Host': 'localhost' },
        timeout: 5000
      }, (res) => {
        let data = '';
        res.on('data', chunk => data += chunk);
        res.on('end', () => {
          try { resolve(JSON.parse(data)); }
          catch (e) { reject(new Error('Failed to parse CDP response')); }
        });
      });
      req.on('error', e => reject(new Error('CDP not accessible: ' + e.message)));
      req.on('timeout', () => { req.destroy(); reject(new Error('CDP timeout')); });
    });
  }

  async findTab(urlPattern) {
    const targets = await this.listTargets();
    const pages = targets.filter(t => t.type === 'page');
    if (typeof urlPattern === 'string') {
      const match = pages.find(t => t.url && t.url.includes(urlPattern));
      if (match) return match;
    }
    return pages.find(t => !t.url?.includes('workbench.html')) || pages[0];
  }

  async connect(urlPattern = null) {
    if (this.ws?.readyState === WebSocket.OPEN) return;
    const target = await this.findTab(urlPattern);
    if (!target) throw new Error('No browser tab found');
    this.log('Connecting to:', target.title || target.url);
    const wsUrl = 'ws://' + this.host + ':' + this.port + '/devtools/page/' + target.id;
    this.ws = new WebSocket(wsUrl, { headers: { 'Host': 'localhost' } });
    await new Promise((resolve, reject) => {
      const timeout = setTimeout(() => reject(new Error('WS timeout')), 10000);
      this.ws.on('open', () => { clearTimeout(timeout); resolve(); });
      this.ws.on('error', err => { clearTimeout(timeout); reject(err); });
    });
    this.log('Connected');
  }

  async send(method, params = {}) {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) throw new Error('Not connected');
    return new Promise((resolve, reject) => {
      const id = this.msgId++;
      const timeout = setTimeout(() => reject(new Error('Timeout: ' + method)), 30000);
      const handler = data => {
        try {
          const msg = JSON.parse(data);
          if (msg.id === id) {
            clearTimeout(timeout);
            this.ws.off('message', handler);
            msg.error ? reject(new Error(msg.error.message)) : resolve(msg.result);
          }
        } catch {}
      };
      this.ws.on('message', handler);
      this.ws.send(JSON.stringify({ id, method, params }));
    });
  }

  async eval(expression) {
    const result = await this.send('Runtime.evaluate', { expression, returnByValue: true });
    if (result.exceptionDetails) throw new Error('Eval error');
    return result.result?.value;
  }

  async goto(url) {
    this.log('Navigating to:', url);
    await this.send('Page.navigate', { url });
    await this.waitForLoad();
  }

  async waitForLoad(timeout = 30000) {
    const start = Date.now();
    while (Date.now() - start < timeout) {
      try {
        const state = await this.eval('document.readyState');
        if (state === 'complete') return;
      } catch {}
      await this.sleep(200);
    }
    throw new Error('Page load timeout');
  }

  async waitFor(selector, timeout = 10000) {
    this.log('Waiting for:', selector);
    const start = Date.now();
    while (Date.now() - start < timeout) {
      try {
        const found = await this.eval('!!document.querySelector("' + selector.replace('"', '') + '")');
        if (found) return true;
      } catch {}
      await this.sleep(200);
    }
    throw new Error('Selector timeout: ' + selector);
  }

  async click(selector) {
    await this.waitFor(selector);
    await this.eval('document.querySelector("' + selector.replace('"', '') + '").click()');
  }

  async type(selector, text) {
    await this.waitFor(selector);
    const escaped = JSON.stringify(text);
    await this.eval('(function(){var e=document.querySelector("' + selector.replace('"', '') + '");e.focus();e.value=' + escaped + ';e.dispatchEvent(new Event("input",{bubbles:true}));})()');
  }

  async getPageInfo() {
    return this.eval('({url:location.href,title:document.title})');
  }

  sleep(ms) { return new Promise(r => setTimeout(r, ms)); }
  close() { if (this.ws) { this.ws.close(); this.ws = null; } }

  static async loadCredentials(domain) {
    const credPath = path.join(VAULT_PATH, 'gigs', domain, 'credentials.json');
    const data = await fs.readFile(credPath, 'utf8');
    return JSON.parse(data);
  }
}

export default Web;