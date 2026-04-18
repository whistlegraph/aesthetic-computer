#!/usr/bin/env node
/**
 * ac-shop livereload — installs + controls the Shopify live-reload snippet.
 *
 * Commands:
 *   node livereload.mjs bump        Bump the version marker (triggers a reload in all open tabs).
 *   node livereload.mjs on          Flip the snippet on  (AC_LIVERELOAD_ENABLED = true).
 *   node livereload.mjs off         Flip the snippet off (AC_LIVERELOAD_ENABLED = false).
 *   node livereload.mjs status      Show current state (snippet present? enabled? last version?).
 */

import { readFileSync } from 'fs';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';

const __dirname = dirname(fileURLToPath(import.meta.url));
const envPath = join(__dirname, '../aesthetic-computer-vault/shop/.env');
const envContent = readFileSync(envPath, 'utf-8');
for (const line of envContent.split('\n')) {
  if (line && !line.startsWith('#') && line.includes('=')) {
    const [key, ...valueParts] = line.split('=');
    process.env[key.trim()] = valueParts.join('=').trim();
  }
}

const STORE = process.env.SHOPIFY_STORE_DOMAIN;
const TOKEN = process.env.SHOPIFY_ADMIN_ACCESS_TOKEN;
if (!STORE || !TOKEN) {
  console.error('❌ Missing SHOPIFY_STORE_DOMAIN / SHOPIFY_ADMIN_ACCESS_TOKEN');
  process.exit(1);
}

const base = `https://${STORE}/admin/api/2024-10`;

async function gq(path, options = {}) {
  const r = await fetch(`${base}${path}`, {
    headers: { 'X-Shopify-Access-Token': TOKEN, 'Content-Type': 'application/json', ...(options.headers || {}) },
    ...options,
  });
  if (!r.ok) {
    const txt = await r.text();
    throw new Error(`${r.status}: ${txt}`);
  }
  return r.json();
}

async function getMainTheme() {
  const { themes } = await gq('/themes.json');
  const main = themes.find((t) => t.role === 'main');
  if (!main) throw new Error('No main theme found');
  return main;
}

async function readAsset(themeId, key) {
  try {
    const { asset } = await gq(`/themes/${themeId}/assets.json?asset[key]=${encodeURIComponent(key)}`);
    return asset?.value ?? null;
  } catch (e) {
    if (String(e).includes('404')) return null;
    throw e;
  }
}

async function writeAsset(themeId, key, value) {
  await gq(`/themes/${themeId}/assets.json`, {
    method: 'PUT',
    body: JSON.stringify({ asset: { key, value } }),
  });
}

async function bump() {
  const theme = await getMainTheme();
  const v = String(Date.now());
  await writeAsset(theme.id, 'assets/ac-livereload-version.txt', v);
  console.log(`✅ bumped ac-livereload-version.txt → ${v}`);
}

async function setEnabled(nextEnabled) {
  const theme = await getMainTheme();
  const snippet = await readAsset(theme.id, 'snippets/ac-livereload.liquid');
  if (!snippet) {
    console.error('❌ snippets/ac-livereload.liquid not found — re-run the install script first.');
    process.exit(1);
  }
  const newValue = nextEnabled
    ? snippet.replace(/AC_LIVERELOAD_ENABLED = false/, 'AC_LIVERELOAD_ENABLED = true')
    : snippet.replace(/AC_LIVERELOAD_ENABLED = true/, 'AC_LIVERELOAD_ENABLED = false');
  if (newValue === snippet) {
    console.log(`ℹ️  already ${nextEnabled ? 'enabled' : 'disabled'}`);
    return;
  }
  await writeAsset(theme.id, 'snippets/ac-livereload.liquid', newValue);
  console.log(`✅ livereload is now ${nextEnabled ? 'ENABLED' : 'DISABLED'}`);
  // Trigger a reload so open browsers pick up the new snippet state.
  await bump();
}

async function status() {
  const theme = await getMainTheme();
  const snippet = await readAsset(theme.id, 'snippets/ac-livereload.liquid');
  const version = await readAsset(theme.id, 'assets/ac-livereload-version.txt');
  const themeLiquid = await readAsset(theme.id, 'layout/theme.liquid');
  const rendered = themeLiquid?.includes(`render 'ac-livereload'`) ?? false;
  const enabled = snippet?.match(/AC_LIVERELOAD_ENABLED = (true|false)/)?.[1] ?? 'unknown';
  console.log('theme:             ', theme.name, `(id=${theme.id})`);
  console.log('snippet present:   ', !!snippet);
  console.log('rendered in layout:', rendered);
  console.log('AC_LIVERELOAD:     ', enabled);
  console.log('last version:      ', version);
}

const cmd = process.argv[2];
switch (cmd) {
  case 'bump':
    await bump();
    break;
  case 'on':
    await setEnabled(true);
    break;
  case 'off':
    await setEnabled(false);
    break;
  case 'status':
    await status();
    break;
  default:
    console.log('Usage: node livereload.mjs {bump|on|off|status}');
    process.exit(1);
}
