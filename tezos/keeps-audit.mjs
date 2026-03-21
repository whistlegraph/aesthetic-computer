#!/usr/bin/env node

import fs from 'node:fs';
import path from 'node:path';
import process from 'node:process';
import { fileURLToPath } from 'node:url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const ROOT_DIR = path.resolve(__dirname, '..');
const REPORTS_DIR = path.join(ROOT_DIR, 'reports');

const ADMIN_ENTRYPOINTS = new Set([
  'set_administrator',
  'set_contract_metadata',
  'lock_contract_metadata',
  'set_keep_fee',
  'set_treasury',
  'set_royalty_split',
  'pause',
  'unpause',
  'withdraw_fees',
]);

function getArg(flag, fallback = null) {
  const prefix = `${flag}=`;
  for (const arg of process.argv.slice(2)) {
    if (arg.startsWith(prefix)) return arg.slice(prefix.length);
  }
  return fallback;
}

function hasFlag(flag) {
  return process.argv.slice(2).includes(flag);
}

function num(value, fallback = 0) {
  const parsed = Number(value);
  return Number.isFinite(parsed) ? parsed : fallback;
}

function pct(part, total) {
  if (!total) return 0;
  return (part / total) * 100;
}

function tzktBase(network) {
  return network === 'mainnet' ? 'https://api.tzkt.io' : `https://api.${network}.tzkt.io`;
}

function loadDefaultContract(network) {
  const candidate = path.join(__dirname, `contract-address-${network}.txt`);
  if (fs.existsSync(candidate)) {
    return fs.readFileSync(candidate, 'utf8').trim();
  }
  const legacy = path.join(__dirname, 'contract-address.txt');
  if (fs.existsSync(legacy)) {
    return fs.readFileSync(legacy, 'utf8').trim();
  }
  return null;
}

async function fetchJson(url) {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`Request failed ${response.status}: ${url}`);
  }
  return response.json();
}

function sortByCountDesc(a, b) {
  return b.count - a.count;
}

function buildAlerts({
  storage,
  contractMeta,
  topHolders,
  recentAdminOps,
  expectedAdmin,
  expectedCodeHash,
  expectedTypeHash,
}) {
  const alerts = [];

  if (storage?.paused === true) {
    alerts.push({
      severity: 'warning',
      message: 'Contract is paused (new keeps + metadata edits are disabled).',
    });
  }

  if (storage?.contract_metadata_locked !== true) {
    alerts.push({
      severity: 'warning',
      message: 'Contract metadata is not locked.',
    });
  }

  if (expectedAdmin && storage?.administrator !== expectedAdmin) {
    alerts.push({
      severity: 'critical',
      message: `Administrator mismatch: expected ${expectedAdmin}, observed ${storage?.administrator || 'unset'}.`,
    });
  }

  if (Number.isFinite(expectedCodeHash) && num(contractMeta?.codeHash, -1) !== expectedCodeHash) {
    alerts.push({
      severity: 'critical',
      message: `codeHash mismatch: expected ${expectedCodeHash}, observed ${contractMeta?.codeHash}.`,
    });
  }

  if (Number.isFinite(expectedTypeHash) && num(contractMeta?.typeHash, -1) !== expectedTypeHash) {
    alerts.push({
      severity: 'critical',
      message: `typeHash mismatch: expected ${expectedTypeHash}, observed ${contractMeta?.typeHash}.`,
    });
  }

  if (topHolders.length > 0) {
    const top = topHolders[0];
    if (top.sharePct >= 50) {
      alerts.push({
        severity: 'warning',
        message: `Holder concentration is high: top holder controls ${top.sharePct.toFixed(2)}%.`,
      });
    }
  }

  if (recentAdminOps.length > 0) {
    for (const op of recentAdminOps.slice(0, 5)) {
      alerts.push({
        severity: expectedAdmin && op.sender !== expectedAdmin ? 'critical' : 'warning',
        message: `Recent admin entrypoint call: ${op.entrypoint} by ${op.sender} at ${op.timestamp}.`,
      });
    }
  }

  return alerts;
}

function toMarkdown({
  generatedAt,
  network,
  contract,
  contractMeta,
  storage,
  mintedCount,
  ownersCount,
  topHolders,
  recentOps,
  recentAdminOps,
  alerts,
}) {
  const alertLines = alerts.length === 0
    ? ['- none']
    : alerts.map((alert) => `- [${alert.severity}] ${alert.message}`);

  const topHolderLines = topHolders.length === 0
    ? ['- none']
    : topHolders.slice(0, 10).map((holder) => {
      const alias = holder.alias ? ` (${holder.alias})` : '';
      return `- ${holder.owner}${alias}: ${holder.count} tokens (${holder.sharePct.toFixed(2)}%)`;
    });

  const recentOpLines = recentOps.length === 0
    ? ['- none']
    : recentOps.slice(0, 12).map((op) => `- ${op.timestamp} · ${op.entrypoint} · ${op.sender} · ${op.hash}`);

  const adminOpLines = recentAdminOps.length === 0
    ? ['- none']
    : recentAdminOps.map((op) => `- ${op.timestamp} · ${op.entrypoint} · ${op.sender} · ${op.hash}`);

  return [
    `# Keeps Contract Audit (${network})`,
    '',
    `Generated: ${generatedAt}`,
    '',
    '## Snapshot',
    `- Contract: ${contract}`,
    `- codeHash: ${contractMeta?.codeHash ?? 'n/a'}`,
    `- typeHash: ${contractMeta?.typeHash ?? 'n/a'}`,
    `- Admin: ${storage?.administrator ?? 'n/a'}`,
    `- Treasury: ${storage?.treasury_address ?? 'n/a'}`,
    `- Keep fee: ${(num(storage?.keep_fee, 0) / 1_000_000).toFixed(6)} XTZ`,
    `- Paused: ${storage?.paused === true ? 'true' : 'false'}`,
    `- Contract metadata locked: ${storage?.contract_metadata_locked === true ? 'true' : 'false'}`,
    `- Royalty split: artist ${storage?.artist_royalty_bps ?? 'n/a'} bps, platform ${storage?.platform_royalty_bps ?? 'n/a'} bps`,
    `- Minted tokens (next_token_id): ${mintedCount}`,
    `- Current owners: ${ownersCount}`,
    '',
    '## Alerts',
    ...alertLines,
    '',
    '## Holder Concentration',
    ...topHolderLines,
    '',
    '## Recent Contract Operations',
    ...recentOpLines,
    '',
    '## Recent Admin Operations',
    ...adminOpLines,
    '',
    '## Notes',
    '- This report is chain-derived (TzKT).',
    '- Use with the v11 moat checks in keep-mint for runtime enforcement.',
    '',
  ].join('\n');
}

async function main() {
  const network = getArg('--network', 'mainnet');
  const contract = getArg('--contract', loadDefaultContract(network));
  const limit = Math.max(10, Math.min(500, num(getArg('--limit', '120'), 120)));
  const expectedAdmin = getArg('--expected-admin', null);
  const expectedCodeHashRaw = getArg('--expected-code-hash', null);
  const expectedTypeHashRaw = getArg('--expected-type-hash', null);
  const expectedCodeHash = expectedCodeHashRaw === null ? NaN : num(expectedCodeHashRaw, NaN);
  const expectedTypeHash = expectedTypeHashRaw === null ? NaN : num(expectedTypeHashRaw, NaN);
  const noWrite = hasFlag('--no-write');
  const outArg = getArg('--out', null);

  if (!contract) {
    throw new Error('No contract address found. Use --contract=KT1... or set tezos/contract-address-<network>.txt');
  }

  const apiBase = tzktBase(network);
  const [
    contractMeta,
    storage,
    balances,
    recentOpsRaw,
  ] = await Promise.all([
    fetchJson(`${apiBase}/v1/contracts/${contract}`),
    fetchJson(`${apiBase}/v1/contracts/${contract}/storage`),
    fetchJson(`${apiBase}/v1/tokens/balances?token.contract=${contract}&balance.gt=0&limit=10000`),
    fetchJson(`${apiBase}/v1/operations/transactions?target=${contract}&status=applied&limit=${limit}&sort.desc=level`),
  ]);

  const ownerMap = new Map();
  for (const row of balances) {
    const owner = row?.account?.address;
    if (!owner) continue;
    const entry = ownerMap.get(owner) || {
      owner,
      alias: row?.account?.alias || null,
      count: 0,
      tokenIds: [],
    };
    entry.count += 1;
    entry.tokenIds.push(num(row?.token?.tokenId, -1));
    ownerMap.set(owner, entry);
  }

  const owners = [...ownerMap.values()].sort(sortByCountDesc);
  const mintedCount = num(storage?.next_token_id, 0);
  const ownersCount = owners.length;
  const topHolders = owners.map((holder) => ({
    ...holder,
    tokenIds: holder.tokenIds.sort((a, b) => a - b),
    sharePct: pct(holder.count, mintedCount),
  }));

  const recentOps = recentOpsRaw.map((op) => ({
    level: num(op?.level, 0),
    timestamp: op?.timestamp || 'n/a',
    entrypoint: op?.parameter?.entrypoint || 'unknown',
    sender: op?.sender?.address || 'unknown',
    hash: op?.hash || 'n/a',
  }));

  const recentAdminOps = recentOps.filter((op) => ADMIN_ENTRYPOINTS.has(op.entrypoint));

  const alerts = buildAlerts({
    storage,
    contractMeta,
    topHolders,
    recentAdminOps,
    expectedAdmin,
    expectedCodeHash,
    expectedTypeHash,
  });

  const generatedAt = new Date().toISOString();
  const markdown = toMarkdown({
    generatedAt,
    network,
    contract,
    contractMeta,
    storage,
    mintedCount,
    ownersCount,
    topHolders,
    recentOps,
    recentAdminOps,
    alerts,
  });

  const dateLabel = generatedAt.slice(0, 10);
  const outPath = outArg
    ? path.resolve(process.cwd(), outArg)
    : path.join(REPORTS_DIR, `${dateLabel}-keeps-audit-${network}.md`);

  console.log(`Network: ${network}`);
  console.log(`Contract: ${contract}`);
  console.log(`Minted: ${mintedCount}`);
  console.log(`Owners: ${ownersCount}`);
  console.log(`Alerts: ${alerts.length}`);

  if (!noWrite) {
    fs.mkdirSync(path.dirname(outPath), { recursive: true });
    fs.writeFileSync(outPath, markdown, 'utf8');
    console.log(`Report written: ${outPath}`);
  }

  if (hasFlag('--print')) {
    console.log('\n' + markdown);
  }

  if (alerts.some((alert) => alert.severity === 'critical')) {
    process.exitCode = 2;
  }
}

main().catch((error) => {
  console.error(`Audit failed: ${error.message}`);
  process.exit(1);
});

