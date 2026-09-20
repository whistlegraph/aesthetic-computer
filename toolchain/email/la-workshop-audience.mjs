#!/usr/bin/env node
// Build a private, read-only LA-area candidate report from Lith. Never sends mail.
// node toolchain/email/la-workshop-audience.mjs --output /absolute/private/report.json
import { spawnSync } from 'node:child_process';
import { mkdirSync, writeFileSync } from 'node:fs';
import { dirname, isAbsolute } from 'node:path';

async function collect(allCalifornia = false, neighbors = false) {
  const { MongoClient } = await import('mongodb');
  const client = new MongoClient(process.env.MONGODB_CONNECTION_STRING, {
    maxPoolSize: 1, serverSelectionTimeoutMS: 10000,
  });
  await client.connect();
  try {
    const db = client.db(process.env.MONGODB_NAME);
    const since = new Date(Date.now() - 90 * 86400000);
    const cities = ['Los Angeles', 'East Los Angeles', 'Santa Monica', 'Burbank',
      'Pasadena', 'South Pasadena', 'Glendale', 'Long Beach', 'Inglewood',
      'Culver City', 'West Hollywood', 'Beverly Hills', 'Torrance', 'Alhambra',
      'Monterey Park', 'Monrovia', 'Azusa', 'Pomona', 'Hidden Hills'];
    const match = { createdAt: { $gte: since }, 'server.country': 'US',
      'server.region': { $in: ['California', 'CA'] }, 'server.city': { $in: cities } };
    if (allCalifornia || neighbors) delete match['server.city'];
    if (neighbors) match['server.region'] = { $in: ['Arizona', 'AZ', 'Nevada', 'NV', 'Oregon', 'OR'] };
    const locationSummary = await db.collection('boots').aggregate([
      { $match: match },
      { $group: { _id: {city: '$server.city', state: '$server.region'}, boots: { $sum: 1 },
        identifiedBoots: { $sum: { $cond: [
          { $ne: [{ $ifNull: ['$meta.user', null] }, null] }, 1, 0] } } } },
      { $sort: { boots: -1 } },
    ], { maxTimeMS: 15000 }).toArray();
    const accounts = await db.collection('boots').aggregate([
      { $match: match },
      { $set: { accountId: { $ifNull: ['$meta.user.sub', '$meta.user'] } } },
      { $match: { accountId: { $type: 'string', $ne: '' } } },
      { $group: { _id: '$accountId', cities: { $addToSet: '$server.city' },
        states: { $addToSet: '$server.region' },
        boots: { $sum: 1 }, lastSeen: { $max: '$createdAt' } } },
    ], { maxTimeMS: 15000 }).toArray();
    const normalize = (email) => String(email || '').trim().toLowerCase();
    const unsubs = new Set((await db.collection('email-blast-unsubscribes')
      .find({}, { projection: { email: 1 }, maxTimeMS: 10000 }).toArray()).map(x => normalize(x.email)));
    const muted = new Set();
    for (const name of ['chat-system-mutes', 'chat-clock-mutes', 'chat-sotce-mutes']) {
      for (const row of await db.collection(name).find({}, {
        projection: { user: 1 }, maxTimeMS: 10000,
      }).toArray()) muted.add(row.user);
    }
    const exclusions = { muted: 0, unsubscribed: 0, unverified: 0,
      blocked: 0, missingAccount: 0, missingEmail: 0, duplicate: 0, organizer: 0 };
    const candidates = [];
    const seen = new Set();
    const organizerEmails = new Set(['mail@aesthetic.computer', 'me@jas.life',
      'mail@whistlegraph.org', 'jeffrey@aesthetic.computer']);
    let token;
    if (accounts.length) {
      const res = await fetch('https://aesthetic.us.auth0.com/oauth/token', {
        method: 'POST', headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ client_id: process.env.AUTH0_M2M_CLIENT_ID,
          client_secret: process.env.AUTH0_M2M_SECRET,
          audience: 'https://aesthetic.us.auth0.com/api/v2/', grant_type: 'client_credentials' }),
        signal: AbortSignal.timeout(15000),
      });
      if (!res.ok) throw new Error(`Auth0 token request failed: ${res.status}`);
      token = (await res.json()).access_token;
    }
    for (const account of accounts) {
      if (muted.has(account._id)) { exclusions.muted++; continue; }
      const res = await fetch(`https://aesthetic.us.auth0.com/api/v2/users/${encodeURIComponent(account._id)}?fields=email,email_verified,blocked&include_fields=true`, {
        headers: { Authorization: `Bearer ${token}` }, signal: AbortSignal.timeout(15000),
      });
      if (res.status === 404) { exclusions.missingAccount++; continue; }
      if (!res.ok) throw new Error(`Auth0 account lookup failed: ${res.status}`);
      const user = await res.json();
      const email = normalize(user.email);
      if (user.blocked) { exclusions.blocked++; continue; }
      if (!user.email_verified) { exclusions.unverified++; continue; }
      if (!email || !email.includes('@')) { exclusions.missingEmail++; continue; }
      if (unsubs.has(email)) { exclusions.unsubscribed++; continue; }
      if (organizerEmails.has(email)) { exclusions.organizer++; continue; }
      if (seen.has(email)) { exclusions.duplicate++; continue; }
      seen.add(email);
      candidates.push({ email, cities: account.cities.sort(), states: account.states.sort(), boots: account.boots,
        lastSeen: account.lastSeen, priorInvitation: 'unchecked',
        basis: 'signed-in boot with approximate Cloudflare city; not confirmed residence' });
    }
    return { generatedAt: new Date(), since, scope: neighbors ? 'Arizona, Nevada, Oregon' : allCalifornia ? 'California' : 'LA area', cities: allCalifornia || neighbors ? null : cities, locationSummary,
      identifiedAccounts: accounts.length, exclusions,
      note: 'Candidates only. No mail sent. Check prior invitations and recheck suppressions before sending. IPs and account IDs are not exported.',
      candidates: candidates.sort((a, b) => a.email.localeCompare(b.email)) };
  } finally { await client.close(); }
}

const args = process.argv.slice(2);
const allCalifornia = args.includes('--california');
if (allCalifornia) args.splice(args.indexOf('--california'), 1);
const neighbors = args.includes('--neighbors');
if (neighbors) args.splice(args.indexOf('--neighbors'), 1);
if (args.length !== 2 || args[0] !== '--output' || !isAbsolute(args[1]) || (allCalifornia && neighbors)) {
  console.error('Usage: node toolchain/email/la-workshop-audience.mjs [--california | --neighbors] --output /absolute/private/report.json');
  process.exit(1);
}
const remote = spawnSync('ssh', ['-o', 'BatchMode=yes', '-o', 'ConnectTimeout=8',
  'root@lith.aesthetic.computer', 'cd /opt/ac/system && node --env-file=.env --input-type=module'], {
  input: `const collect = ${collect.toString()};\nconsole.log(JSON.stringify(await collect(${allCalifornia}, ${neighbors})));\n`,
  encoding: 'utf8', timeout: 180000, maxBuffer: 4 * 1024 * 1024,
});
if (remote.error || remote.status !== 0) {
  // Do not echo remote errors that could contain credentials or customer data.
  console.error(`Audience collection failed (${remote.error?.code || remote.status}). No report written.`);
  process.exit(1);
}
const report = JSON.parse(remote.stdout.trim());
mkdirSync(dirname(args[1]), { recursive: true, mode: 0o700 });
writeFileSync(args[1], JSON.stringify(report, null, 2) + '\n', { mode: 0o600, flag: 'wx' });
console.log(JSON.stringify({ output: args[1], identifiedAccounts: report.identifiedAccounts,
  candidates: report.candidates.length, exclusions: report.exclusions,
  locationSummary: report.locationSummary }, null, 2));
