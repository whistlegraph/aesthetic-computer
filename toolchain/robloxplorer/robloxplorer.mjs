#!/usr/bin/env node
// Roblox network exploration and Aesthetic Network publishing.
import { readFileSync, existsSync } from 'node:fs';
import { resolve, extname } from 'node:path';
import { pathToFileURL } from 'node:url';
import { createHash } from 'node:crypto';

const root = resolve(import.meta.dirname, '../..');
export const endpoints = {
  user: 'https://users.roblox.com/v1/users/{userId}',
  search: 'https://users.roblox.com/v1/users/search?keyword={keyword}&limit=10',
  games: 'https://games.roblox.com/v2/users/{userId}/games?accessFilter=Public&limit=50&sortOrder=Asc',
  groupGames: 'https://games.roblox.com/v2/groups/{groupId}/games?accessFilter=Public&limit=50&sortOrder=Asc',
  groups: 'https://groups.roblox.com/v1/users/{userId}/groups/roles',
  game: 'https://games.roblox.com/v1/games?universeIds={universeId}',
  publish: 'https://apis.roblox.com/universes/v1/{universeId}/places/{placeId}/versions?versionType=Published',
  message: 'https://apis.roblox.com/cloud/v2/universes/{universeId}:publishMessage',
};

export function config() {
  const file = resolve(root, 'vault/whistlegraph/roblox.env');
  const values = {};
  if (existsSync(file)) for (const line of readFileSync(file, 'utf8').split('\n')) {
    const match = line.match(/^([A-Z_]+)=(.*)$/);
    if (match) values[match[1]] = match[2].trim().replace(/^(['"])(.*)\1$/, '$2');
  }
  return {
    key: process.env.ROBLOX_API_KEY || values.ROBLOX_API_KEY,
    userId: process.env.ROBLOX_USER_ID || values.ROBLOX_USER_ID || '1366409382',
  };
}

export function id(value) {
  if (!/^[1-9]\d*$/.test(String(value || ''))) throw new Error('Expected a positive Roblox ID.');
  return String(value);
}

export async function request(url, options = {}, fetcher = fetch) {
  const response = await fetcher(url, { ...options, redirect: 'error', signal: AbortSignal.timeout(30000) });
  // Do not echo remote error bodies: authenticated responses may include private details.
  if (!response.ok) throw new Error(`Roblox HTTP ${response.status}${response.status === 429 ? '; retry later' : ''}`);
  return response.status === 204 ? {} : response.json();
}

function url(name, values) {
  return endpoints[name].replace(/\{(\w+)\}/g, (_, key) => encodeURIComponent(values[key]));
}

export function publishPlan(file, universeId, placeId) {
  const extension = extname(file).toLowerCase();
  if (!['.rbxl', '.rbxlx'].includes(extension)) throw new Error('Publish requires a .rbxl or .rbxlx place file.');
  const body = readFileSync(resolve(file));
  if (!body.length) throw new Error('Place file is empty.');
  return {
    url: url('publish', { universeId: id(universeId), placeId: id(placeId) }),
    contentType: extension === '.rbxlx' ? 'application/xml' : 'application/octet-stream',
    bytes: body.length,
    sha256: createHash('sha256').update(body).digest('hex'),
    body,
  };
}

const help = `robloxplorer — Aesthetic Network on Roblox

  account                         Public account + local credential status
  search <creator-name>           Search public creators
  user <username-or-id>           Resolve a public profile
  games [user-id] [cursor]        Public games (defaults to Whistlegraph)
  groups [user-id]                Public group memberships
  group-games <group-id> [cursor] Public group games
  game <universe-id>              Experience details and public metrics
  endpoints                       Supported API endpoint map
  campaign                        Aesthetic Network campaign manifest
  publish <file> <universe-id> <place-id> [--live]

Publishing defaults to a local upload plan. --live publishes a new version.
Credentials: ROBLOX_API_KEY or vault/whistlegraph/roblox.env.
The API key must grant universe-places Write for the selected experience.
List responses retain nextPageCursor; pass it to retrieve another page.
`;

export async function main(args = process.argv.slice(2)) {
  const [command = 'help', ...rest] = args;
  const cfg = config();
  if (command === 'help' || command === '--help') return help;
  if (command === 'endpoints') return endpoints;
  if (command === 'campaign') return JSON.parse(readFileSync(new URL('./campaign.json', import.meta.url), 'utf8'));
  if (command === 'account') return {
    profile: await request(url('user', { userId: id(cfg.userId) })),
    credentialConfigured: Boolean(cfg.key),
    authenticationVerified: false,
  };
  if (command === 'user') {
    if (!rest[0]) throw new Error('Provide a username or user ID.');
    if (/^\d+$/.test(rest[0])) return request(url('user', { userId: id(rest[0]) }));
    return request('https://users.roblox.com/v1/usernames/users', {
      method: 'POST', headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ usernames: [rest[0]], excludeBannedUsers: false }),
    });
  }
  if (command === 'search') {
    if (!rest[0]?.trim()) throw new Error('Provide a creator search term.');
    return request(url('search', { keyword: rest.join(' ') }));
  }
  if (['games', 'groups', 'group-games', 'game'].includes(command)) {
    const name = command === 'group-games' ? 'groupGames' : command;
    const target = id(rest[0] || (['games', 'groups'].includes(command) ? cfg.userId : undefined));
    const address = new URL(url(name, { userId: target, groupId: target, universeId: target }));
    if (rest[1] && ['games', 'group-games'].includes(command)) address.searchParams.set('cursor', rest[1]);
    return request(address);
  }
  if (command === 'publish') {
    if (rest.length < 3 || rest.length > 4 || (rest[3] && rest[3] !== '--live')) {
      throw new Error('Usage: publish <file> <universe-id> <place-id> [--live]');
    }
    const { body, ...plan } = publishPlan(...rest);
    if (rest[3] !== '--live') return { live: false, ...plan };
    if (!cfg.key) throw new Error('Missing ROBLOX_API_KEY; configure vault/whistlegraph/roblox.env.');
    // The scoped key is the authority for this target; public username lookup is not authentication.
    const result = await request(plan.url, {
      method: 'POST', headers: { 'x-api-key': cfg.key, 'Content-Type': plan.contentType }, body,
    });
    return { live: true, ...plan, result };
  }
  throw new Error(`Unknown command: ${command}. Run with --help.`);
}

if (process.argv[1] && import.meta.url === pathToFileURL(resolve(process.argv[1])).href) {
  main().then(result => console.log(typeof result === 'string' ? result : JSON.stringify(result, null, 2)))
    .catch(error => { console.error(error.message); process.exitCode = 1; });
}
