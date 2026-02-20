#!/usr/bin/env node

/**
 * Silo-side KidLisp playlist updater
 * Runs on silo.aesthetic.computer via systemd timer
 *
 * Regenerates dynamic playlists daily:
 *   - Top 100 (all authors)
 *   - Top @jeffrey
 *   - Top @fifi
 *
 * Static playlists (Colors, Chords) are left untouched.
 */

import { MongoClient } from 'mongodb';

const FEED_URL = process.env.FEED_URL || 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET;
const MONGO_URI = process.env.MONGODB_CONNECTION_STRING;
const MONGO_DB = process.env.MONGODB_NAME || 'aesthetic';
const CHANNEL_ID = process.env.KIDLISP_CHANNEL_ID || '156c4235-4b24-4001-bec9-61ce0ac7c25e';

// Dynamic playlists that get regenerated (order matters — matches channel slot order)
// Channel playlist order: [Top 100, Top @jeffrey, Top @fifi, Colors, Chords]
const DYNAMIC_PLAYLISTS = [
  { slug: 'top-100', title: (d) => `Top 100 as of ${d}`, summary: 'The 100 most popular KidLisp pieces by total hits', handle: null, limit: 100 },
  { slug: 'top-jeffrey', title: (d) => `Top @jeffrey as of ${d}`, summary: 'The most popular KidLisp pieces by @jeffrey', handle: 'jeffrey', limit: 100 },
  { slug: 'top-fifi', title: (d) => `Top @fifi as of ${d}`, summary: 'The most popular KidLisp pieces by @fifi', handle: 'fifi', limit: 100 },
];

if (!API_SECRET) { console.error('FEED_API_SECRET required'); process.exit(1); }
if (!MONGO_URI) { console.error('MONGODB_CONNECTION_STRING required'); process.exit(1); }

const log = (msg) => console.log(`${new Date().toISOString()} ${msg}`);

let mongoClient;

async function getDb() {
  if (!mongoClient) {
    mongoClient = new MongoClient(MONGO_URI);
    await mongoClient.connect();
  }
  return mongoClient.db(MONGO_DB);
}

async function getTopPieces(handle, limit) {
  const db = await getDb();

  const pipeline = [];

  if (handle) {
    // Look up the user ID for this handle
    const handleDoc = await db.collection('@handles').findOne({ handle });
    if (!handleDoc) {
      log(`  warning: handle @${handle} not found`);
      return [];
    }
    pipeline.push({ $match: { user: handleDoc._id } });
  }

  pipeline.push(
    { $lookup: { from: '@handles', localField: 'user', foreignField: '_id', as: 'handleInfo' } },
    { $sort: { hits: -1 } },
    { $limit: limit },
    { $project: { _id: 0, code: 1, hits: 1, handle: { $cond: { if: { $gt: [{ $size: '$handleInfo' }, 0] }, then: { $arrayElemAt: ['$handleInfo.handle', 0] }, else: null } } } },
  );

  return db.collection('kidlisp').aggregate(pipeline).toArray();
}

function buildPlaylist(config, pieces) {
  const date = new Date().toLocaleDateString('en-US', { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' });
  const duration = 24;

  return {
    dpVersion: '1.1.0',
    title: config.title(date),
    summary: config.summary,
    items: pieces.map((p, i) => ({
      title: `$${p.code}`,
      source: `https://device.kidlisp.com/$${p.code}?playlist=true&duration=${duration}&index=${i}&total=${pieces.length}`,
      duration,
      license: 'open',
      provenance: { type: 'offChainURI', uri: `https://kidlisp.com/$${p.code}` },
    })),
    defaults: { display: { scaling: 'fit', background: '#000000', margin: '0%' }, license: 'open', duration: 24 },
  };
}

async function api(method, path, body) {
  const opts = {
    method,
    headers: { 'Authorization': `Bearer ${API_SECRET}`, 'Content-Type': 'application/json' },
  };
  if (body) opts.body = JSON.stringify(body);
  const res = await fetch(`${FEED_URL}${path}`, opts);
  if (!res.ok && res.status !== 404) {
    throw new Error(`${method} ${path}: ${res.status} ${await res.text()}`);
  }
  if (res.status === 204 || res.status === 404) return null;
  return res.json();
}

async function main() {
  log('updating kidlisp playlists...');

  // Get current channel
  const channel = await api('GET', `/channels/${CHANNEL_ID}`);
  const currentPlaylists = channel.playlists || [];

  // Static playlists are at the end (Colors, Chords) — everything after the dynamic slots
  const dynamicCount = DYNAMIC_PLAYLISTS.length;
  const oldDynamicUrls = currentPlaylists.slice(0, dynamicCount);
  const staticUrls = currentPlaylists.slice(dynamicCount);

  // Generate each dynamic playlist
  const newDynamicUrls = [];
  const oldIdsToDelete = [];

  for (const config of DYNAMIC_PLAYLISTS) {
    const pieces = await getTopPieces(config.handle, config.limit);
    log(`  ${config.slug}: ${pieces.length} pieces`);

    if (pieces.length === 0) {
      log(`  skipping ${config.slug} (no pieces)`);
      // Keep old URL if it exists
      const idx = DYNAMIC_PLAYLISTS.indexOf(config);
      if (oldDynamicUrls[idx]) newDynamicUrls.push(oldDynamicUrls[idx]);
      continue;
    }

    const playlist = buildPlaylist(config, pieces);
    const created = await api('POST', '/playlists', playlist);
    newDynamicUrls.push(`${FEED_URL}/playlists/${created.id}`);
    log(`  ${config.slug}: created ${created.id}`);

    // Track old playlist for deletion
    const idx = DYNAMIC_PLAYLISTS.indexOf(config);
    if (oldDynamicUrls[idx]) {
      const oldId = oldDynamicUrls[idx].split('/').pop();
      if (oldId !== created.id) oldIdsToDelete.push(oldId);
    }
  }

  // Update channel: [dynamic playlists..., static playlists...]
  const allPlaylistUrls = [...newDynamicUrls, ...staticUrls];
  await api('PUT', `/channels/${CHANNEL_ID}`, {
    title: channel.title,
    curator: channel.curator,
    summary: channel.summary,
    playlists: allPlaylistUrls,
  });
  log(`  channel updated (${allPlaylistUrls.length} playlists)`);

  // Delete old dynamic playlists
  for (const oldId of oldIdsToDelete) {
    await api('DELETE', `/playlists/${oldId}`);
    log(`  deleted old: ${oldId}`);
  }

  // Clean up orphaned channels
  const allChannels = await api('GET', '/channels');
  for (const ch of (allChannels.items || [])) {
    if (ch.id !== CHANNEL_ID) {
      await api('DELETE', `/channels/${ch.id}`);
    }
  }

  if (mongoClient) await mongoClient.close();
  log('done.');
}

main().catch(e => {
  console.error('FAILED:', e.message);
  if (mongoClient) mongoClient.close();
  process.exit(1);
});
