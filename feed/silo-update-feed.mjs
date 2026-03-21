#!/usr/bin/env node

/**
 * Silo-side feed updater — manages all channels and dynamic playlists.
 * Runs on silo.aesthetic.computer via systemd timer.
 *
 * Channels: KidLisp, Paintings, Mugs, Clocks, Moods, Chats, Instruments, Tapes
 *
 * Usage:
 *   node silo-update-feed.mjs                # Update all channels
 *   node silo-update-feed.mjs kidlisp        # Update one channel
 *   node silo-update-feed.mjs paintings clocks  # Update specific channels
 */

import { MongoClient } from 'mongodb';

const FEED_URL = process.env.FEED_URL || 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET;
const MONGO_URI = process.env.MONGODB_CONNECTION_STRING;
const MONGO_DB = process.env.MONGODB_NAME || 'aesthetic';

if (!API_SECRET) { console.error('FEED_API_SECRET required'); process.exit(1); }
if (!MONGO_URI) { console.error('MONGODB_CONNECTION_STRING required'); process.exit(1); }

const log = (msg) => console.log(`${new Date().toISOString()} ${msg}`);

// ---------------------------------------------------------------------------
// Channel definitions
// ---------------------------------------------------------------------------

const dateStr = () => new Date().toLocaleDateString('en-US', {
  weekday: 'long', year: 'numeric', month: 'long', day: 'numeric',
});

const CHANNELS = {
  kidlisp: {
    title: 'KidLisp',
    curator: 'prompt.ac',
    summary: 'KidLisp is a TV friendly programming language for kids of all ages! Developed by @jeffrey of Aesthetic Computer.',
    dynamicPlaylists: [
      { slug: 'top-100', title: () => `Top 100 as of ${dateStr()}`, summary: 'The 100 most popular KidLisp pieces by total hits', handle: null, limit: 100 },
      { slug: 'top-jeffrey', title: () => `Top @jeffrey as of ${dateStr()}`, summary: 'The most popular KidLisp pieces by @jeffrey', handle: 'jeffrey', limit: 100 },
      { slug: 'top-fifi', title: () => `Top @fifi as of ${dateStr()}`, summary: 'The most popular KidLisp pieces by @fifi', handle: 'fifi', limit: 100 },
    ],
    // Colors playlist is static — managed separately, preserved by ID
    query: async (db, handle, limit) => queryCollection(db, 'kidlisp', handle, limit, { hits: -1 }),
    buildItem: (doc, i, total) => ({
      title: `$${doc.code}`,
      source: `https://device.kidlisp.com/$${doc.code}?playlist=true&duration=24&index=${i}&total=${total}`,
      duration: 24,
      license: 'open',
      provenance: { type: 'offChainURI', uri: `https://kidlisp.com/$${doc.code}` },
    }),
  },

  paintings: {
    title: 'Paintings',
    curator: 'prompt.ac',
    summary: 'User-created art from Aesthetic Computer drawing tools.',
    dynamicPlaylists: [
      { slug: 'recent', title: () => `Recent Paintings as of ${dateStr()}`, summary: 'The latest paintings from Aesthetic Computer', handle: null, limit: 100 },
      { slug: 'top-jeffrey', title: () => `Top @jeffrey Paintings`, summary: 'Paintings by @jeffrey', handle: 'jeffrey', limit: 100 },
      { slug: 'top-fifi', title: () => `Top @fifi Paintings`, summary: 'Paintings by @fifi', handle: 'fifi', limit: 100 },
    ],
    query: async (db, handle, limit) => queryCollection(db, 'paintings', handle, limit, { when: -1 }, { nuked: { $ne: true }, user: { $exists: true } }),
    buildItem: (doc, i, total) => ({
      title: `#${doc.code}`,
      source: `https://aesthetic.computer/painting~${doc.code}`,
      duration: 10,
      license: 'open',
      provenance: { type: 'offChainURI', uri: `https://aesthetic.computer/painting~${doc.code}` },
    }),
  },

  mugs: {
    title: 'Mugs',
    curator: 'prompt.ac',
    summary: 'Print-on-demand ceramic mugs made from paintings and KidLisp art.',
    dynamicPlaylists: [
      { slug: 'recent', title: () => `Recent Mugs as of ${dateStr()}`, summary: 'The latest mugs from Aesthetic Computer', handle: null, limit: 50 },
    ],
    query: async (db, handle, limit) => {
      const pipeline = [];
      pipeline.push({ $sort: { createdAt: -1 } });
      pipeline.push({ $limit: limit });
      pipeline.push({ $project: { _id: 0, code: 1, preview: 1, variant: 1, source: 1, createdAt: 1 } });
      return db.collection('products').aggregate(pipeline).toArray();
    },
    buildItem: (doc, i, total) => ({
      title: `+${doc.code} (${doc.variant || 'white'})`,
      source: `https://aesthetic.computer/mug~+${doc.code}`,
      duration: 12,
      license: 'open',
    }),
  },

  clocks: {
    title: 'Clocks',
    curator: 'prompt.ac',
    summary: 'User-created musical clocks — time displays with melody.',
    dynamicPlaylists: [
      { slug: 'top', title: () => `Top Clocks as of ${dateStr()}`, summary: 'The most popular clocks by total hits', handle: null, limit: 100 },
      { slug: 'recent', title: () => `Recent Clocks as of ${dateStr()}`, summary: 'Newly created clock melodies', handle: null, limit: 100, sort: { when: -1 } },
    ],
    query: async (db, handle, limit, extraSort) => {
      const pipeline = [];
      pipeline.push({ $match: { source: { $exists: true, $ne: '' } } });
      pipeline.push({ $sort: extraSort || { hits: -1 } });
      pipeline.push({ $limit: limit });
      pipeline.push({ $project: { _id: 0, code: 1, source: 1, hits: 1, when: 1 } });
      return db.collection('clocks').aggregate(pipeline).toArray();
    },
    buildItem: (doc, i, total) => ({
      title: `*${doc.code}`,
      source: `https://aesthetic.computer/clock~*${doc.code}`,
      duration: 30,
      license: 'open',
    }),
  },

  moods: {
    title: 'Moods',
    curator: 'prompt.ac',
    summary: 'Text status updates from Aesthetic Computer users.',
    dynamicPlaylists: [
      { slug: 'recent', title: () => `Recent Moods as of ${dateStr()}`, summary: 'The latest moods from the community', handle: null, limit: 100 },
      { slug: 'jeffrey', title: () => `@jeffrey Moods`, summary: 'Thoughts from @jeffrey', handle: 'jeffrey', limit: 50 },
    ],
    query: async (db, handle, limit) => queryCollection(db, 'moods', handle, limit, { when: -1 }, { deleted: { $ne: true }, mood: { $exists: true, $ne: '' } }),
    buildItem: (doc, i, total) => ({
      title: doc.mood?.substring(0, 60) || '...',
      source: `https://aesthetic.computer/mood`,
      duration: 8,
      license: 'open',
    }),
  },

  chats: {
    title: 'Chats',
    curator: 'prompt.ac',
    summary: 'Live chat rooms — community conversation in real-time.',
    dynamicPlaylists: [], // No dynamic playlists — all static
    staticItems: [
      { title: 'chat', source: 'https://aesthetic.computer/chat', duration: 60, license: 'open' },
      { title: 'laer-klokken', source: 'https://aesthetic.computer/laer-klokken', duration: 60, license: 'open' },
    ],
  },

  instruments: {
    title: 'Instruments',
    curator: 'prompt.ac',
    summary: 'Curated sequences using parameterized pieces for ambient display.',
    dynamicPlaylists: [], // Chords playlist is static — migrated from KidLisp channel
    // Chords playlist ID: ea5e26c9-e755-4f88-807e-c68a91cff49a (seeded via feed-state.json)
  },

  tapes: {
    title: 'Tapes',
    curator: 'prompt.ac',
    summary: 'Session recordings and replays from Aesthetic Computer.',
    dynamicPlaylists: [
      { slug: 'recent', title: () => `Recent Tapes as of ${dateStr()}`, summary: 'The latest session recordings', handle: null, limit: 50 },
    ],
    query: async (db, handle, limit) => queryCollection(db, 'tapes', handle, limit, { when: -1 }, { nuked: { $ne: true }, code: { $exists: true } }),
    buildItem: (doc, i, total) => ({
      title: `!${doc.code}`,
      source: `https://aesthetic.computer/replay~${doc.slug || doc.code}`,
      duration: 30,
      license: 'open',
    }),
  },
};

// ---------------------------------------------------------------------------
// Shared query helper
// ---------------------------------------------------------------------------

async function queryCollection(db, collection, handle, limit, sort, baseMatch = {}) {
  const pipeline = [];

  const match = { ...baseMatch };
  if (handle) {
    const handleDoc = await db.collection('@handles').findOne({ handle });
    if (!handleDoc) {
      log(`  warning: handle @${handle} not found`);
      return [];
    }
    match.user = handleDoc._id;
  }
  if (Object.keys(match).length > 0) pipeline.push({ $match: match });

  pipeline.push(
    { $lookup: { from: '@handles', localField: 'user', foreignField: '_id', as: '_h' } },
    { $sort: sort },
    { $limit: limit },
  );

  return db.collection(collection).aggregate(pipeline).toArray();
}

// ---------------------------------------------------------------------------
// Feed API helpers
// ---------------------------------------------------------------------------

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

// ---------------------------------------------------------------------------
// Channel state store (JSON file on disk to track channel/playlist IDs)
// ---------------------------------------------------------------------------

import { readFileSync, writeFileSync, existsSync } from 'fs';

const STATE_FILE = new URL('./feed-state.json', import.meta.url).pathname;

function loadState() {
  if (existsSync(STATE_FILE)) {
    return JSON.parse(readFileSync(STATE_FILE, 'utf8'));
  }
  return { channels: {} };
}

function saveState(state) {
  writeFileSync(STATE_FILE, JSON.stringify(state, null, 2));
}

// ---------------------------------------------------------------------------
// Update one channel
// ---------------------------------------------------------------------------

async function updateChannel(slug, db) {
  const config = CHANNELS[slug];
  if (!config) { log(`unknown channel: ${slug}`); return; }

  log(`--- ${slug} ---`);

  const state = loadState();
  let channelId = state.channels[slug]?.id;
  let staticPlaylistUrls = state.channels[slug]?.staticPlaylistUrls || [];

  // Create channel if it doesn't exist
  if (channelId) {
    const existing = await api('GET', `/channels/${channelId}`);
    if (!existing) channelId = null;
    else staticPlaylistUrls = state.channels[slug]?.staticPlaylistUrls || [];
  }

  // Handle static-only channels (chats, instruments)
  if (config.dynamicPlaylists.length === 0) {
    if (config.staticItems && staticPlaylistUrls.length === 0) {
      const playlist = {
        dpVersion: '1.1.0',
        title: config.title,
        summary: config.summary,
        items: config.staticItems,
        defaults: { display: { scaling: 'fit', background: '#000000' }, license: 'open', duration: 60 },
      };
      const created = await api('POST', '/playlists', playlist);
      staticPlaylistUrls = [`${FEED_URL}/playlists/${created.id}`];
      log(`  created static playlist: ${created.id} (${config.staticItems.length} items)`);
    }

    if (staticPlaylistUrls.length === 0) {
      log(`  ${slug}: no playlists yet, skipping channel creation`);
      return;
    }

    if (!channelId) {
      const created = await api('POST', '/channels', {
        title: config.title, curator: config.curator, summary: config.summary,
        playlists: staticPlaylistUrls,
      });
      channelId = created.id;
      log(`  created channel: ${channelId}`);
    } else {
      await api('PUT', `/channels/${channelId}`, {
        title: config.title, curator: config.curator, summary: config.summary,
        playlists: staticPlaylistUrls,
      });
    }

    state.channels[slug] = { id: channelId, staticPlaylistUrls };
    saveState(state);
    log(`  ${slug}: done (static)`);
    return;
  }

  // Track old dynamic playlist IDs for cleanup
  const oldDynamicUrls = state.channels[slug]?.dynamicPlaylistUrls || [];
  const newDynamicUrls = [];

  for (const plConfig of config.dynamicPlaylists) {
    const sortOverride = plConfig.sort;
    let docs;

    if (config.query) {
      docs = await config.query(db, plConfig.handle, plConfig.limit, sortOverride);
    } else {
      docs = [];
    }

    log(`  ${plConfig.slug}: ${docs.length} items`);

    if (docs.length === 0) {
      const idx = config.dynamicPlaylists.indexOf(plConfig);
      if (oldDynamicUrls[idx]) newDynamicUrls.push(oldDynamicUrls[idx]);
      continue;
    }

    const playlist = {
      dpVersion: '1.1.0',
      title: plConfig.title(),
      summary: plConfig.summary,
      items: docs.map((d, i) => config.buildItem(d, i, docs.length)),
      defaults: { display: { scaling: 'fit', background: '#000000' }, license: 'open', duration: 24 },
    };

    const created = await api('POST', '/playlists', playlist);
    newDynamicUrls.push(`${FEED_URL}/playlists/${created.id}`);
    log(`  ${plConfig.slug}: created ${created.id}`);
  }

  // Build final playlist URL list: [dynamic..., static...]
  const allUrls = [...newDynamicUrls, ...staticPlaylistUrls];

  if (!channelId) {
    const created = await api('POST', '/channels', {
      title: config.title, curator: config.curator, summary: config.summary,
      playlists: allUrls,
    });
    channelId = created.id;
    log(`  created channel: ${channelId} (${allUrls.length} playlists)`);
  } else {
    await api('PUT', `/channels/${channelId}`, {
      title: config.title, curator: config.curator, summary: config.summary,
      playlists: allUrls,
    });
    log(`  channel updated (${allUrls.length} playlists)`);
  }

  // Delete old dynamic playlists
  for (const oldUrl of oldDynamicUrls) {
    const oldId = oldUrl.split('/').pop();
    if (!newDynamicUrls.some(u => u.includes(oldId))) {
      await api('DELETE', `/playlists/${oldId}`);
      log(`  deleted old: ${oldId}`);
    }
  }

  // Save state
  state.channels[slug] = { id: channelId, dynamicPlaylistUrls: newDynamicUrls, staticPlaylistUrls };
  saveState(state);
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

async function main() {
  const requestedSlugs = process.argv.slice(2);
  const slugs = requestedSlugs.length > 0
    ? requestedSlugs
    : Object.keys(CHANNELS);

  log(`updating channels: ${slugs.join(', ')}`);

  const client = new MongoClient(MONGO_URI);
  await client.connect();
  const db = client.db(MONGO_DB);

  for (const slug of slugs) {
    try {
      await updateChannel(slug, db);
    } catch (e) {
      log(`  ERROR in ${slug}: ${e.message}`);
    }
  }

  await client.close();
  log('all done.');
}

main().catch(e => {
  console.error('FATAL:', e.message);
  process.exit(1);
});
