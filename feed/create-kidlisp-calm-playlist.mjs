#!/usr/bin/env node

/**
 * Create KidLisp Calm Playlist
 *
 * Generates a DP-1 playlist of hand-curated calming KidLisp pieces.
 *
 * Usage: FEED_API_SECRET=... node create-kidlisp-calm-playlist.mjs
 */

const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET || 'YOUR_FEED_API_SECRET_HERE';
const CHANNEL_ID = '156c4235-4b24-4001-bec9-61ce0ac7c25e';

// Hand-curated calm pieces (add more as desired)
const calmPieces = [
  'bop',
  'xom',
];

const duration = 60; // seconds per piece

function generateCalmPlaylist() {
  return {
    dpVersion: '1.1.0',
    title: 'Calm',
    summary: 'A hand-curated selection of calming KidLisp pieces, perfect for ambient display.',
    items: calmPieces.map((code, index) => ({
      title: code,
      source: `https://device.kidlisp.com/$${code}?playlist=true&duration=${duration}&index=${index}&total=${calmPieces.length}`,
      duration,
      license: 'open',
      provenance: {
        type: 'offChainURI',
        uri: `https://kidlisp.com/${code}`,
      },
    })),
    defaults: {
      display: {
        scaling: 'fit',
        background: '#000000',
        margin: '0%',
      },
      license: 'open',
      duration,
    },
  };
}

async function uploadPlaylist(playlist) {
  const response = await fetch(`${FEED_API_URL}/playlists`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${API_SECRET}`,
    },
    body: JSON.stringify(playlist),
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Failed to upload playlist: ${response.status} ${error}`);
  }

  return await response.json();
}

async function addPlaylistToChannel(playlistUrl) {
  // Fetch current channel
  const resp = await fetch(`${FEED_API_URL}/channels/${CHANNEL_ID}`);
  if (!resp.ok) throw new Error(`Failed to fetch channel: ${resp.status}`);
  const channel = await resp.json();

  // Add new playlist URL
  const playlists = [...(channel.playlists || []), playlistUrl];

  // Update channel
  const updateResp = await fetch(`${FEED_API_URL}/channels/${CHANNEL_ID}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${API_SECRET}`,
    },
    body: JSON.stringify({ ...channel, playlists }),
  });

  if (!updateResp.ok) {
    const error = await updateResp.text();
    throw new Error(`Failed to update channel: ${updateResp.status} ${error}`);
  }

  return await updateResp.json();
}

async function main() {
  try {
    console.log('🧘 Creating KidLisp Calm playlist...\n');

    const playlist = generateCalmPlaylist();
    console.log(`📝 Generated playlist with ${playlist.items.length} pieces: ${calmPieces.join(', ')}`);

    console.log('📤 Uploading playlist to feed.aesthetic.computer...');
    const result = await uploadPlaylist(playlist);
    const playlistId = result.id;
    const playlistUrl = `${FEED_API_URL}/playlists/${playlistId}`;
    console.log(`✅ Playlist created: ${playlistUrl}\n`);

    console.log('🔗 Adding playlist to KidLisp channel...');
    await addPlaylistToChannel(playlistUrl);
    console.log('✅ Channel updated!\n');

    console.log('📋 Summary:');
    console.log(`   Playlist ID: ${playlistId}`);
    console.log(`   Playlist URL: ${playlistUrl}`);
    console.log(`   Pieces: ${calmPieces.join(', ')}`);
    console.log(`   Duration: ${duration}s per piece`);
  } catch (err) {
    console.error('❌ Error:', err.message);
    process.exit(1);
  }
}

main();
