#!/usr/bin/env node

/**
 * Update existing KidLisp playlists with provenance blocks
 * Uses PUT to update existing playlists by ID instead of creating new ones
 * 
 * Usage: node update-existing-playlists.mjs <API_SECRET>
 */

import { connect } from '../system/backend/database.mjs';

const API_SECRET = process.argv[2] || process.env.FEED_API_SECRET;
const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';

// Known playlist IDs
const PLAYLISTS = {
  colors: '2680b102-04ee-47b5-b7d7-f814094695e7',
  chords: 'e1bf1aae-2427-4dd0-a39d-f5da89fdf02e'
};

if (!API_SECRET || API_SECRET === 'YOUR_FEED_API_SECRET_HERE') {
  console.error('❌ Error: API_SECRET required');
  console.error('Usage: node update-existing-playlists.mjs <API_SECRET>');
  console.error('   or: FEED_API_SECRET=<secret> node update-existing-playlists.mjs');
  process.exit(1);
}

/**
 * Fetch existing playlist
 */
async function getPlaylist(id) {
  const response = await fetch(`${FEED_API_URL}/playlists/${id}`);
  if (!response.ok) {
    throw new Error(`Failed to fetch playlist: ${response.status}`);
  }
  return await response.json();
}

/**
 * Update playlist with PUT
 */
async function updatePlaylist(id, updates) {
  const response = await fetch(`${FEED_API_URL}/playlists/${id}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${API_SECRET}`,
    },
    body: JSON.stringify(updates),
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`HTTP ${response.status}: ${error}`);
  }

  return await response.json();
}

/**
 * Add provenance blocks to playlist items
 */
function addProvenanceToItems(items, getUri) {
  return items.map(item => ({
    ...item,
    provenance: {
      type: 'offChainURI',
      contract: {
        chain: 'other',
        uri: getUri(item),
      }
    }
  }));
}

/**
 * Update Colors playlist
 */
async function updateColorsPlaylist() {
  console.log('\n🎨 Updating Colors playlist...');
  
  const playlist = await getPlaylist(PLAYLISTS.colors);
  console.log(`   Current title: "${playlist.title}"`);
  console.log(`   Items: ${playlist.items.length}`);
  
  // Add provenance to each item
  const updatedItems = addProvenanceToItems(playlist.items, item => {
    // Extract color name from source URL
    const match = item.source.match(/aesthetic\.computer\/([^?]+)/);
    const colorName = match ? match[1] : 'unknown';
    return `https://aesthetic.computer/${colorName}`;
  });
  
  // Remove signature field for update
  const { signature, ...playlistWithoutSignature } = playlist;
  
  const result = await updatePlaylist(PLAYLISTS.colors, {
    ...playlistWithoutSignature,
    items: updatedItems
  });
  
  console.log(`   ✅ Updated successfully!`);
  return result;
}

/**
 * Update Chords playlist
 */
async function updateChordsPlaylist() {
  console.log('\n🎵 Updating Chords playlist...');
  
  const playlist = await getPlaylist(PLAYLISTS.chords);
  console.log(`   Current title: "${playlist.title}"`);
  console.log(`   Items: ${playlist.items.length}`);
  
  // Add provenance to each item
  const updatedItems = addProvenanceToItems(playlist.items, item => {
    // Extract chord pattern from source URL
    const match = item.source.match(/aesthetic\.computer\/([^?]+)/);
    const pattern = match ? match[1] : 'unknown';
    return `https://aesthetic.computer/${pattern}`;
  });
  
  // Remove signature field for update
  const { signature, ...playlistWithoutSignature } = playlist;
  
  const result = await updatePlaylist(PLAYLISTS.chords, {
    ...playlistWithoutSignature,
    items: updatedItems
  });
  
  console.log(`   ✅ Updated successfully!`);
  return result;
}

/**
 * Update Top 100 playlist
 */
async function updateTop100Playlist() {
  console.log('\n📊 Finding and updating Top 100 playlist...');
  
  // Find it by searching
  const listResponse = await fetch(`${FEED_API_URL}/playlists`);
  if (!listResponse.ok) {
    throw new Error('Failed to list playlists');
  }
  
  const list = await listResponse.json();
  const top100 = list.items?.find(p => p.title?.includes('Top 100'));
  
  if (!top100) {
    console.log('   ⚠️  Top 100 playlist not found, skipping');
    return null;
  }
  
  console.log(`   Found: "${top100.title}" (${top100.id})`);
  console.log(`   Items: ${top100.items.length}`);
  
  // Add provenance to each item
  const updatedItems = addProvenanceToItems(top100.items, item => {
    // Extract code from title or source
    const codeMatch = item.title?.match(/\$(\w+)/) || item.source?.match(/\$(\w+)/);
    const code = codeMatch ? codeMatch[1] : 'unknown';
    return `https://aesthetic.computer/$${code}`;
  });
  
  // Remove signature field for update
  const { signature, ...playlistWithoutSignature } = top100;
  
  const result = await updatePlaylist(top100.id, {
    ...playlistWithoutSignature,
    items: updatedItems
  });
  
  console.log(`   ✅ Updated successfully!`);
  return result;
}

/**
 * Main execution
 */
async function main() {
  console.log('🔄 Updating KidLisp Playlists with Provenance Blocks\n');
  console.log('This will add provenance blocks to existing playlist items.\n');

  try {
    await updateColorsPlaylist();
    await updateChordsPlaylist();
    await updateTop100Playlist();
    
    console.log('\n🎉 All playlists updated successfully!');
    console.log('All playlists now include provenance blocks according to DP-1 spec.\n');
  } catch (error) {
    console.error('\n❌ Error:', error.message);
    process.exit(1);
  }
}

main();
