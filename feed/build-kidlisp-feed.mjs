#!/usr/bin/env node

/**
 * Build and Publish KidLisp Feed
 * 
 * This script:
 * 1. Generates Top 100 and Colors playlists
 * 2. Updates the KidLisp channel with both playlists
 * 3. Cleans up old/stale playlists
 * 4. Shows the final feed structure
 * 
 * Usage: node build-kidlisp-feed.mjs
 */

import fetch from 'node-fetch';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET || 'YOUR_FEED_API_SECRET_HERE';
const KIDLISP_CHANNEL_ID = '23b63744-649f-4274-add5-d1b439984e51';

// Keep track of playlist IDs we want to preserve
let keepPlaylistIds = new Set();

/**
 * Run external script and capture output
 */
async function runScript(scriptPath, env = {}) {
  console.log(`\n▶️  Running ${scriptPath}...`);
  const { stdout, stderr } = await execAsync(`node ${scriptPath}`, {
    env: { ...process.env, ...env },
    cwd: process.cwd()
  });
  
  if (stderr && !stderr.includes('ExperimentalWarning')) {
    console.error('⚠️  Warnings:', stderr);
  }
  
  return stdout;
}

/**
 * Extract playlist ID from script output
 */
function extractPlaylistId(output) {
  const match = output.match(/Playlist uploaded successfully! ID: ([a-f0-9-]+)/i) ||
                output.match(/ID: ([a-f0-9-]+)/);
  return match ? match[1] : null;
}

/**
 * Get all playlists from feed
 */
async function getAllPlaylists() {
  const response = await fetch(`${FEED_API_URL}/playlists`, {
    headers: { 'Authorization': `Bearer ${API_SECRET}` }
  });
  
  if (!response.ok) {
    throw new Error(`Failed to fetch playlists: ${response.status}`);
  }
  
  const data = await response.json();
  return data.items || [];
}

/**
 * Get all channels from feed
 */
async function getAllChannels() {
  const response = await fetch(`${FEED_API_URL}/channels`, {
    headers: { 'Authorization': `Bearer ${API_SECRET}` }
  });
  
  if (!response.ok) {
    throw new Error(`Failed to fetch channels: ${response.status}`);
  }
  
  const data = await response.json();
  return data.items || [];
}

/**
 * Delete a playlist by ID
 */
async function deletePlaylist(id) {
  const response = await fetch(`${FEED_API_URL}/playlists/${id}`, {
    method: 'DELETE',
    headers: { 'Authorization': `Bearer ${API_SECRET}` }
  });
  
  return response.ok || response.status === 404; // 404 means already deleted
}

/**
 * Update channel with new playlists
 */
async function updateChannel(channelId, playlistIds) {
  // First get the current channel
  const getResponse = await fetch(`${FEED_API_URL}/channels/${channelId}`, {
    headers: { 'Authorization': `Bearer ${API_SECRET}` }
  });
  
  if (!getResponse.ok) {
    throw new Error(`Failed to fetch channel: ${getResponse.status}`);
  }
  
  const channel = await getResponse.json();
  
  // Update with new playlists
  const updateData = {
    title: channel.title,
    curator: channel.curator,
    summary: channel.summary,
    playlists: playlistIds.map(id => `${FEED_API_URL}/playlists/${id}`)
  };
  
  const updateResponse = await fetch(`${FEED_API_URL}/channels/${channelId}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${API_SECRET}`
    },
    body: JSON.stringify(updateData)
  });
  
  if (!updateResponse.ok) {
    const error = await updateResponse.text();
    throw new Error(`Failed to update channel: ${updateResponse.status} ${error}`);
  }
  
  return await updateResponse.json();
}

/**
 * Display feed structure
 */
async function showFeedStructure() {
  console.log('\n📊 FEED STRUCTURE\n');
  console.log('═'.repeat(60));
  
  const playlists = await getAllPlaylists();
  const channels = await getAllChannels();
  
  console.log(`\n📋 Playlists (${playlists.length}):`);
  playlists.forEach(p => {
    const itemCount = p.items?.length || 0;
    const kept = keepPlaylistIds.has(p.id) ? '✓' : ' ';
    console.log(`  [${kept}] ${p.title}`);
    console.log(`      ID: ${p.id}`);
    console.log(`      Items: ${itemCount}`);
  });
  
  console.log(`\n📺 Channels (${channels.length}):`);
  channels.forEach(c => {
    const playlistCount = c.playlists?.length || 0;
    const active = c.id === KIDLISP_CHANNEL_ID ? '🌟' : '  ';
    console.log(`${active} ${c.title} (${c.curator})`);
    console.log(`      ID: ${c.id}`);
    console.log(`      Playlists: ${playlistCount}`);
    if (playlistCount > 0) {
      c.playlists.forEach(pUrl => {
        const pId = pUrl.split('/').pop();
        const playlist = playlists.find(p => p.id === pId);
        if (playlist) {
          console.log(`        - ${playlist.title}`);
        }
      });
    }
  });
  
  console.log('\n' + '═'.repeat(60));
}

/**
 * Main build process
 */
async function main() {
  console.log('🎨 Building KidLisp Feed\n');
  console.log('═'.repeat(60));
  
  try {
    // Step 1: Generate Top 100 playlist
    console.log('\n📊 Step 1: Generate Top 100 Playlist');
    const top100Output = await runScript('create-top-kidlisp-playlist.mjs', {
      MONGODB_CONNECTION_STRING: process.env.MONGODB_URI || process.env.MONGODB_CONNECTION_STRING,
      MONGODB_NAME: process.env.MONGODB_DB || process.env.MONGODB_NAME || 'aesthetic',
      FEED_API_SECRET: API_SECRET
    });
    const top100Id = extractPlaylistId(top100Output);
    
    if (!top100Id) {
      throw new Error('Failed to extract Top 100 playlist ID');
    }
    
    keepPlaylistIds.add(top100Id);
    console.log(`✅ Top 100 playlist created: ${top100Id}`);
    
    // Step 2: Generate Colors playlist
    console.log('\n🎨 Step 2: Generate Colors Playlist');
    const colorsOutput = await runScript('create-kidlisp-colors-playlist.mjs', {
      FEED_API_SECRET: API_SECRET
    });
    const colorsId = extractPlaylistId(colorsOutput);
    
    if (!colorsId) {
      throw new Error('Failed to extract Colors playlist ID');
    }
    
    keepPlaylistIds.add(colorsId);
    console.log(`✅ Colors playlist created: ${colorsId}`);
    
    // Step 3: Generate Chords playlist
    console.log('\n🎵 Step 3: Generate Chords Playlist');
    const chordsOutput = await runScript('create-kidlisp-chords-playlist.mjs', {
      FEED_API_SECRET: API_SECRET
    });
    const chordsId = extractPlaylistId(chordsOutput);
    
    if (!chordsId) {
      throw new Error('Failed to extract Chords playlist ID');
    }
    
    keepPlaylistIds.add(chordsId);
    console.log(`✅ Chords playlist created: ${chordsId}`);
    
    // Step 4: Update KidLisp channel
    console.log('\n📺 Step 4: Update KidLisp Channel');
    await updateChannel(KIDLISP_CHANNEL_ID, [top100Id, colorsId, chordsId]);
    console.log(`✅ Channel updated with both playlists`);
    
    // Step 5: Clean up old playlists
    console.log('\n🗑️  Step 5: Clean Up Old Playlists');
    const allPlaylists = await getAllPlaylists();
    const stalePlaylists = allPlaylists.filter(p => !keepPlaylistIds.has(p.id));
    
    if (stalePlaylists.length === 0) {
      console.log('✅ No stale playlists to remove');
    } else {
      console.log(`Found ${stalePlaylists.length} stale playlists to remove:`);
      for (const playlist of stalePlaylists) {
        const success = await deletePlaylist(playlist.id);
        console.log(`  ${success ? '✓' : '✗'} ${playlist.title} (${playlist.id})`);
      }
    }
    
    // Step 6: Show final structure
    console.log('\n📋 Step 6: Final Feed Structure');
    await showFeedStructure();
    
    console.log('\n🎉 Build Complete!\n');
    console.log('🔗 Channel URL: https://feed.aesthetic.computer/api/v1/channels/' + KIDLISP_CHANNEL_ID);
    console.log('📋 Top 100: https://feed.aesthetic.computer/api/v1/playlists/' + top100Id);
    console.log('🎨 Colors: https://feed.aesthetic.computer/api/v1/playlists/' + colorsId);
    console.log('🎵 Chords: https://feed.aesthetic.computer/api/v1/playlists/' + chordsId);
    console.log('');
    
  } catch (error) {
    console.error('\n❌ Build failed:', error.message);
    process.exit(1);
  }
}

main();
