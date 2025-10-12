#!/usr/bin/env node

/**
 * Regenerate KidLisp Playlists with Provenance Blocks
 * 
 * This script:
 * 1. Deletes the existing KidLisp channel
 * 2. Deletes all existing KidLisp playlists
 * 3. Regenerates all playlists with proper provenance blocks
 * 4. Creates a new channel with all the playlists
 * 
 * Usage: FEED_API_SECRET=your_secret node regenerate-kidlisp-playlists.mjs
 */

const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET;

if (!API_SECRET || API_SECRET === 'YOUR_FEED_API_SECRET_HERE') {
  console.error('❌ Error: FEED_API_SECRET environment variable must be set');
  console.error('Usage: FEED_API_SECRET=your_secret node regenerate-kidlisp-playlists.mjs');
  process.exit(1);
}

/**
 * List all channels and find KidLisp channel
 */
async function findKidLispChannel() {
  try {
    const response = await fetch(`${FEED_API_URL}/channels`, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${API_SECRET}`,
      },
    });

    if (!response.ok) {
      throw new Error(`Failed to list channels: ${response.status}`);
    }

    const data = await response.json();
    const kidlispChannel = data.items?.find(ch => 
      ch.title?.toLowerCase().includes('kidlisp') || 
      ch.slug?.includes('kidlisp')
    );

    return kidlispChannel;
  } catch (error) {
    console.error('Error finding KidLisp channel:', error.message);
    return null;
  }
}

/**
 * List all playlists and find KidLisp-related ones
 */
async function findKidLispPlaylists() {
  try {
    const response = await fetch(`${FEED_API_URL}/playlists`, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${API_SECRET}`,
      },
    });

    if (!response.ok) {
      throw new Error(`Failed to list playlists: ${response.status}`);
    }

    const data = await response.json();
    const kidlispPlaylists = data.items?.filter(pl => 
      pl.title?.toLowerCase().includes('kidlisp') ||
      pl.title?.toLowerCase().includes('colors') ||
      pl.title?.toLowerCase().includes('chords') ||
      pl.title?.toLowerCase().includes('top 100') ||
      pl.slug?.includes('kidlisp')
    );

    return kidlispPlaylists || [];
  } catch (error) {
    console.error('Error finding KidLisp playlists:', error.message);
    return [];
  }
}

/**
 * Delete a channel by ID or slug
 */
async function deleteChannel(identifier) {
  try {
    const response = await fetch(`${FEED_API_URL}/channels/${identifier}`, {
      method: 'DELETE',
      headers: {
        'Authorization': `Bearer ${API_SECRET}`,
      },
    });

    if (!response.ok && response.status !== 404) {
      const error = await response.text();
      throw new Error(`Failed to delete channel: ${response.status} ${error}`);
    }

    return response.ok;
  } catch (error) {
    console.error(`Error deleting channel ${identifier}:`, error.message);
    return false;
  }
}

/**
 * Delete a playlist by ID or slug
 */
async function deletePlaylist(identifier) {
  try {
    const response = await fetch(`${FEED_API_URL}/playlists/${identifier}`, {
      method: 'DELETE',
      headers: {
        'Authorization': `Bearer ${API_SECRET}`,
      },
    });

    if (!response.ok && response.status !== 404) {
      const error = await response.text();
      throw new Error(`Failed to delete playlist: ${response.status} ${error}`);
    }

    return response.ok;
  } catch (error) {
    console.error(`Error deleting playlist ${identifier}:`, error.message);
    return false;
  }
}

/**
 * Run a playlist generation script
 */
async function runPlaylistScript(scriptPath) {
  const { spawn } = await import('child_process');
  
  return new Promise((resolve, reject) => {
    console.log(`  🔧 Running ${scriptPath}...`);
    
    const childProcess = spawn('node', [scriptPath], {
      env: { ...process.env, FEED_API_SECRET: API_SECRET },
      stdio: 'pipe'
    });

    let output = '';
    let errorOutput = '';

    childProcess.stdout.on('data', (data) => {
      output += data.toString();
    });

    childProcess.stderr.on('data', (data) => {
      errorOutput += data.toString();
    });

    childProcess.on('close', (code) => {
      if (code === 0) {
        console.log(`  ✅ ${scriptPath} completed successfully`);
        // Extract playlist ID from output
        const match = output.match(/Playlist uploaded successfully! ID: ([a-f0-9-]+)/i) ||
                     output.match(/ID: ([a-f0-9-]+)/);
        const playlistId = match ? match[1] : null;
        resolve({ success: true, playlistId, output });
      } else {
        console.error(`  ❌ ${scriptPath} failed with code ${code}`);
        if (errorOutput) console.error(`  Error: ${errorOutput}`);
        resolve({ success: false, error: errorOutput || output });
      }
    });

    childProcess.on('error', (err) => {
      console.error(`  ❌ Failed to run ${scriptPath}:`, err.message);
      reject(err);
    });
  });
}

/**
 * Create a new KidLisp channel with all playlists
 */
async function createKidLispChannel(playlistIds) {
  try {
    const channel = {
      title: 'KidLisp',
      curator: 'prompt.ac',
      summary: 'KidLisp is a TV friendly programming language for kids of all ages! Developed by @jeffrey of Aesthetic Computer.',
      playlists: playlistIds.map(id => `${FEED_API_URL}/playlists/${id}`)
    };

    const response = await fetch(`${FEED_API_URL}/channels`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${API_SECRET}`,
      },
      body: JSON.stringify(channel),
    });

    if (!response.ok) {
      const error = await response.text();
      throw new Error(`Failed to create channel: ${response.status} ${error}`);
    }

    return await response.json();
  } catch (error) {
    console.error('Error creating KidLisp channel:', error.message);
    throw error;
  }
}

/**
 * Main execution
 */
async function main() {
  console.log('🔄 KidLisp Playlist Regeneration\n');
  console.log('This will delete and recreate all KidLisp playlists with provenance blocks.\n');

  // Step 1: Find and delete existing channel
  console.log('📡 Step 1: Finding existing KidLisp channel...');
  const existingChannel = await findKidLispChannel();
  
  if (existingChannel) {
    console.log(`  Found channel: "${existingChannel.title}" (${existingChannel.id})`);
    console.log('  🗑️  Deleting channel...');
    const deleted = await deleteChannel(existingChannel.id);
    if (deleted) {
      console.log('  ✅ Channel deleted successfully');
    } else {
      console.log('  ⚠️  Channel may not have been deleted');
    }
  } else {
    console.log('  ℹ️  No existing KidLisp channel found');
  }

  // Step 2: Find and delete existing playlists
  console.log('\n📋 Step 2: Finding existing KidLisp playlists...');
  const existingPlaylists = await findKidLispPlaylists();
  
  if (existingPlaylists.length > 0) {
    console.log(`  Found ${existingPlaylists.length} playlists:`);
    for (const playlist of existingPlaylists) {
      console.log(`    - "${playlist.title}" (${playlist.id})`);
    }
    
    console.log('  🗑️  Deleting playlists...');
    for (const playlist of existingPlaylists) {
      const deleted = await deletePlaylist(playlist.id);
      if (deleted) {
        console.log(`    ✅ Deleted "${playlist.title}"`);
      } else {
        console.log(`    ⚠️  Failed to delete "${playlist.title}"`);
      }
    }
  } else {
    console.log('  ℹ️  No existing KidLisp playlists found');
  }

  // Step 3: Regenerate playlists with provenance blocks
  console.log('\n🎨 Step 3: Regenerating playlists with provenance blocks...');
  
  const playlistScripts = [
    'create-top-kidlisp-playlist.mjs',
    'create-kidlisp-colors-playlist.mjs',
    'create-kidlisp-chords-playlist.mjs'
  ];

  const generatedPlaylistIds = [];
  
  for (const script of playlistScripts) {
    const result = await runPlaylistScript(`./feed/${script}`);
    if (result.success && result.playlistId) {
      generatedPlaylistIds.push(result.playlistId);
    }
  }

  if (generatedPlaylistIds.length === 0) {
    console.error('\n❌ Failed to generate any playlists. Aborting.');
    process.exit(1);
  }

  console.log(`\n✅ Generated ${generatedPlaylistIds.length} playlists`);

  // Step 4: Create new channel with all playlists
  console.log('\n📺 Step 4: Creating new KidLisp channel...');
  try {
    const newChannel = await createKidLispChannel(generatedPlaylistIds);
    console.log('✅ Channel created successfully!');
    console.log(`   ID: ${newChannel.id}`);
    console.log(`   Slug: ${newChannel.slug}`);
    console.log(`   URL: ${FEED_API_URL}/channels/${newChannel.id}`);
  } catch (error) {
    console.error('❌ Failed to create channel:', error.message);
    console.log('\n⚠️  Playlists were created but channel creation failed.');
    console.log('   You may need to create the channel manually.');
    process.exit(1);
  }

  console.log('\n🎉 Regeneration complete!');
  console.log('All KidLisp playlists now include provenance blocks according to DP-1 spec.\n');
}

main().catch(error => {
  console.error('❌ Fatal error:', error);
  process.exit(1);
});
