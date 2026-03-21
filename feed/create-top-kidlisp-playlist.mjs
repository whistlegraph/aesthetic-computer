#!/usr/bin/env node
// Create a DP-1 playlist of top 100 most hit KidLisp pieces
// Usage: node create-top-kidlisp-playlist.mjs

import { connect } from '../system/backend/database.mjs';

const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET || 'YOUR_FEED_API_SECRET_HERE';

async function getTop100KidLispPieces() {
  const { db, disconnect } = await connect();
  console.log('📊 Connected to MongoDB');
  
  const collection = db.collection('kidlisp');
  
  // Get top 100 by hits with handle information
  const pipeline = [
    {
      $lookup: {
        from: "@handles",
        localField: "user",
        foreignField: "_id",
        as: "handleInfo"
      }
    },
    {
      $sort: { hits: -1 }
    },
    {
      $limit: 100
    },
    {
      $project: {
        _id: 0,
        code: 1,
        source: 1,
        hits: 1,
        when: 1,
        user: 1,
        handle: { 
          $cond: {
            if: { $gt: [{ $size: "$handleInfo" }, 0] },
            then: { $arrayElemAt: ["$handleInfo.handle", 0] },
            else: null
          }
        }
      }
    }
  ];
  
  const pieces = await collection.aggregate(pipeline).toArray();
  
  console.log(`✅ Found ${pieces.length} top KidLisp pieces`);
  console.log(`🏆 Top 5:`);
  pieces.slice(0, 5).forEach((p, i) => {
    console.log(`   ${i + 1}. $${p.code} - ${p.hits} hits - @${p.handle || 'anon'}`);
  });
  
  return { pieces, disconnect };
}

function generateDP1Playlist(pieces) {
  const timestamp = new Date().toISOString();
  const dateFormatted = new Date().toLocaleDateString('en-US', { 
    weekday: 'long', 
    year: 'numeric', 
    month: 'long', 
    day: 'numeric' 
  });
  
  const duration = 24; // seconds per piece
  
  const items = pieces.map((piece, index) => ({
    title: `$${piece.code}`,
    // Include playlist params so device.kidlisp.com shows progress bar
    source: `https://device.kidlisp.com/$${piece.code}?playlist=true&duration=${duration}&index=${index}&total=${pieces.length}`,
    duration: duration,
    license: 'open',
    provenance: {
      type: 'offChainURI',
      uri: `https://kidlisp.com/$${piece.code}`,
    },
  }));
  
  return {
    dpVersion: '1.1.0',
    title: `Top 100 as of ${dateFormatted}`,
    summary: 'The 100 most popular KidLisp pieces by total hits',
    items: items,
    defaults: {
      display: {
        scaling: 'fit',
        background: '#000000',
        margin: '0%'
      },
      license: 'open',
      duration: 24
    }
  };
}

async function createChannel(playlistId) {
  try {
    const channel = {
      title: 'KidLisp',
      curator: 'prompt.ac',
      summary: 'KidLisp is a TV friendly programming language for kids of all ages! Developed by @jeffrey of Aesthetic Computer.',
      playlists: [
        `${FEED_API_URL}/playlists/${playlistId}`
      ]
    };

    const response = await fetch(`${FEED_API_URL}/channels`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${API_SECRET}`
      },
      body: JSON.stringify(channel)
    });
    
    if (!response.ok) {
      const error = await response.text();
      throw new Error(`HTTP ${response.status}: ${error}`);
    }
    
    const result = await response.json();
    console.log(`✅ Channel created successfully!`);
    console.log(`   ID: ${result.id}`);
    console.log(`   Title: ${result.title}`);
    
    return result;
  } catch (error) {
    console.error(`❌ Failed to create channel:`, error.message);
    throw error;
  }
}

async function uploadPlaylist(playlist) {
  try {
    const response = await fetch(`${FEED_API_URL}/playlists`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${API_SECRET}`
      },
      body: JSON.stringify(playlist)
    });
    
    if (!response.ok) {
      const error = await response.text();
      throw new Error(`HTTP ${response.status}: ${error}`);
    }
    
    const result = await response.json();
    console.log(`✅ Playlist uploaded successfully!`);
    console.log(`   ID: ${result.id}`);
    
    return result;
  } catch (error) {
    console.error(`❌ Failed to upload playlist:`, error.message);
    throw error;
  }
}

async function main() {
  let disconnect;
  try {
    console.log('🎵 Creating top 100 KidLisp playlist...\n');
    
    // Get top 100 pieces from MongoDB
    const { pieces, disconnect: dbDisconnect } = await getTop100KidLispPieces();
    disconnect = dbDisconnect;
    
    if (pieces.length === 0) {
      console.log('⚠️  No KidLisp pieces found!');
      return;
    }
    
    // Generate DP-1 playlist
    const playlist = generateDP1Playlist(pieces);
    
    console.log('\n📝 Generated playlist:');
    console.log(`   Title: ${playlist.title}`);
    console.log(`   Items: ${playlist.items.length}`);
    
    // Output JSON
    console.log('\n📄 Playlist JSON:\n');
    console.log(JSON.stringify(playlist, null, 2));
    
    // Upload to feed service
    console.log('\n📤 Uploading playlist to feed.aesthetic.computer...');
    const uploadedPlaylist = await uploadPlaylist(playlist);
    
    console.log('\n📺 Creating KidLisp channel...');
    const channel = await createChannel(uploadedPlaylist.id);
    
    console.log('\n✅ Done! Playlist and channel created successfully.');
    console.log(`🔗 Playlist: ${FEED_API_URL}/playlists/${uploadedPlaylist.id}`);
    console.log(`📺 Channel: ${FEED_API_URL}/channels/${channel.id}`);
    
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  } finally {
    if (disconnect) {
      await disconnect();
      console.log('\n🔌 Database connection closed.');
    }
  }
}

main().then(() => process.exit(0)).catch(() => process.exit(1));
