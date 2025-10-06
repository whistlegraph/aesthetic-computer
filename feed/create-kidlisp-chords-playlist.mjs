#!/usr/bin/env node

/**
 * Create KidLisp Chords Playlist
 * 
 * Generates a DP-1 playlist featuring common Western musical chords
 * using the clock piece with notepat notation.
 * 
 * Usage: node create-kidlisp-chords-playlist.mjs
 */

import fetch from 'node-fetch';

// Common Western chords using notepat notation
// Format: name, notes (as they appear in clock)
const chords = [
  // Major Chords
  { name: 'C Major', notes: 'ceg' },
  { name: 'D Major', notes: 'dwv' }, // d f# a
  { name: 'E Major', notes: 'ewh' }, // e g# b
  { name: 'F Major', notes: 'fac' },
  { name: 'G Major', notes: 'gbd' },
  { name: 'A Major', notes: 'avh' }, // a c# e
  { name: 'B Major', notes: 'bsi' }, // b d# f#
  
  // Minor Chords
  { name: 'C Minor', notes: 'crg' }, // c eb g
  { name: 'D Minor', notes: 'dfa' },
  { name: 'E Minor', notes: 'egb' },
  { name: 'F Minor', notes: 'fry' }, // f ab c
  { name: 'G Minor', notes: 'gqd' }, // g bb d
  { name: 'A Minor', notes: 'ace' },
  { name: 'B Minor', notes: 'bdi' }, // b d f#
  
  // Seventh Chords
  { name: 'C Major 7', notes: 'cegb' },
  { name: 'D Major 7', notes: 'dwvv' }, // d f# a c#
  { name: 'G Major 7', notes: 'gbdw' }, // g b d f#
  { name: 'A Minor 7', notes: 'aceg' },
  { name: 'E Minor 7', notes: 'egbd' },
  
  // Diminished Chords
  { name: 'C Diminished', notes: 'crq' }, // c eb gb
  { name: 'D Diminished', notes: 'dfy' }, // d f ab
  { name: 'B Diminished', notes: 'bdf' },
  
  // Augmented Chords
  { name: 'C Augmented', notes: 'cew' }, // c e g#
  { name: 'F Augmented', notes: 'fav' }, // f a c#
  
  // Suspended Chords
  { name: 'C Sus4', notes: 'cfg' },
  { name: 'D Sus4', notes: 'dga' },
  { name: 'G Sus4', notes: 'gcd' },
  { name: 'A Sus4', notes: 'ade' },
  
  // Extended Chords
  { name: 'C Major 9', notes: 'cegbd' },
  { name: 'D Minor 9', notes: 'dfae' },
  { name: 'G Dominant 7', notes: 'gbdf' },
];

const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET || '008f7c7ceab429051d18370f5d580fcee453cdf0768c900d71660367feb95436';

/**
 * Generate DP-1 compliant playlist from chord definitions
 */
function generateChordsPlaylist() {
  // Create the DP-1 playlist
  const playlist = {
    dpVersion: '1.0.0',
    title: 'Chords for `clock`',
    description: 'Western musical chords using the clock piece with notepat notation',
    license: 'open',
    items: chords.map((chord, index) => ({
      source: `https://aesthetic.computer/clock~^${chord.notes}?tv=true&density=5`,
      title: chord.name,
      creator: 'Aesthetic Computer',
      duration: 24,
      license: 'open',
      url: `/clock~^${chord.notes}`,
    })),
  };

  return playlist;
}

/**
 * Upload playlist to feed API
 */
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

/**
 * Main execution
 */
async function main() {
  try {
    console.log('🎵 Creating KidLisp Chords playlist...\n');

    // Generate playlist
    const playlist = generateChordsPlaylist();
    console.log(`📝 Generated playlist with ${playlist.items.length} chords`);
    console.log(`🎵 Sample chords: ${chords.slice(0, 5).map(c => c.name).join(', ')}...\n`);

    // Upload playlist
    console.log('📤 Uploading playlist to feed.aesthetic.computer...');
    const playlistResult = await uploadPlaylist(playlist);
    console.log(`✅ Playlist uploaded successfully! ID: ${playlistResult.id}`);
    console.log(`🔗 ${FEED_API_URL}/playlists/${playlistResult.id}\n`);

    console.log('🎉 Done!');
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

main();
