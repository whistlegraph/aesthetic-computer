#!/usr/bin/env node

/**
 * Create KidLisp Colors Playlist
 * 
 * Generates a DP-1 playlist featuring all CSS color names available in 
 * Aesthetic Computer, suitable for TV display and color exploration.
 * 
 * Usage: node create-kidlisp-colors-playlist.mjs
 */

import fetch from 'node-fetch';

// All CSS colors from num.mjs
const cssColors = {
  aliceblue: [240, 248, 255],
  antiquewhite: [250, 235, 215],
  aqua: [0, 255, 255],
  aquamarine: [127, 255, 212],
  azure: [240, 255, 255],
  beige: [245, 245, 220],
  bisque: [255, 228, 196],
  black: [0, 0, 0],
  blanchedalmond: [255, 235, 205],
  blue: [0, 0, 255],
  blueviolet: [138, 43, 226],
  brown: [165, 42, 42],
  burlywood: [222, 184, 135],
  cadetblue: [95, 158, 160],
  chartreuse: [127, 255, 0],
  chocolate: [210, 105, 30],
  coral: [255, 127, 80],
  cornflowerblue: [100, 149, 237],
  cornsilk: [255, 248, 220],
  crimson: [220, 20, 60],
  cyan: [0, 255, 255],
  darkblue: [0, 0, 139],
  darkcyan: [0, 139, 139],
  darkgoldenrod: [184, 134, 11],
  darkgray: [169, 169, 169],
  darkgrey: [169, 169, 169],
  darkgreen: [0, 100, 0],
  darkkhaki: [189, 183, 107],
  darkmagenta: [139, 0, 139],
  darkolivegreen: [85, 107, 47],
  darkorange: [255, 140, 0],
  darkorchid: [153, 50, 204],
  darkred: [139, 0, 0],
  darksalmon: [233, 150, 122],
  darkseagreen: [143, 188, 143],
  darkslateblue: [72, 61, 139],
  darkslategray: [47, 79, 79],
  darkslategrey: [47, 79, 79],
  darkturquoise: [0, 206, 209],
  darkviolet: [148, 0, 211],
  deeppink: [255, 20, 147],
  deepskyblue: [0, 191, 255],
  dimgray: [105, 105, 105],
  dimgrey: [105, 105, 105],
  dodgerblue: [30, 144, 255],
  firebrick: [178, 34, 34],
  floralwhite: [255, 250, 240],
  forestgreen: [34, 139, 34],
  fuchsia: [255, 0, 255],
  gainsboro: [220, 220, 220],
  ghostwhite: [248, 248, 255],
  gold: [255, 215, 0],
  goldenrod: [218, 165, 32],
  gray: [128, 128, 128],
  grey: [128, 128, 128],
  green: [0, 128, 0],
  greenyellow: [173, 255, 47],
  honeydew: [240, 255, 240],
  hotpink: [255, 105, 180],
  indianred: [205, 92, 92],
  indigo: [75, 0, 130],
  ivory: [255, 255, 240],
  khaki: [240, 230, 140],
  lavender: [230, 230, 250],
  lavenderblush: [255, 240, 245],
  lawngreen: [124, 252, 0],
  lemonchiffon: [255, 250, 205],
  lightblue: [173, 216, 230],
  lightcoral: [240, 128, 128],
  lightcyan: [224, 255, 255],
  lightgoldenrodyellow: [250, 250, 210],
  lightgray: [211, 211, 211],
  lightgrey: [211, 211, 211],
  lightgreen: [144, 238, 144],
  lightpink: [255, 182, 193],
  lightsalmon: [255, 160, 122],
  lightseagreen: [32, 178, 170],
  lightskyblue: [135, 206, 250],
  lightslategray: [119, 136, 153],
  lightslategrey: [119, 136, 153],
  lightsteelblue: [176, 196, 222],
  lightyellow: [255, 255, 224],
  lime: [0, 255, 0],
  limegreen: [50, 205, 50],
  linen: [250, 240, 230],
  magenta: [255, 0, 255],
  maroon: [128, 0, 0],
  mediumaquamarine: [102, 205, 170],
  mediumblue: [0, 0, 205],
  mediumorchid: [186, 85, 211],
  mediumpurple: [147, 112, 219],
  mediumseagreen: [60, 179, 113],
  mediumslateblue: [123, 104, 238],
  mediumspringgreen: [0, 250, 154],
  mediumturquoise: [72, 209, 204],
  mediumvioletred: [199, 21, 133],
  midnightblue: [25, 25, 112],
  mintcream: [245, 255, 250],
  mistyrose: [255, 228, 225],
  moccasin: [255, 228, 181],
  navajowhite: [255, 222, 173],
  navy: [0, 0, 128],
  oldlace: [253, 245, 230],
  olive: [128, 128, 0],
  olivedrab: [107, 142, 35],
  orange: [255, 165, 0],
  orangered: [255, 69, 0],
  orchid: [218, 112, 214],
  palegoldenrod: [238, 232, 170],
  palegreen: [152, 251, 152],
  paleturquoise: [175, 238, 238],
  palevioletred: [219, 112, 147],
  papayawhip: [255, 239, 213],
  peachpuff: [255, 218, 185],
  peru: [205, 133, 63],
  pink: [255, 192, 203],
  plum: [221, 160, 221],
  powderblue: [176, 224, 230],
  purple: [128, 0, 128],
  rebeccapurple: [102, 51, 153],
  red: [255, 0, 0],
  rosybrown: [188, 143, 143],
  royalblue: [65, 105, 225],
  saddlebrown: [139, 69, 19],
  salmon: [250, 128, 114],
  sandybrown: [244, 164, 96],
  seagreen: [46, 139, 87],
  seashell: [255, 245, 238],
  sienna: [160, 82, 45],
  silver: [192, 192, 192],
  skyblue: [135, 206, 235],
  slateblue: [106, 90, 205],
  slategray: [112, 128, 144],
  slategrey: [112, 128, 144],
  snow: [255, 250, 250],
  springgreen: [0, 255, 127],
  steelblue: [70, 130, 180],
  tan: [210, 180, 140],
  teal: [0, 128, 128],
  thistle: [216, 191, 216],
  tomato: [255, 99, 71],
  turquoise: [64, 224, 208],
  violet: [238, 130, 238],
  wheat: [245, 222, 179],
  white: [255, 255, 255],
  whitesmoke: [245, 245, 245],
  yellow: [255, 255, 0],
  yellowgreen: [154, 205, 50],
  // Custom brown colors for AC
  darkbrown: [101, 67, 33],
  darkerbrown: [62, 39, 35],
  darksienna: [139, 90, 43],
};

const FEED_API_URL = 'https://feed.aesthetic.computer/api/v1';
const API_SECRET = process.env.FEED_API_SECRET || '008f7c7ceab429051d18370f5d580fcee453cdf0768c900d71660367feb95436';

/**
 * Generate DP-1 compliant playlist from color names
 */
function generateColorPlaylist() {
  const colorNames = Object.keys(cssColors);
  const now = new Date();
  const dateString = now.toLocaleDateString('en-US', { 
    weekday: 'long', 
    year: 'numeric', 
    month: 'long', 
    day: 'numeric' 
  });

  const playlist = {
    dpVersion: '1.0.0',
    title: 'Colors',
    description: 'Explore all CSS color names available in Aesthetic Computer\'s KidLisp! Each color is rendered as a solid fill.',
    license: 'open',
    attribution: 'Curated by prompt.ac',
    items: colorNames.map((colorName, index) => ({
      source: `https://aesthetic.computer/${colorName}?tv=true&density=5`,
      title: colorName,
      creator: 'Aesthetic Computer',
      duration: 24,
      license: 'open',
      url: `/${colorName}`,
    })),
  };

  return playlist;
}

/**
 * Create a channel for the colors playlist
 */
function createColorChannel(playlistId) {
  const now = new Date();
  const dateString = now.toLocaleDateString('en-US', { 
    weekday: 'long', 
    year: 'numeric', 
    month: 'long', 
    day: 'numeric' 
  });

  return {
    title: `KidLisp Colors`,
    curator: 'prompt.ac',
    summary: 'A visual tour through all CSS color names available in KidLisp! Perfect for learning colors and exploring the full palette of web design.',
    playlists: [
      `${FEED_API_URL}/playlists/${playlistId}`
    ],
  };
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
 * Create a channel
 */
async function createChannel(channelData) {
  const response = await fetch(`${FEED_API_URL}/channels`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${API_SECRET}`,
    },
    body: JSON.stringify(channelData),
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Failed to create channel: ${response.status} ${error}`);
  }

  return await response.json();
}

/**
 * Main execution
 */
async function main() {
  try {
    console.log('🎨 Creating KidLisp Colors playlist...\n');

    // Generate playlist
    const playlist = generateColorPlaylist();
    console.log(`📝 Generated playlist with ${playlist.items.length} colors`);
    console.log(`🎨 Sample colors: ${Object.keys(cssColors).slice(0, 10).join(', ')}...\n`);

    // Upload playlist
    console.log('📤 Uploading playlist to feed.aesthetic.computer...');
    const playlistResult = await uploadPlaylist(playlist);
    console.log(`✅ Playlist uploaded successfully! ID: ${playlistResult.id}`);
    console.log(`🔗 ${FEED_API_URL}/playlists/${playlistResult.id}\n`);

    // Create channel
    console.log('📺 Creating KidLisp Colors channel...');
    const channelData = createColorChannel(playlistResult.id);
    const channelResult = await createChannel(channelData);
    console.log(`✅ Channel created successfully! ID: ${channelResult.id}`);
    console.log(`🔗 ${FEED_API_URL}/channels/${channelResult.id}\n`);

    console.log('🎉 Done!');
  } catch (error) {
    console.error('❌ Error:', error.message);
    process.exit(1);
  }
}

main();
