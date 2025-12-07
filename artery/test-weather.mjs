#!/usr/bin/env node
/**
 * 🌤️ Weather Display Test Suite
 * 
 * Tests the Weather Channel style ambient display with generative music.
 * Includes world tour of locations and audio verification.
 * 
 * Usage: node test-weather.mjs [command] [options]
 * 
 * Commands:
 *   (default)     - Run weather at default location (New York)
 *   tour          - World tour: cycle through global locations
 *   test          - Run automated test suite
 * 
 * Options:
 *   --location, -l <name>   - Location name or preset shortcode
 *   --audio, -a             - Enable audio context (required for music)
 *   --duration, -d <secs>   - Duration per location in tour mode (default: 30)
 *   --list                  - List all preset locations
 * 
 * Examples:
 *   node test-weather.mjs --audio           - NYC with music
 *   node test-weather.mjs -l tokyo -a       - Tokyo with music
 *   node test-weather.mjs tour -a -d 20     - World tour, 20s each, with music
 *   node test-weather.mjs test              - Run test suite
 */

import Artery from './artery.mjs';

// ═══════════════════════════════════════════════════════════════════════════
// TERMINAL COLORS
// ═══════════════════════════════════════════════════════════════════════════
const RESET = '\x1b[0m';
const GREEN = '\x1b[92m';
const CYAN = '\x1b[96m';
const YELLOW = '\x1b[93m';
const RED = '\x1b[91m';
const MAGENTA = '\x1b[95m';
const DIM = '\x1b[2m';
const BOLD = '\x1b[1m';
const BLUE_BG = '\x1b[44m';
const WHITE = '\x1b[97m';

const testLog = (msg) => console.log(`${BLUE_BG}${WHITE}🌤️${RESET} ${msg}`);
const successLog = (msg) => console.log(`${GREEN}✅ ${msg}${RESET}`);
const infoLog = (msg) => console.log(`${CYAN}ℹ️  ${msg}${RESET}`);
const warnLog = (msg) => console.log(`${YELLOW}⚠️  ${msg}${RESET}`);
const errorLog = (msg) => console.log(`${RED}❌ ${msg}${RESET}`);
const musicLog = (msg) => console.log(`${MAGENTA}🎶 ${msg}${RESET}`);

// ═══════════════════════════════════════════════════════════════════════════
// WORLD LOCATIONS - Global weather tour
// ═══════════════════════════════════════════════════════════════════════════
const WORLD_LOCATIONS = {
  // Americas
  'nyc': { name: 'New York', region: 'USA', coords: [40.7128, -74.0060] },
  'la': { name: 'Los Angeles', region: 'USA', coords: [34.0522, -118.2437] },
  'miami': { name: 'Miami', region: 'USA', coords: [25.7617, -80.1918] },
  'sf': { name: 'San Francisco', region: 'USA', coords: [37.7749, -122.4194] },
  'chicago': { name: 'Chicago', region: 'USA', coords: [41.8781, -87.6298] },
  'denver': { name: 'Denver', region: 'USA', coords: [39.7392, -104.9903] },
  'seattle': { name: 'Seattle', region: 'USA', coords: [47.6062, -122.3321] },
  'cdmx': { name: 'Mexico City', region: 'Mexico', coords: [19.4326, -99.1332] },
  'saopaulo': { name: 'São Paulo', region: 'Brazil', coords: [-23.5505, -46.6333] },
  'buenosaires': { name: 'Buenos Aires', region: 'Argentina', coords: [-34.6037, -58.3816] },
  
  // Europe
  'london': { name: 'London', region: 'UK', coords: [51.5074, -0.1278] },
  'paris': { name: 'Paris', region: 'France', coords: [48.8566, 2.3522] },
  'berlin': { name: 'Berlin', region: 'Germany', coords: [52.5200, 13.4050] },
  'rome': { name: 'Rome', region: 'Italy', coords: [41.9028, 12.4964] },
  'madrid': { name: 'Madrid', region: 'Spain', coords: [40.4168, -3.7038] },
  'amsterdam': { name: 'Amsterdam', region: 'Netherlands', coords: [52.3676, 4.9041] },
  'moscow': { name: 'Moscow', region: 'Russia', coords: [55.7558, 37.6173] },
  'stockholm': { name: 'Stockholm', region: 'Sweden', coords: [59.3293, 18.0686] },
  'reykjavik': { name: 'Reykjavik', region: 'Iceland', coords: [64.1466, -21.9426] },
  
  // Asia
  'tokyo': { name: 'Tokyo', region: 'Japan', coords: [35.6762, 139.6503] },
  'seoul': { name: 'Seoul', region: 'South Korea', coords: [37.5665, 126.9780] },
  'beijing': { name: 'Beijing', region: 'China', coords: [39.9042, 116.4074] },
  'shanghai': { name: 'Shanghai', region: 'China', coords: [31.2304, 121.4737] },
  'hongkong': { name: 'Hong Kong', region: 'China', coords: [22.3193, 114.1694] },
  'singapore': { name: 'Singapore', region: 'Singapore', coords: [1.3521, 103.8198] },
  'bangkok': { name: 'Bangkok', region: 'Thailand', coords: [13.7563, 100.5018] },
  'mumbai': { name: 'Mumbai', region: 'India', coords: [19.0760, 72.8777] },
  'delhi': { name: 'New Delhi', region: 'India', coords: [28.6139, 77.2090] },
  'dubai': { name: 'Dubai', region: 'UAE', coords: [25.2048, 55.2708] },
  'telaviv': { name: 'Tel Aviv', region: 'Israel', coords: [32.0853, 34.7818] },
  
  // Oceania
  'sydney': { name: 'Sydney', region: 'Australia', coords: [-33.8688, 151.2093] },
  'melbourne': { name: 'Melbourne', region: 'Australia', coords: [-37.8136, 144.9631] },
  'auckland': { name: 'Auckland', region: 'New Zealand', coords: [-36.8509, 174.7645] },
  
  // Africa
  'cairo': { name: 'Cairo', region: 'Egypt', coords: [30.0444, 31.2357] },
  'lagos': { name: 'Lagos', region: 'Nigeria', coords: [6.5244, 3.3792] },
  'capetown': { name: 'Cape Town', region: 'South Africa', coords: [-33.9249, 18.4241] },
  'nairobi': { name: 'Nairobi', region: 'Kenya', coords: [-1.2921, 36.8219] },
  
  // Extreme weather test locations
  'phoenix': { name: 'Phoenix', region: 'USA (Hot)', coords: [33.4484, -112.0740] },
  'fairbanks': { name: 'Fairbanks', region: 'Alaska (Cold)', coords: [64.8378, -147.7164] },
  'cherrapunji': { name: 'Cherrapunji', region: 'India (Rainy)', coords: [25.2700, 91.7320] },
};

// Subset for quick tour
const TOUR_LOCATIONS = [
  'nyc', 'london', 'tokyo', 'sydney', 'dubai', 'paris',
  'saopaulo', 'moscow', 'singapore', 'cairo', 'la', 'berlin'
];

// ═══════════════════════════════════════════════════════════════════════════
// PARSE ARGUMENTS
// ═══════════════════════════════════════════════════════════════════════════
function parseArgs(args) {
  const options = {
    command: 'run',           // run, tour, test
    location: 'nyc',
    audio: false,
    duration: 30,             // seconds per location in tour
  };
  
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    
    if (arg === 'tour') options.command = 'tour';
    else if (arg === 'test') options.command = 'test';
    else if (arg === '--audio' || arg === '-a') options.audio = true;
    else if (arg === '--list') {
      showLocations();
      process.exit(0);
    }
    else if (arg === '--help' || arg === '-h') {
      showHelp();
      process.exit(0);
    }
    else if ((arg === '--location' || arg === '-l') && args[i + 1]) {
      options.location = args[++i].toLowerCase();
    }
    else if ((arg === '--duration' || arg === '-d') && args[i + 1]) {
      options.duration = parseInt(args[++i], 10) || 30;
    }
    else if (!arg.startsWith('-')) {
      // Bare argument could be location or command
      if (['tour', 'test'].includes(arg)) {
        options.command = arg;
      } else {
        options.location = arg.toLowerCase();
      }
    }
  }
  
  return options;
}

// ═══════════════════════════════════════════════════════════════════════════
// HELP & INFO
// ═══════════════════════════════════════════════════════════════════════════
function showHelp() {
  console.log(`
${BOLD}${CYAN}🌤️  Weather Display Test Suite${RESET}
${DIM}Weather Channel style ambient display with generative music${RESET}

${BOLD}Usage:${RESET}
  node test-weather.mjs [command] [options]

${BOLD}Commands:${RESET}
  ${CYAN}(default)${RESET}     Run weather at a single location
  ${CYAN}tour${RESET}          World tour: cycle through global locations
  ${CYAN}test${RESET}          Run automated test suite

${BOLD}Options:${RESET}
  ${YELLOW}--audio, -a${RESET}             Enable audio context (${BOLD}required for music!${RESET})
  ${YELLOW}--location, -l <name>${RESET}   Location name or preset shortcode
  ${YELLOW}--duration, -d <secs>${RESET}   Seconds per location in tour mode (default: 30)
  ${YELLOW}--list${RESET}                  List all ${Object.keys(WORLD_LOCATIONS).length} preset locations

${BOLD}Examples:${RESET}
  node test-weather.mjs ${DIM}# NYC, no music${RESET}
  node test-weather.mjs ${YELLOW}-a${RESET} ${DIM}# NYC with music${RESET}
  node test-weather.mjs ${YELLOW}-l tokyo -a${RESET} ${DIM}# Tokyo with music${RESET}
  node test-weather.mjs ${CYAN}tour${RESET} ${YELLOW}-a -d 20${RESET} ${DIM}# World tour, 20s each${RESET}
  node test-weather.mjs ${CYAN}test${RESET} ${DIM}# Run test suite${RESET}

${BOLD}Preset Regions:${RESET}
  ${DIM}Americas:${RESET} nyc, la, miami, sf, chicago, denver, seattle, cdmx, saopaulo
  ${DIM}Europe:${RESET}   london, paris, berlin, rome, madrid, amsterdam, moscow, stockholm
  ${DIM}Asia:${RESET}     tokyo, seoul, beijing, shanghai, hongkong, singapore, mumbai, dubai
  ${DIM}Other:${RESET}    sydney, melbourne, auckland, cairo, lagos, capetown
`);
}

function showLocations() {
  console.log(`\n${BOLD}${CYAN}🌍 Weather Test Locations${RESET}\n`);
  
  const regions = {
    'Americas': ['nyc', 'la', 'miami', 'sf', 'chicago', 'denver', 'seattle', 'cdmx', 'saopaulo', 'buenosaires'],
    'Europe': ['london', 'paris', 'berlin', 'rome', 'madrid', 'amsterdam', 'moscow', 'stockholm', 'reykjavik'],
    'Asia': ['tokyo', 'seoul', 'beijing', 'shanghai', 'hongkong', 'singapore', 'bangkok', 'mumbai', 'delhi', 'dubai', 'telaviv'],
    'Oceania': ['sydney', 'melbourne', 'auckland'],
    'Africa': ['cairo', 'lagos', 'capetown', 'nairobi'],
    'Extreme': ['phoenix', 'fairbanks', 'cherrapunji'],
  };
  
  for (const [region, codes] of Object.entries(regions)) {
    console.log(`${YELLOW}${region}:${RESET}`);
    for (const code of codes) {
      const loc = WORLD_LOCATIONS[code];
      if (loc) {
        console.log(`  ${CYAN}${code.padEnd(12)}${RESET} ${loc.name}, ${DIM}${loc.region}${RESET}`);
      }
    }
    console.log();
  }
  
  console.log(`${DIM}Total: ${Object.keys(WORLD_LOCATIONS).length} locations${RESET}\n`);
}

// ═══════════════════════════════════════════════════════════════════════════
// SINGLE LOCATION RUN
// ═══════════════════════════════════════════════════════════════════════════
async function runSingleLocation(options) {
  const { location, audio } = options;
  const loc = WORLD_LOCATIONS[location] || { name: location, region: '' };
  
  console.log(`\n${BOLD}${CYAN}🌤️  Weather Display${RESET}`);
  console.log(`${DIM}Weather Channel style ambient display${RESET}\n`);
  
  infoLog(`Location: ${BOLD}${loc.name}${loc.region ? `, ${loc.region}` : ''}${RESET}`);
  if (audio) {
    musicLog(`Audio enabled - music will play!`);
  } else {
    warnLog(`Audio disabled - use ${YELLOW}-a${RESET} flag for music`);
  }
  console.log();
  
  const client = new Artery();
  
  try {
    testLog('Connecting to AC...');
    await client.connect();
    successLog('Connected');
    
    // Enable audio context if requested
    if (audio) {
      testLog('Enabling audio context...');
      await client.send({ type: 'enable-audio' });
      musicLog('Audio context enabled');
    }
    
    const weatherPath = `weather~${loc.name}`;
    testLog(`Navigating to: ${weatherPath}`);
    await client.jump(weatherPath);
    
    await new Promise(resolve => setTimeout(resolve, 2000));
    successLog('Weather display loaded!\n');
    
    console.log(`${BOLD}Display Info:${RESET}`);
    console.log(`  📍 Location: ${CYAN}${loc.name}${RESET}`);
    if (loc.coords) {
      console.log(`  🌐 Coords: ${DIM}${loc.coords[0].toFixed(2)}, ${loc.coords[1].toFixed(2)}${RESET}`);
    }
    if (audio) {
      console.log(`  🎶 Music: ${GREEN}Playing${RESET} (weather-reactive ambient)`);
    }
    console.log(`\n${DIM}Views scroll continuously - Weather Channel style`);
    console.log(`Press Ctrl+C to exit${RESET}\n`);
    
    // Keep running until interrupted
    await new Promise((resolve) => {
      process.on('SIGINT', () => {
        console.log(`\n${YELLOW}Exiting...${RESET}`);
        resolve();
      });
    });
    
  } catch (error) {
    errorLog(`Error: ${error.message}`);
    process.exit(1);
  } finally {
    client.disconnect();
    testLog('Disconnected');
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// WORLD TOUR
// ═══════════════════════════════════════════════════════════════════════════
async function runWorldTour(options) {
  const { audio, duration } = options;
  
  console.log(`\n${BOLD}${CYAN}🌍 Weather World Tour${RESET}`);
  console.log(`${DIM}Cycling through ${TOUR_LOCATIONS.length} global locations${RESET}\n`);
  
  if (audio) {
    musicLog(`Audio enabled - weather-reactive music at each location!`);
  } else {
    warnLog(`Audio disabled - use ${YELLOW}-a${RESET} flag for music`);
  }
  console.log(`⏱️  Duration: ${duration} seconds per location\n`);
  
  const client = new Artery();
  let currentIndex = 0;
  let running = true;
  
  try {
    testLog('Connecting to AC...');
    await client.connect();
    successLog('Connected');
    
    if (audio) {
      testLog('Enabling audio context...');
      await client.send({ type: 'enable-audio' });
      musicLog('Audio context enabled');
    }
    
    // Handle Ctrl+C
    process.on('SIGINT', () => {
      console.log(`\n${YELLOW}Tour interrupted${RESET}`);
      running = false;
    });
    
    console.log(`\n${BOLD}Starting tour...${RESET}\n`);
    console.log('─'.repeat(60));
    
    while (running) {
      const code = TOUR_LOCATIONS[currentIndex];
      const loc = WORLD_LOCATIONS[code];
      const num = currentIndex + 1;
      
      console.log(`\n${BOLD}[${num}/${TOUR_LOCATIONS.length}]${RESET} ${CYAN}${loc.name}${RESET}, ${DIM}${loc.region}${RESET}`);
      
      const weatherPath = `weather~${loc.name}`;
      await client.jump(weatherPath);
      
      // Countdown
      for (let i = duration; i > 0 && running; i--) {
        process.stdout.write(`\r  ⏱️  ${i}s remaining...  `);
        await new Promise(r => setTimeout(r, 1000));
      }
      process.stdout.write('\r' + ' '.repeat(30) + '\r');
      
      if (running) {
        currentIndex = (currentIndex + 1) % TOUR_LOCATIONS.length;
        if (currentIndex === 0) {
          console.log(`\n${'─'.repeat(60)}`);
          console.log(`${GREEN}🔄 Tour complete! Restarting...${RESET}`);
          console.log('─'.repeat(60));
        }
      }
    }
    
  } catch (error) {
    errorLog(`Error: ${error.message}`);
    process.exit(1);
  } finally {
    client.disconnect();
    testLog('Disconnected');
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// TEST SUITE
// ═══════════════════════════════════════════════════════════════════════════
async function runTestSuite() {
  console.log(`\n${BOLD}${CYAN}🧪 Weather Display Test Suite${RESET}\n`);
  
  const client = new Artery();
  const results = { passed: 0, failed: 0, tests: [] };
  
  const addResult = (name, passed, message = '') => {
    results.tests.push({ name, passed, message });
    if (passed) results.passed++;
    else results.failed++;
    console.log(`  ${passed ? GREEN + '✅' : RED + '❌'} ${name}${message ? ` - ${DIM}${message}${RESET}` : ''}${RESET}`);
  };
  
  try {
    // Test 1: Connection
    testLog('Test 1: Connection');
    try {
      await client.connect();
      addResult('Connect to AC', true);
    } catch (e) {
      addResult('Connect to AC', false, e.message);
      throw e;
    }
    
    // Test 2: Enable audio
    testLog('Test 2: Audio Context');
    try {
      await client.send({ type: 'enable-audio' });
      addResult('Enable audio context', true);
    } catch (e) {
      addResult('Enable audio context', false, e.message);
    }
    
    // Test 3: Load weather with default location
    testLog('Test 3: Default Location (NYC)');
    try {
      await client.jump('weather~New York');
      await new Promise(r => setTimeout(r, 3000));
      addResult('Load NYC weather', true);
    } catch (e) {
      addResult('Load NYC weather', false, e.message);
    }
    
    // Test 4: Location change
    testLog('Test 4: Location Change');
    const testLocations = ['tokyo', 'london', 'sydney'];
    for (const loc of testLocations) {
      const locData = WORLD_LOCATIONS[loc];
      try {
        await client.jump(`weather~${locData.name}`);
        await new Promise(r => setTimeout(r, 2000));
        addResult(`Load ${locData.name}`, true);
      } catch (e) {
        addResult(`Load ${locData.name}`, false, e.message);
      }
    }
    
    // Test 5: Coordinates input
    testLog('Test 5: Coordinate Input');
    try {
      await client.jump('weather~48.8566,2.3522'); // Paris coords
      await new Promise(r => setTimeout(r, 2000));
      addResult('Load by coordinates', true);
    } catch (e) {
      addResult('Load by coordinates', false, e.message);
    }
    
    // Test 6: Invalid location handling
    testLog('Test 6: Error Handling');
    try {
      await client.jump('weather~INVALID_LOCATION_12345');
      await new Promise(r => setTimeout(r, 2000));
      // Should still load (with error message shown)
      addResult('Handle invalid location gracefully', true);
    } catch (e) {
      addResult('Handle invalid location gracefully', false, e.message);
    }
    
  } catch (error) {
    errorLog(`Test suite error: ${error.message}`);
  } finally {
    client.disconnect();
    
    // Summary
    console.log(`\n${'─'.repeat(60)}`);
    console.log(`${BOLD}Test Results:${RESET}`);
    console.log(`  ${GREEN}Passed: ${results.passed}${RESET}`);
    console.log(`  ${RED}Failed: ${results.failed}${RESET}`);
    console.log(`  Total: ${results.passed + results.failed}`);
    console.log('─'.repeat(60) + '\n');
    
    process.exit(results.failed > 0 ? 1 : 0);
  }
}

// ═══════════════════════════════════════════════════════════════════════════
// MAIN
// ═══════════════════════════════════════════════════════════════════════════
export async function main() {
  const args = process.argv.slice(2);
  const options = parseArgs(args);
  
  switch (options.command) {
    case 'tour':
      await runWorldTour(options);
      break;
    case 'test':
      await runTestSuite();
      break;
    default:
      await runSingleLocation(options);
  }
}

main().catch((e) => {
  errorLog(e.message);
  process.exit(1);
});
