#!/usr/bin/env node
// publish.mjs
// Automated Max for Live device publishing script
// Builds production devices, generates checksums, copies to assets, syncs to S3,
// and creates MongoDB records for version tracking

import { exec } from 'child_process';
import { readFileSync, statSync, copyFileSync, existsSync, mkdirSync } from 'fs';
import { createHash } from 'crypto';
import { promisify } from 'util';
import { fileURLToPath } from 'url';
import { dirname, join } from 'path';
import { connect } from '../system/backend/database.mjs';
import { createPlugin, generateCode } from '../system/backend/plugins.mjs';

const execAsync = promisify(exec);

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

// Parse command line arguments
const args = process.argv.slice(2);
const deviceFilter = args.find(arg => !arg.startsWith('--'));
const versionArg = args.find(arg => arg.startsWith('--version='));
const versionOverride = versionArg ? versionArg.split('=')[1] : null;
const skipSync = args.includes('--skip-sync');

/**
 * Get version for a device
 * Priority: CLI override > devices.json version > package.json version > default 1.0.0
 */
async function getDeviceVersion(deviceName) {
  if (versionOverride) {
    return versionOverride;
  }

  // Try to read from devices.json if it has a version field
  const configPath = join(__dirname, 'devices.json');
  const config = JSON.parse(readFileSync(configPath, 'utf8'));
  const device = config.devices.find(d => d.piece === deviceName);

  if (device?.version) {
    return device.version;
  }

  // Try package.json
  const packagePath = join(__dirname, '..', 'package.json');
  if (existsSync(packagePath)) {
    const pkg = JSON.parse(readFileSync(packagePath, 'utf8'));
    if (pkg.version) {
      return pkg.version;
    }
  }

  // Default version
  return '1.0.0';
}

/**
 * Parse version string into components
 */
function parseVersion(versionString) {
  const parts = versionString.split('.');
  return {
    major: parseInt(parts[0]) || 1,
    minor: parseInt(parts[1]) || 0,
    patch: parseInt(parts[2]) || 0,
    string: versionString
  };
}

/**
 * Generate icon emoji based on device type
 */
function getDeviceIcon(deviceType) {
  if (deviceType === 'effect') return '🎸';
  if (deviceType === 'midi') return '🎹';
  return '🟪';  // Default for instruments
}

/**
 * Publish devices to MongoDB and S3
 */
async function publishPlugins() {
  console.log('🎹 AC Max for Live Device Publisher');
  console.log('=====================================\n');

  try {
    // 1. Build production devices
    console.log('🔨 Building production devices...');
    const buildResult = await execAsync('python3 build.py --production', {
      cwd: __dirname
    });
    console.log(buildResult.stdout);
    if (buildResult.stderr) console.error(buildResult.stderr);

    // 2. Read devices.json configuration
    const configPath = join(__dirname, 'devices.json');
    const config = JSON.parse(readFileSync(configPath, 'utf8'));

    // Filter devices if specified
    let devices = config.devices;
    if (deviceFilter) {
      devices = devices.filter(d => d.piece === deviceFilter);
      if (devices.length === 0) {
        console.error(`❌ Device '${deviceFilter}' not found in devices.json`);
        process.exit(1);
      }
    }

    // Skip devices with slashes in piece names (build.py can't handle them)
    devices = devices.filter(d => !d.piece.includes('/'));
    if (devices.length === 0) {
      console.warn('⚠️  No valid devices to publish (devices with slashes in names are skipped)');
      process.exit(0);
    }

    // 3. Connect to database
    console.log('\n📦 Connecting to database...');
    const { db, disconnect } = await connect();

    // 4. Process each device
    const assetsDir = join(__dirname, '..', 'system', 'public', 'assets', 'm4l');
    if (!existsSync(assetsDir)) {
      mkdirSync(assetsDir, { recursive: true });
    }

    const publishedPlugins = [];

    for (const device of devices) {
      console.log(`\n📱 Processing: ${device.name}`);

      const deviceType = device.type || 'instrument';
      const icon = getDeviceIcon(deviceType);
      const fileName = `AC ${icon} ${device.piece} (aesthetic.computer).amxd`;
      const filePath = join(__dirname, fileName);

      // Check if file exists
      if (!existsSync(filePath)) {
        console.warn(`⚠️  File not found: ${fileName}, skipping...`);
        continue;
      }

      // Get file stats and checksum
      const stats = statSync(filePath);
      const fileBuffer = readFileSync(filePath);
      const checksum = createHash('sha256').update(fileBuffer).digest('hex');

      // Get version
      const versionString = await getDeviceVersion(device.piece);
      const version = parseVersion(versionString);

      // Copy to assets directory
      const destPath = join(assetsDir, fileName);
      copyFileSync(filePath, destPath);
      console.log(`   📦 Copied to: ${destPath}`);
      console.log(`   📏 Size: ${(stats.size / 1024).toFixed(2)} KB`);
      console.log(`   🔐 SHA256: ${checksum.substring(0, 16)}...`);

      // Generate unique code
      const code = generateCode(device.piece, version);

      // Create plugin data
      const pluginData = {
        code,
        device: {
          name: device.piece,
          displayName: device.name,
          category: deviceType,
          icon: icon
        },
        version,
        m4l: {
          downloadUrl: `https://assets.aesthetic.computer/m4l/${encodeURIComponent(fileName)}`,
          fileName,
          fileSize: stats.size,
          checksum
        },
        metadata: {
          description: device.description || `${device.name} Max for Live device`,
          piece: device.piece,
          width: device.width || 400,
          height: device.height || 200,
          releaseNotes: `Version ${versionString} release`
        },
        stats: {
          downloads: 0,
          views: 0
        },
        publishedAt: new Date(),
        deprecated: false
      };

      // Insert into MongoDB
      try {
        const plugin = await createPlugin(db, pluginData);
        publishedPlugins.push(plugin);
        console.log(`   ✅ Published to database: ${code}`);
        console.log(`   🔗 URL: ${plugin.m4l.downloadUrl}`);
      } catch (error) {
        console.error(`   ❌ Database error: ${error.message}`);
      }
    }

    // Disconnect from database
    await disconnect();
    console.log('\n✅ Database updates complete');

    // 5. Sync to S3
    if (!skipSync) {
      console.log('\n☁️  Syncing to S3/Digital Ocean Spaces...');
      try {
        const syncResult = await execAsync('npm run assets:sync:up', {
          cwd: join(__dirname, '..')
        });
        console.log(syncResult.stdout);
        if (syncResult.stderr) console.warn(syncResult.stderr);
        console.log('✅ S3 sync complete');
      } catch (error) {
        console.error('❌ S3 sync failed:', error.message);
        console.error('   You may need to run "npm run assets:sync:up" manually');
      }
    } else {
      console.log('\n⏭️  Skipping S3 sync (--skip-sync flag)');
    }

    // 6. Summary
    console.log('\n🎉 Publishing Complete!');
    console.log('========================');
    console.log(`Published ${publishedPlugins.length} plugin(s):\n`);

    publishedPlugins.forEach(plugin => {
      console.log(`  ${plugin.device.icon} ${plugin.device.displayName} v${plugin.version.string}`);
      console.log(`     Code: ${plugin.code}`);
      console.log(`     URL: ${plugin.m4l.downloadUrl}\n`);
    });

    console.log('Next steps:');
    console.log('  • Test downloads at https://aesthetic.computer/ableton');
    console.log('  • Verify files at https://assets.aesthetic.computer/m4l/');
    console.log('  • Install in Ableton Live to test functionality');

  } catch (error) {
    console.error('\n❌ Publishing failed:', error);
    console.error(error.stack);
    process.exit(1);
  }
}

// Run if called directly
if (import.meta.url === `file://${process.argv[1]}`) {
  publishPlugins();
}

export { publishPlugins };
