#!/usr/bin/env node
/**
 * Trigger Electron app reboot from within the devcontainer
 * 
 * This script creates a marker file that the Electron app monitors.
 * When detected, the app will relaunch itself.
 */

const fs = require('fs');
const path = require('path');

const REBOOT_MARKER = path.join(__dirname, '.reboot-requested');

console.log('Requesting Electron app reboot...');
console.log('Creating marker file:', REBOOT_MARKER);

// Create the marker file
fs.writeFileSync(REBOOT_MARKER, new Date().toISOString());

console.log('✓ Reboot request sent');
console.log('The Electron app should restart shortly...');
