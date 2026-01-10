#!/usr/bin/env node
/**
 * Backup thomaslawson.com WordPress site
 * Downloads: database dumps + wp-content folder
 */

import { spawn } from 'child_process';
import { existsSync, mkdirSync } from 'fs';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const BACKUP_DIR = join(__dirname, '..', 'gigs', 'thomaslawson.com', 'backups', '2026-01-09');
const SSH_KEY = join(__dirname, '..', 'gigs', 'thomaslawson.com', 'ssh', 'thomaslawson_ed25519');
const REMOTE_USER = 'ihfdfni4xqvj';
const REMOTE_HOST = '208.109.70.142';

// Ensure backup dir exists
if (!existsSync(BACKUP_DIR)) {
  mkdirSync(BACKUP_DIR, { recursive: true });
}

async function runSftp(commands) {
  return new Promise((resolve, reject) => {
    const batchFile = join(BACKUP_DIR, '.sftp-batch');
    const fs = await import('fs');
    fs.default.writeFileSync(batchFile, commands.join('\n'));
    
    const sftp = spawn('sftp', [
      '-i', SSH_KEY,
      '-o', 'StrictHostKeyChecking=no',
      '-b', batchFile,
      `${REMOTE_USER}@${REMOTE_HOST}`
    ]);
    
    let output = '';
    sftp.stdout.on('data', d => { output += d; console.log(d.toString()); });
    sftp.stderr.on('data', d => console.error(d.toString()));
    sftp.on('close', code => {
      fs.default.unlinkSync(batchFile);
      code === 0 ? resolve(output) : reject(new Error(`sftp exit ${code}`));
    });
  });
}

async function main() {
  console.log('📦 Backing up thomaslawson.com');
  console.log('============================\n');
  console.log('Backup directory:', BACKUP_DIR);
  console.log('');
  
  // Download database backups
  console.log('📥 Downloading MySQL backups...');
  await runSftp([
    `lcd ${BACKUP_DIR}`,
    'cd .mysql_backup',
    'get *'
  ]);
  
  console.log('\n📥 Downloading wp-config.php...');
  await runSftp([
    `lcd ${BACKUP_DIR}`,
    'cd public_html',
    'get wp-config.php'
  ]);
  
  console.log('\n📥 Downloading wp-content...');
  console.log('(This may take a while...)\n');
  await runSftp([
    `lcd ${BACKUP_DIR}`,
    'cd public_html',
    'get -r wp-content'
  ]);
  
  console.log('\n✅ Backup complete!');
  console.log('\nBackup contents:');
}

main().catch(console.error);
