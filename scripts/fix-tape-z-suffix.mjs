#!/usr/bin/env node
// Fix tapes with Z suffix in filename

import { execSync } from 'child_process';
import { readFileSync } from 'fs';

const SPACES_KEY = execSync('grep "^SPACES_KEY=" aesthetic-computer-vault/.env | cut -d"=" -f2', { encoding: 'utf8' }).trim();
const SPACES_SECRET = execSync('grep "^SPACES_SECRET=" aesthetic-computer-vault/.env | cut -d"=" -f2', { encoding: 'utf8' }).trim();

const s3cmd = (cmd) => {
  return execSync(
    `s3cmd --access_key=${SPACES_KEY} --secret_key=${SPACES_SECRET} --host=sfo3.digitaloceanspaces.com --host-bucket='%(bucket)s.sfo3.digitaloceanspaces.com' ${cmd}`,
    { encoding: 'utf8' }
  );
};

// Find all Z.zip files in user bucket that have timestamp format (not random slug format)
const userFiles = s3cmd("ls -r s3://user-aesthetic-computer/ | grep 'Z\\.zip$'");
const timestampPattern = /\d{4}\.\d{2}\.\d{2}\.\d{2}\.\d{2}\.\d{2}\.\d{3}Z\.zip$/;

const filesToFix = userFiles.split('\n')
  .filter(line => line.trim() && timestampPattern.test(line))
  .map(line => {
    const match = line.match(/s3:\/\/.+/);
    return match ? match[0] : null;
  })
  .filter(Boolean);

console.log(`Found ${filesToFix.length} tape(s) with timestamp Z suffix to fix:\n`);

for (const file of filesToFix) {
  const fixedFile = file.replace(/Z\.zip$/, '.zip');
  console.log(`📦 ${file}`);
  console.log(`   → ${fixedFile}`);
  
  // Check if fixed version already exists
  try {
    s3cmd(`ls '${fixedFile}'`);
    console.log(`   ✅ Already has correct version, skipping\n`);
  } catch (e) {
    // Doesn't exist, need to copy
    console.log(`   🔧 Copying and setting ACL...`);
    try {
      s3cmd(`cp '${file}' '${fixedFile}'`);
      s3cmd(`setacl '${fixedFile}' --acl-public`);
      console.log(`   ✅ Fixed!\n`);
    } catch (err) {
      console.log(`   ❌ Error: ${err.message}\n`);
    }
  }
}

console.log('Done!');
