#!/usr/bin/env node

/**
 * Rebake $air token metadata on v4 staging contract
 * This will regenerate the bundle and update on-chain metadata
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import https from 'https';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// V4 staging contract
const V4_CONTRACT = 'KT1ER1GyoeRNhkv6E57yKbBbEKi5ynKbaH3W';
const TOKEN_ID = 3; // $air
const PIECE_CODE = 'air';

// Load staging wallet credentials
const stagingEnvPath = path.join(__dirname, 'staging/.env');
const envContent = fs.readFileSync(stagingEnvPath, 'utf8');
let stagingAddress, stagingKey;
for (const line of envContent.split('\n')) {
  if (line.startsWith('STAGING_ADDRESS=') || line.startsWith('ADDRESS=')) {
    stagingAddress = line.split('=')[1].trim().replace(/"/g, '');
  } else if (line.startsWith('STAGING_KEY=') || line.startsWith('KEY=') || line.startsWith('SECRET_KEY=')) {
    stagingKey = line.split('=')[1].trim().replace(/"/g, '');
  }
}

if (!stagingAddress || !stagingKey) {
  console.error('❌ Could not load staging wallet credentials');
  process.exit(1);
}

const tezos = new TezosToolkit('https://mainnet.api.tez.ie');
tezos.setProvider({ signer: new InMemorySigner(stagingKey) });

console.log('\n╔══════════════════════════════════════════════════════════════╗');
console.log('║  🔄 Rebake $air Metadata                                     ║');
console.log('╚══════════════════════════════════════════════════════════════╝\n');

console.log(`📍 Contract: ${V4_CONTRACT}`);
console.log(`🎨 Token: #${TOKEN_ID} ($${PIECE_CODE})`);
console.log(`👤 Admin: ${stagingAddress}\n`);

// Fetch new bundle from bundle-html API
console.log('📦 Generating fresh bundle...');
const bundleUrl = `https://aesthetic.computer/api/bundle-html?code=${PIECE_CODE}&format=json`;

function fetchUrl(url) {
  return new Promise((resolve, reject) => {
    https.get(url, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        if (res.statusCode === 200) {
          resolve(JSON.parse(data));
        } else {
          reject(new Error(`HTTP ${res.statusCode}: ${data}`));
        }
      });
    }).on('error', reject);
  });
}

const bundleData = await fetchUrl(bundleUrl);
console.log(`   ✓ Bundle generated: ${bundleData.sizeKB} KB`);
console.log(`   ✓ Filename: ${bundleData.filename}\n`);

// Upload to IPFS via Pinata
console.log('📤 Uploading to IPFS...');

// Load Pinata credentials
const pinataEnvPath = path.join(__dirname, '..', 'aesthetic-computer-vault', '.env.pinata');
const pinataContent = fs.readFileSync(pinataEnvPath, 'utf8');
let pinataKey, pinataSecret;
for (const line of pinataContent.split('\n')) {
  if (line.startsWith('PINATA_API_KEY=')) {
    pinataKey = line.split('=')[1].trim().replace(/"/g, '');
  } else if (line.startsWith('PINATA_API_SECRET=')) {
    pinataSecret = line.split('=')[1].trim().replace(/"/g, '');
  }
}

function uploadToPinata(content, filename, pinataApiKey, pinataApiSecret) {
  return new Promise((resolve, reject) => {
    const boundary = '----WebKitFormBoundary' + Math.random().toString(36);
    const bodyParts = [
      `--${boundary}\r\n`,
      `Content-Disposition: form-data; name="file"; filename="${filename}"\r\n`,
      'Content-Type: text/html\r\n\r\n',
      Buffer.from(content, 'base64').toString('utf8'),
      `\r\n--${boundary}--\r\n`
    ];
    const body = bodyParts.join('');

    const options = {
      hostname: 'api.pinata.cloud',
      path: '/pinning/pinFileToIPFS',
      method: 'POST',
      headers: {
        'Content-Type': `multipart/form-data; boundary=${boundary}`,
        'Content-Length': Buffer.byteLength(body),
        'pinata_api_key': pinataApiKey,
        'pinata_secret_api_key': pinataApiSecret
      }
    };

    const req = https.request(options, (res) => {
      let data = '';
      res.on('data', chunk => data += chunk);
      res.on('end', () => {
        if (res.statusCode === 200) {
          resolve(JSON.parse(data));
        } else {
          reject(new Error(`Pinata upload failed: ${res.statusCode} ${data}`));
        }
      });
    });

    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

const pinataResult = await uploadToPinata(bundleData.content, bundleData.filename, pinataKey, pinataSecret);
const ipfsHash = pinataResult.IpfsHash;
const ipfsUri = `ipfs://${ipfsHash}`;

console.log(`   ✓ Uploaded: ${ipfsUri}\n`);

// Build updated token_info with new artifact URI
console.log('📝 Building updated metadata...');

function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

const tokenInfo = {
  'name': stringToBytes(`$${PIECE_CODE}`),
  'description': stringToBytes(bundleData.sourceCode || ''),
  'artifactUri': stringToBytes(ipfsUri),
  'displayUri': stringToBytes(ipfsUri),
  'thumbnailUri': stringToBytes(ipfsUri), // Will use existing thumbnail
  'decimals': stringToBytes('0'),
  'symbol': stringToBytes('KEEP'),
  'isBooleanAmount': stringToBytes('true'),
  'shouldPreferSymbol': stringToBytes('false'),
  '': stringToBytes(ipfsUri) // metadata URI
};

console.log('📤 Calling edit_metadata...');

try {
  const contract = await tezos.contract.at(V4_CONTRACT);

  const op = await contract.methods.edit_metadata(TOKEN_ID, tokenInfo).send();

  console.log(`   ⏳ Operation hash: ${op.hash}`);
  console.log('   ⏳ Waiting for confirmation...');

  await op.confirmation(1);

  console.log('\n✅ Metadata updated!');
  console.log(`   🔗 Explorer: https://tzkt.io/${op.hash}`);
  console.log(`   🎨 View on objkt: https://objkt.com/tokens/${V4_CONTRACT}/${TOKEN_ID}\n`);

} catch (error) {
  console.error('\n❌ Update failed!');
  console.error(`   Error: ${error.message}\n`);
  process.exit(1);
}
