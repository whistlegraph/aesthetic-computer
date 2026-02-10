#!/usr/bin/env node

/**
 * Simple XTZ transfer between wallets
 * Usage: node transfer.mjs <from-wallet> <to-address> <amount-in-xtz>
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const CONFIG = {
  mainnet: {
    name: 'Mainnet',
    rpc: 'https://mainnet.api.tez.ie',
    explorer: 'https://tzkt.io'
  },
  ghostnet: {
    name: 'Ghostnet (Testnet)',
    rpc: 'https://ghostnet.ecadinfra.com',
    explorer: 'https://ghostnet.tzkt.io'
  }
};

function loadCredentials(wallet) {
  const walletPaths = {
    kidlisp: { path: 'kidlisp/.env', addressKey: 'KIDLISP_ADDRESS', secretKey: 'KIDLISP_KEY' },
    aesthetic: { path: 'kidlisp/.env', addressKey: 'AESTHETIC_ADDRESS', secretKey: 'AESTHETIC_KEY' },
    staging: { path: 'staging/.env', addressKey: 'STAGING_ADDRESS', secretKey: 'STAGING_KEY' }
  };

  const walletConfig = walletPaths[wallet];
  if (!walletConfig) {
    throw new Error(`Unknown wallet: ${wallet}. Use: kidlisp, aesthetic, or staging`);
  }

  const envPath = path.join(__dirname, walletConfig.path);
  if (!fs.existsSync(envPath)) {
    throw new Error(`Wallet credentials not found: ${envPath}`);
  }

  const credentials = {};
  const content = fs.readFileSync(envPath, 'utf8');
  for (const line of content.split('\n')) {
    if (line.startsWith(walletConfig.addressKey + '=') || line.startsWith('ADDRESS=')) {
      credentials.address = line.split('=')[1].trim().replace(/"/g, '');
    } else if (line.startsWith(walletConfig.secretKey + '=') || line.startsWith('KEY=') || line.startsWith('SECRET_KEY=')) {
      credentials.secretKey = line.split('=')[1].trim().replace(/"/g, '');
    }
  }

  if (!credentials.address || !credentials.secretKey) {
    throw new Error(`Invalid credentials in ${envPath}`);
  }

  return credentials;
}

async function transfer(fromWallet, toAddress, amountXTZ, network = 'mainnet') {
  console.log('\n╔══════════════════════════════════════════════════════════════╗');
  console.log('║  💸 XTZ Transfer                                             ║');
  console.log('╚══════════════════════════════════════════════════════════════╝\n');

  const credentials = loadCredentials(fromWallet);
  const config = CONFIG[network];
  const tezos = new TezosToolkit(config.rpc);
  tezos.setProvider({ signer: new InMemorySigner(credentials.secretKey) });

  console.log(`📡 Network: ${config.name}`);
  console.log(`👤 From: ${credentials.address} (${fromWallet})`);
  console.log(`📥 To: ${toAddress}`);
  console.log(`💰 Amount: ${amountXTZ} XTZ\n`);

  // Check balance
  console.log('💰 Checking balance...');
  const balance = await tezos.tz.getBalance(credentials.address);
  const balanceXTZ = balance.toNumber() / 1_000_000;
  console.log(`   Current balance: ${balanceXTZ.toFixed(6)} XTZ`);

  if (balanceXTZ < amountXTZ) {
    throw new Error(`Insufficient balance. Have ${balanceXTZ.toFixed(6)} XTZ, need ${amountXTZ} XTZ`);
  }

  // Send transfer
  console.log('\n📤 Sending transfer...');
  const op = await tezos.contract.transfer({
    to: toAddress,
    amount: amountXTZ
  });

  console.log(`   ⏳ Operation hash: ${op.hash}`);
  console.log('   ⏳ Waiting for confirmation...');

  await op.confirmation(1);

  console.log('\n✅ Transfer completed!');
  console.log(`   🔗 Explorer: ${config.explorer}/${op.hash}\n`);

  // Check new balance
  const newBalance = await tezos.tz.getBalance(credentials.address);
  const newBalanceXTZ = newBalance.toNumber() / 1_000_000;
  console.log(`   New balance: ${newBalanceXTZ.toFixed(6)} XTZ\n`);
}

// Parse CLI args
const args = process.argv.slice(2);
if (args.length < 3) {
  console.error('Usage: node transfer.mjs <from-wallet> <to-address> <amount-in-xtz> [network]');
  console.error('Example: node transfer.mjs aesthetic tz1dfoQDuxjwSgxdqJnisyKUxDHweade4Gzt 1.0 mainnet');
  process.exit(1);
}

const [fromWallet, toAddress, amountStr, network = 'mainnet'] = args;
const amount = parseFloat(amountStr);

transfer(fromWallet, toAddress, amount, network).catch(err => {
  console.error('\n❌ Transfer failed!');
  console.error(`   Error: ${err.message}\n`);
  process.exit(1);
});
