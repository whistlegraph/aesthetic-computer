#!/usr/bin/env node
// Derive addresses from mnemonics and check balances across all chains.
// Usage: node derive-addresses.mjs [path-to-wallets.json]

import { readFileSync } from 'fs';
import { ethers } from 'ethers';
import { Keypair } from '@solana/web3.js';
import { derivePath } from 'ed25519-hd-key';
import * as bip39 from 'bip39';
import * as bitcoin from 'bitcoinjs-lib';
import BIP32Factory from 'bip32';
import ecc from '@bitcoinerlab/secp256k1';

bitcoin.initEccLib(ecc);
const bip32 = BIP32Factory(ecc);

const walletsPath = process.argv[2] || '../aesthetic-computer-vault/wallets/wallets.json';
const data = JSON.parse(readFileSync(walletsPath, 'utf8'));

console.log('\n╔══════════════════════════════════════════════════════════════╗');
console.log('║  🏦 Multi-Chain Wallet Dashboard                            ║');
console.log('╚══════════════════════════════════════════════════════════════╝\n');

const results = [];

for (const [id, w] of Object.entries(data.wallets)) {
  let address = w.address || '';

  // Derive address if missing
  if (!address && w.mnemonic) {
    try {
      if (w.chain === 'ethereum') {
        // MetaMask derivation: m/44'/60'/0'/0/0 (first 5 accounts)
        const hdNode = ethers.HDNodeWallet.fromMnemonic(
          ethers.Mnemonic.fromPhrase(w.mnemonic),
          "m/44'/60'/0'/0"
        );
        const addrs = [];
        for (let i = 0; i < 5; i++) {
          addrs.push(hdNode.deriveChild(i).address);
        }
        address = addrs[0];
        w._allAddresses = addrs;
      } else if (w.chain === 'solana') {
        const seed = bip39.mnemonicToSeedSync(w.mnemonic);
        const derived = derivePath("m/44'/501'/0'/0'", seed.toString('hex'));
        const kp = Keypair.fromSeed(derived.key);
        address = kp.publicKey.toBase58();
      } else if (w.chain === 'bitcoin') {
        const seed = bip39.mnemonicToSeedSync(w.mnemonic);
        // Taproot (p2tr) - used by most ordinals wallets
        const root = bip32.fromSeed(seed);
        const child = root.derivePath("m/86'/0'/0'/0/0");
        const p2tr = bitcoin.payments.p2tr({
          internalPubkey: child.publicKey.slice(1, 33),
          network: bitcoin.networks.bitcoin,
        });
        address = p2tr.address;
        // Also derive legacy segwit for reference
        const legacyChild = root.derivePath("m/84'/0'/0'/0/0");
        const p2wpkh = bitcoin.payments.p2wpkh({
          pubkey: legacyChild.publicKey,
          network: bitcoin.networks.bitcoin,
        });
        w._segwit = p2wpkh.address;
      } else if (w.chain === 'cardano') {
        address = '(needs cardano-serialization-lib to derive)';
      }
    } catch (e) {
      address = `(error: ${e.message})`;
    }
  } else if (!address && w.private_key && w.chain === 'ethereum') {
    try {
      const wallet = new ethers.Wallet(w.private_key);
      address = wallet.address;
    } catch (e) {
      address = `(error: ${e.message})`;
    }
  }

  results.push({ id, ...w, address });
}

// Group by chain
const chains = {};
for (const r of results) {
  if (!chains[r.chain]) chains[r.chain] = [];
  chains[r.chain].push(r);
}

// Check balances
async function checkBalances() {
  for (const [chain, wallets] of Object.entries(chains)) {
    console.log(`\n🔗 ${chain.toUpperCase()}`);
    console.log('─'.repeat(60));

    for (const w of wallets) {
      let balance = '—';

      if (w.address && !w.address.startsWith('(')) {
        try {
          if (chain === 'tezos') {
            const resp = await fetch(`https://api.tzkt.io/v1/accounts/${w.address}`);
            if (resp.ok) {
              const d = await resp.json();
              balance = `${(d.balance / 1e6).toFixed(4)} XTZ`;
            }
          } else if (chain === 'ethereum') {
            // Check all derived addresses for balance
            const addrsToCheck = w._allAddresses || [w.address];
            const balances = [];
            for (const addr of addrsToCheck) {
              const rpcResp = await fetch('https://eth.public-rpc.com/', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ jsonrpc: '2.0', method: 'eth_getBalance', params: [addr, 'latest'], id: 1 })
              });
              if (rpcResp.ok) {
                const rpcData = await rpcResp.json();
                const bal = Number(BigInt(rpcData.result || '0x0')) / 1e18;
                balances.push({ addr, bal });
              }
            }
            const total = balances.reduce((s, b) => s + b.bal, 0);
            balance = `${total.toFixed(6)} ETH`;
            // Show individual accounts with balances
            w._balanceDetails = balances;
          } else if (chain === 'solana') {
            const resp = await fetch('https://api.mainnet-beta.solana.com', {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ jsonrpc: '2.0', id: 1, method: 'getBalance', params: [w.address] })
            });
            if (resp.ok) {
              const d = await resp.json();
              if (d.result?.value !== undefined) {
                balance = `${(d.result.value / 1e9).toFixed(6)} SOL`;
              }
            }
          } else if (chain === 'bitcoin') {
            // Check both taproot and segwit addresses
            const addrs = [w.address, w._segwit].filter(Boolean);
            let totalSats = 0;
            for (const addr of addrs) {
              const resp = await fetch(`https://blockstream.info/api/address/${addr}`);
              if (resp.ok) {
                const d = await resp.json();
                totalSats += (d.chain_stats?.funded_txo_sum || 0) - (d.chain_stats?.spent_txo_sum || 0);
              }
            }
            balance = `${(totalSats / 1e8).toFixed(8)} BTC`;
          }
        } catch (e) {
          balance = `(error: ${e.message?.slice(0, 40)})`;
        }
      }

      const domain = w.domain ? ` (${w.domain})` : '';
      const name = w.name || w.id;
      console.log(`  📛 ${name}${domain}`);
      console.log(`  📍 ${w.address || '(no address)'}`);
      if (w._segwit) console.log(`  📍 ${w._segwit} (segwit)`);
      console.log(`  💰 ${balance}`);
      if (w._balanceDetails?.length > 1) {
        for (let i = 0; i < w._balanceDetails.length; i++) {
          const d = w._balanceDetails[i];
          if (d.bal > 0) console.log(`     [${i}] ${d.addr}: ${d.bal.toFixed(6)} ETH`);
        }
      }
      if (w._allAddresses?.length > 1) {
        console.log(`  🔑 Accounts: ${w._allAddresses.join(', ')}`);
      }
      console.log(`  📝 ${w.role}`);
      console.log('');
    }
  }
}

await checkBalances();
