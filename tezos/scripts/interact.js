#!/usr/bin/env node

/**
 * Interact with deployed KidLisp Meme Coin contract
 * 
 * Usage:
 *   node scripts/interact.js mint --code "(wipe blue)" --creator tz1... --amount 1000000
 *   node scripts/interact.js balance --address tz1... --token 0
 *   node scripts/interact.js info --token 0
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import dotenv from 'dotenv';
import crypto from 'crypto';

dotenv.config();

const tezos = new TezosToolkit(process.env.TEZOS_RPC_URL || 'https://ghostnet.ecadinfra.com');
const contractAddress = process.env.KIDLISP_CONTRACT_ADDRESS_GHOSTNET || process.env.KIDLISP_CONTRACT_ADDRESS;

if (process.env.CONTRACT_ADMIN_SECRET_KEY) {
  tezos.setSignerProvider(new InMemorySigner(process.env.CONTRACT_ADMIN_SECRET_KEY));
}

async function mintToken(code, creator, amount = 1000000) {
  console.log(`🪙 Minting KidLisp token...`);
  console.log(`Code: ${code}`);
  console.log(`Creator: ${creator}`);
  console.log(`Amount: ${amount}`);
  
  const codeHash = crypto.createHash('sha256').update(code.trim()).digest('hex');
  console.log(`Hash: ${codeHash}`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const operation = await contract.methods.mint_kidlisp_token({
      code_hash: codeHash,
      kidlisp_code: code,
      creator: creator,
      amount: amount,
      metadata_uri: `https://aesthetic.computer/kidlisp/metadata/${codeHash}`
    }).send({
      amount: 0.1, // Minting fee
    });
    
    console.log(`⏳ Transaction submitted: ${operation.hash}`);
    await operation.confirmation();
    console.log(`✅ Token minted successfully!`);
    
    // Get token ID
    const tokenId = await contract.views.get_token_for_hash(codeHash).read();
    console.log(`🪙 Token ID: ${tokenId}`);
    
  } catch (error) {
    console.error(`❌ Minting failed:`, error.message);
  }
}

async function getBalance(address, tokenId) {
  console.log(`📊 Getting balance for ${address}, token ${tokenId}...`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    const result = await contract.views.balance_of([{
      owner: address,
      token_id: tokenId
    }]).read();
    
    const balance = result[0]?.balance || 0;
    console.log(`💰 Balance: ${balance / 1000000} KLSP`);
    
  } catch (error) {
    console.error(`❌ Balance query failed:`, error.message);
  }
}

async function getTokenInfo(tokenId) {
  console.log(`ℹ️ Getting info for token ${tokenId}...`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    
    const [creator, codeHash] = await Promise.all([
      contract.views.get_creator(tokenId).read(),
      contract.views.get_code_hash(tokenId).read()
    ]);
    
    console.log(`👤 Creator: ${creator}`);
    console.log(`🔍 Code Hash: ${codeHash}`);
    
    // Get metadata
    const storage = await contract.storage();
    const metadata = storage.token_metadata.get(tokenId.toString());
    
    if (metadata) {
      console.log(`📝 Metadata:`);
      for (const [key, value] of Object.entries(metadata)) {
        if (key === 'kidlisp_code') {
          console.log(`  ${key}: ${Buffer.from(value, 'hex').toString('utf8')}`);
        } else {
          console.log(`  ${key}: ${value}`);
        }
      }
    }
    
  } catch (error) {
    console.error(`❌ Token info query failed:`, error.message);
  }
}

async function listTokens() {
  console.log(`📋 Listing all tokens...`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    const storage = await contract.storage();
    
    const nextTokenId = storage.next_token_id;
    console.log(`🔢 Total tokens created: ${nextTokenId}`);
    
    for (let i = 0; i < nextTokenId; i++) {
      console.log(`\n🪙 Token ${i}:`);
      await getTokenInfo(i);
    }
    
  } catch (error) {
    console.error(`❌ Token listing failed:`, error.message);
  }
}

async function getContractInfo() {
  console.log(`📋 Contract Information`);
  console.log(`📍 Address: ${contractAddress}`);
  
  try {
    const contract = await tezos.contract.at(contractAddress);
    const storage = await contract.storage();
    
    console.log(`👤 Administrator: ${storage.administrator}`);
    console.log(`💰 Royalty %: ${storage.royalty_percentage / 100}%`);
    console.log(`💸 Minting Fee: ${storage.minting_fee / 1000000} tez`);
    console.log(`🔢 Next Token ID: ${storage.next_token_id}`);
    
  } catch (error) {
    console.error(`❌ Contract info query failed:`, error.message);
  }
}

// CLI handling
async function main() {
  if (!contractAddress) {
    console.error('❌ Contract address not configured. Set KIDLISP_CONTRACT_ADDRESS in .env');
    process.exit(1);
  }
  
  const args = process.argv.slice(2);
  const command = args[0];
  
  switch (command) {
    case 'mint':
      const codeIndex = args.findIndex(arg => arg === '--code');
      const creatorIndex = args.findIndex(arg => arg === '--creator');
      const amountIndex = args.findIndex(arg => arg === '--amount');
      
      if (codeIndex === -1 || creatorIndex === -1) {
        console.error('❌ Usage: mint --code "(wipe blue)" --creator tz1... [--amount 1000000]');
        process.exit(1);
      }
      
      const code = args[codeIndex + 1];
      const creator = args[creatorIndex + 1];
      const amount = amountIndex > -1 ? parseInt(args[amountIndex + 1]) : 1000000;
      
      await mintToken(code, creator, amount);
      break;
      
    case 'balance':
      const addressIndex = args.findIndex(arg => arg === '--address');
      const tokenIndex = args.findIndex(arg => arg === '--token');
      
      if (addressIndex === -1 || tokenIndex === -1) {
        console.error('❌ Usage: balance --address tz1... --token 0');
        process.exit(1);
      }
      
      const address = args[addressIndex + 1];
      const tokenId = parseInt(args[tokenIndex + 1]);
      
      await getBalance(address, tokenId);
      break;
      
    case 'info':
      const infoTokenIndex = args.findIndex(arg => arg === '--token');
      
      if (infoTokenIndex === -1) {
        console.error('❌ Usage: info --token 0');
        process.exit(1);
      }
      
      const infoTokenId = parseInt(args[infoTokenIndex + 1]);
      await getTokenInfo(infoTokenId);
      break;
      
    case 'list':
      await listTokens();
      break;
      
    case 'contract':
      await getContractInfo();
      break;
      
    default:
      console.log('KidLisp Meme Coin Contract Interaction Tool');
      console.log('');
      console.log('Usage:');
      console.log('  node scripts/interact.js mint --code "(wipe blue)" --creator tz1... [--amount 1000000]');
      console.log('  node scripts/interact.js balance --address tz1... --token 0');
      console.log('  node scripts/interact.js info --token 0');
      console.log('  node scripts/interact.js list');
      console.log('  node scripts/interact.js contract');
      console.log('');
      console.log('Environment variables:');
      console.log('  KIDLISP_CONTRACT_ADDRESS: Contract address');
      console.log('  CONTRACT_ADMIN_SECRET_KEY: Admin private key (for minting)');
      console.log('  TEZOS_RPC_URL: Tezos RPC endpoint');
      break;
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(console.error);
}
