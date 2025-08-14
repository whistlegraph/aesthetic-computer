#!/usr/bin/env node

/**
 * Deploy KidLisp Contract to Tezos
 * 
 * Usage:
 *   node scripts/deploy.js --network ghostnet
 *   node scripts/deploy.js --network mainnet
 */

import { TezosToolkit } from '@taquito/taquito';
import { InMemorySigner } from '@taquito/signer';
import fs from 'fs';
import path from 'path';
import dotenv from 'dotenv';

// Load environment variables
dotenv.config();

const networks = {
  ghostnet: {
    rpc: process.env.TEZOS_RPC_URL_GHOSTNET || 'https://ghostnet.ecadinfra.com',
    name: 'Ghostnet'
  },
  mainnet: {
    rpc: process.env.TEZOS_RPC_URL_MAINNET || 'https://mainnet.api.tez.ie',
    name: 'Mainnet'
  }
};

async function deployContract(network) {
  console.log(`🚀 Deploying KidLisp to ${networks[network].name}...`);
  
  // Initialize Tezos toolkit
  const tezos = new TezosToolkit(networks[network].rpc);
  
  // Set up signer with admin private key
  const privateKey = process.env.CONTRACT_ADMIN_SECRET_KEY;
  if (!privateKey) {
    throw new Error('CONTRACT_ADMIN_SECRET_KEY not found in environment');
  }
  
  tezos.setSignerProvider(new InMemorySigner(privateKey));
  
  // Load compiled contract
  const contractPath = path.join(process.cwd(), 'output', 'kidlisp.json');
  if (!fs.existsSync(contractPath)) {
    throw new Error(`Contract not found at ${contractPath}. Run 'npm run compile:contract' first.`);
  }
  
  const contractCode = JSON.parse(fs.readFileSync(contractPath, 'utf8'));
  
  // Get admin address
  const adminAddress = await tezos.signer.publicKeyHash();
  console.log(`📝 Admin address: ${adminAddress}`);
  
  // Contract metadata
  const metadata = {
    "": Buffer.from("tezos-storage:content").toString('hex'),
    "content": Buffer.from(JSON.stringify({
      "name": "KidLisp",
      "description": "FA2 fungible tokens for aesthetic.computer KidLisp code snippets",
      "version": "1.0.0",
      "homepage": "https://aesthetic.computer",
      "license": "MIT",
      "authors": ["aesthetic.computer"],
      "source": {
        "tools": ["SmartPy"],
        "location": "https://github.com/whistlegraph/aesthetic-computer/tree/main/tezos"
      }
    })).toString('hex')
  };
  
  try {
    console.log('📦 Originating contract...');
    
    const operation = await tezos.contract.originate({
      code: contractCode.michelson,
      storage: {
        // Initialize with admin and metadata
        administrator: adminAddress,
        metadata: metadata,
        // Add other required FA2 storage fields
        ledger: {},
        token_metadata: {},
        operators: {},
        // KidLisp specific fields
        creators: {},
        code_hashes: {},
        hash_to_token: {},
        royalty_percentage: parseInt(process.env.CREATOR_ROYALTY_PERCENTAGE || '500'),
        next_token_id: 0,
        minting_fee: parseInt(process.env.MINTING_FEE || '100000'),
        contract_metadata: metadata
      }
    });
    
    console.log('⏳ Waiting for confirmation...');
    await operation.confirmation();
    
    const contractAddress = operation.contractAddress;
    console.log(`✅ Contract deployed successfully!`);
    console.log(`📍 Contract address: ${contractAddress}`);
    console.log(`🔗 Operation hash: ${operation.hash}`);
    
    // Save deployment info
    const deploymentInfo = {
      network: networks[network].name,
      contractAddress,
      adminAddress,
      deploymentHash: operation.hash,
      timestamp: new Date().toISOString(),
      config: {
        royalty_percentage: process.env.CREATOR_ROYALTY_PERCENTAGE,
        minting_fee: process.env.MINTING_FEE,
        coin_name: process.env.KIDLISP_COIN_NAME,
        coin_symbol: process.env.KIDLISP_COIN_SYMBOL
      }
    };
    
    const deploymentFile = path.join(process.cwd(), `deployment-${network}.json`);
    fs.writeFileSync(deploymentFile, JSON.stringify(deploymentInfo, null, 2));
    console.log(`💾 Deployment info saved to ${deploymentFile}`);
    
    // Update .env file with contract address
    const envFile = path.join(process.cwd(), '.env');
    let envContent = '';
    if (fs.existsSync(envFile)) {
      envContent = fs.readFileSync(envFile, 'utf8');
    }
    
    const contractAddressVar = `KIDLISP_CONTRACT_ADDRESS_${network.toUpperCase()}=${contractAddress}`;
    
    if (envContent.includes(`KIDLISP_CONTRACT_ADDRESS_${network.toUpperCase()}`)) {
      envContent = envContent.replace(
        new RegExp(`KIDLISP_CONTRACT_ADDRESS_${network.toUpperCase()}=.*`),
        contractAddressVar
      );
    } else {
      envContent += `\n${contractAddressVar}\n`;
    }
    
    fs.writeFileSync(envFile, envContent);
    console.log(`📝 Updated .env with contract address`);
    
    return contractAddress;
    
  } catch (error) {
    console.error('❌ Deployment failed:', error);
    throw error;
  }
}

// CLI handling
async function main() {
  const args = process.argv.slice(2);
  const networkArg = args.find(arg => arg.startsWith('--network='));
  const network = networkArg ? networkArg.split('=')[1] : 'ghostnet';
  
  if (!networks[network]) {
    console.error(`❌ Invalid network: ${network}`);
    console.error(`Available networks: ${Object.keys(networks).join(', ')}`);
    process.exit(1);
  }
  
  try {
    const contractAddress = await deployContract(network);
    console.log(`\n🎉 Deployment complete!`);
    console.log(`Next steps:`);
    console.log(`1. Verify the contract on a block explorer`);
    console.log(`2. Test the contract with 'npm run test'`);
    console.log(`3. Update store-kidlisp.js with the new contract address`);
    
  } catch (error) {
    console.error(`\n💥 Deployment failed:`, error.message);
    process.exit(1);
  }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  main();
}

export { deployContract };
