const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");
require('dotenv').config({ path: '../aesthetic-computer-vault/tezos/kidlisp/.env' });

const GHOSTNET_RPC = "https://ghostnet.ecadinfra.com";
const PRIVATE_KEY = process.env.PRIVATE_KEY;
const CONTRACT_ADDRESS = "KT1S1sXpFiV4GGxLM3zWX4cDLVEhVp9yuD7b";

async function mint() {
  try {
    console.log("=" + "=".repeat(68) + "=");
    console.log("🎨 Minting Test Token on FA2 Contract");
    console.log("=" + "=".repeat(68) + "=");

    console.log("\n📋 Configuration:");
    console.log(`  🌐 Network: Ghostnet`);
    console.log(`  📍 Contract: ${CONTRACT_ADDRESS}`);
    console.log(`  👤 Wallet: tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC`);

    // Initialize Tezos client
    console.log("\n🔐 Connecting to Ghostnet...");
    const Tezos = new TezosToolkit(GHOSTNET_RPC);
    const signer = new InMemorySigner(PRIVATE_KEY);
    Tezos.setSignerProvider(signer);

    const address = await signer.publicKeyHash();
    console.log(`✅ Connected as: ${address}`);

    // Get the contract
    console.log(`\n📍 Loading contract: ${CONTRACT_ADDRESS}`);
    const contract = await Tezos.contract.at(CONTRACT_ADDRESS);
    console.log("✅ Contract loaded");

    // Mint a token using the 'keep' entrypoint
    console.log("\n🎨 Minting new token...");

    const mintParams = {
      ac_url: "ipfs://Qm1234567890",
      content_hash: "sha256:abcd1234",
      content_type: "painting", // Must be "painting" or "tape"
      metadata_uri: "ipfs://QmMetadata",
      owner: address
    };

    const op = await contract.methods.keep(
      mintParams.ac_url,
      mintParams.content_hash,
      mintParams.content_type,
      mintParams.metadata_uri,
      mintParams.owner
    ).send();

    console.log(`\n✅ Operation submitted!`);
    console.log(`📍 Hash: ${op.hash}`);

    // Wait for confirmation
    console.log(`\n⏳ Waiting for confirmation...`);
    const confirmation = await op.confirmation();

    if (confirmation) {
      console.log(`\n🎉 Token minted successfully!`);
      console.log(`\n📊 Mint Parameters:`);
      console.log(`  AC URL: ${mintParams.ac_url}`);
      console.log(`  Content Hash: ${mintParams.content_hash}`);
      console.log(`  Content Type: ${mintParams.content_type}`);
      console.log(`  Metadata URI: ${mintParams.metadata_uri}`);
      console.log(`  Owner: ${mintParams.owner}`);
      console.log(`\n🔗 View on TzKT: https://ghostnet.tzkt.io/${CONTRACT_ADDRESS}`);
    }
  } catch (error) {
    console.error(`\n❌ Minting failed:`);
    console.error(error.message);
    if (error.errors) {
      console.error("Details:", error.errors);
    }
    process.exit(1);
  }
}

mint();
