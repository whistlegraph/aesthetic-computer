const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");
const { MichelsonMap } = require("@taquito/taquito");
const fs = require("fs");
require('dotenv').config({ path: '../aesthetic-computer-vault/tezos/kidlisp/.env' });

const GHOSTNET_RPC = "https://ghostnet.ecadinfra.com";
const PRIVATE_KEY = process.env.PRIVATE_KEY;

async function deploy() {
  try {
    console.log("=" + "=".repeat(68) + "=");
    console.log("🚀 Deploying FA2 NFT to Ghostnet");
    console.log("=" + "=".repeat(68) + "=");

    // Read contract file - CORRECTED VERSION with proper ledger structure
  const contractCode = fs.readFileSync(
    "/workspaces/aesthetic-computer/tezos/keeps_fa2_corrected_compiled.tz",
    "utf-8"
  );    console.log("\n📋 Configuration:");
    console.log(`  🌐 Network: Ghostnet`);
    console.log(`  🔗 RPC: ${GHOSTNET_RPC}`);
    console.log(`  👤 Wallet: tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC`);
    console.log(`  📝 Contract: FA2 NFT`);
    console.log(`  📝 Contract size: ${contractCode.length} bytes`);

    // Initialize Tezos client
    console.log("\n🔐 Connecting to Ghostnet...");
    const Tezos = new TezosToolkit(GHOSTNET_RPC);
    const signer = new InMemorySigner(PRIVATE_KEY);
    Tezos.setSignerProvider(signer);

    const address = await signer.publicKeyHash();
    console.log(`✅ Connected as: ${address}`);

    // Create storage for FA2 with TZIP-16 metadata
    const metadataMap = new MichelsonMap();
    metadataMap.set("", Buffer.from("tezos-storage:content").toString("hex"));
    metadataMap.set("content", Buffer.from(JSON.stringify({
      name: "Aesthetic Computer Keeps",
      description: "FA2 NFT contract for aesthetic.computer",
      version: "1.0.0",
      interfaces: ["TZIP-012", "TZIP-016"],
      authors: ["aesthetic.computer"],
      homepage: "https://aesthetic.computer"
    })).toString("hex"));
    
    const storage = {
      administrator: "tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC",
      ledger: new MichelsonMap(),
      metadata: metadataMap,
      next_token_id: 0,
      operators: new MichelsonMap(),
      token_metadata: new MichelsonMap()
    };

    console.log("\n📡 Originating contract...");
    console.log("⏳ This may take a moment...");

    const originationOp = await Tezos.contract.originate({
      code: contractCode,
      storage: storage,
    });

    console.log(`\n✅ Operation submitted!`);
    console.log(`📍 Hash: ${originationOp.hash}`);

    // Wait for confirmation
    console.log(`\n⏳ Waiting for confirmation (30-60 seconds)...`);
    const confirmation = await originationOp.confirmation();

    if (confirmation) {
      const contractAddress = originationOp.contractAddress;
      console.log(`\n🎉 SUCCESS!`);
      console.log(`📍 Contract Address: ${contractAddress}`);
      console.log(`\n🔗 View on TzKT: https://ghostnet.tzkt.io/${contractAddress}`);
      console.log(`\n✅ Deployment complete!`);

      // Save address to file
      fs.writeFileSync(
        "/workspaces/aesthetic-computer/tezos/DEPLOYED_CONTRACT.txt",
        `Contract Address: ${contractAddress}\nDeployment Hash: ${originationOp.hash}\nDeployed at: ${new Date().toISOString()}\n`
      );

      console.log(
        `📄 Address saved to: /workspaces/aesthetic-computer/tezos/DEPLOYED_CONTRACT.txt`
      );
    }
  } catch (error) {
    console.error(`\n❌ Deployment failed:`);
    console.error(error.message);
    if (error.errors) {
      console.error("Details:", error.errors);
    }
    process.exit(1);
  }
}

deploy();
