const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");
require('dotenv').config({ path: '../aesthetic-computer-vault/tezos/kidlisp/.env' });

const GHOSTNET_RPC = "https://ghostnet.ecadinfra.com";
const PRIVATE_KEY = process.env.PRIVATE_KEY;
const CONTRACT_ADDRESS = "KT1S1sXpFiV4GGxLM3zWX4cDLVEhVp9yuD7b";

async function transferToDifferentAddress() {
  try {
    console.log("🔄 Transferring to different address to trigger TzKT detection\n");

    const Tezos = new TezosToolkit(GHOSTNET_RPC);
    const signer = new InMemorySigner(PRIVATE_KEY);
    Tezos.setSignerProvider(signer);

    const from = await signer.publicKeyHash();
    // Transfer to a burn address
    const to = "tz1burnburnburnburnburnburnburjAYjjX";
    
    console.log(`From: ${from}`);
    console.log(`To: ${to}`);
    console.log(`Token ID: 0\n`);

    const contract = await Tezos.contract.at(CONTRACT_ADDRESS);
    
    const op = await contract.methods.transfer([
      {
        from_: from,
        txs: [{
          to_: to,
          token_id: 0,
          amount: 1
        }]
      }
    ]).send();

    console.log(`✅ Transfer submitted: ${op.hash}`);
    console.log("⏳ Waiting for confirmation...\n");
    
    await op.confirmation();
    
    console.log("🎉 Transfer to different address confirmed!");
    console.log(`🔗 https://ghostnet.tzkt.io/${op.hash}`);
    console.log("\n⏳ TzKT may take a few minutes to recognize the contract as FA2...");

  } catch (error) {
    console.error("❌ Error:", error.message);
    process.exit(1);
  }
}

transferToDifferentAddress();
