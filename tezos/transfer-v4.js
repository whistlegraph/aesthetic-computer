const { TezosToolkit } = require("@taquito/taquito");
const { InMemorySigner } = require("@taquito/signer");
require('dotenv').config({ path: '../aesthetic-computer-vault/tezos/kidlisp/.env' });

const GHOSTNET_RPC = "https://ghostnet.ecadinfra.com";
const PRIVATE_KEY = process.env.PRIVATE_KEY;
const CONTRACT_ADDRESS = "KT1SFyh9C9qqZCC2izMj3BJMs3gUuur4Hy8K";

async function transfer() {
  const Tezos = new TezosToolkit(GHOSTNET_RPC);
  Tezos.setSignerProvider(new InMemorySigner(PRIVATE_KEY));
  
  const address = await Tezos.signer.publicKeyHash();
  console.log(`🔄 Transferring token to different address...`);
  
  const contract = await Tezos.contract.at(CONTRACT_ADDRESS);
  
  const op = await contract.methods.transfer([
    {
      from_: address,
      txs: [{
        to_: "tz1burnburnburnburnburnburnburjAYjjX",
        token_id: 0,
        amount: 1
      }]
    }
  ]).send();
  
  console.log(`✅ Transfer: ${op.hash}`);
  await op.confirmation(1);
  console.log(`✅ Confirmed`);
}

transfer().catch(console.error);
