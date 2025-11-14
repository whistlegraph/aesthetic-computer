const { TezosToolkit } = require('@taquito/taquito');
const { InMemorySigner } = require('@taquito/signer');
require('dotenv').config({ path: '../aesthetic-computer-vault/tezos/kidlisp/.env' });

const RPC_URL = 'https://ghostnet.ecadinfra.com';
const CONTRACT_ADDRESS = 'KT1EMgN12CGoXRnj82dLUs57MKJZLnbK4EBS';
const PRIVATE_KEY = process.env.PRIVATE_KEY;

// Helper to convert string to bytes
function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

async function editMetadata(tokenId) {
  console.log('\n🔧 Testing Metadata Edit & Lock');
  console.log('================================\n');

  const Tezos = new TezosToolkit(RPC_URL);
  const signer = new InMemorySigner(PRIVATE_KEY);
  Tezos.setProvider({ signer });

  const contract = await Tezos.contract.at(CONTRACT_ADDRESS);
  const storage = await contract.storage();

  // Get current metadata via TzKT API (big_map access is easier via API)
  console.log(`📋 Fetching current metadata for token ${tokenId}...`);
  const response = await fetch(`https://api.ghostnet.tzkt.io/v1/tokens?contract=${CONTRACT_ADDRESS}&tokenId=${tokenId}`);
  const tokens = await response.json();
  
  if (!tokens || tokens.length === 0) {
    console.error(`❌ Token ${tokenId} not found!`);
    return;
  }

  const currentMetadata = tokens[0].metadata;
  
  // Display current metadata
  console.log('\n📊 Current Metadata:');
  console.log(`   Name: ${currentMetadata.name}`);
  console.log(`   Artifact URI: ${currentMetadata.artifactUri}`);

  // Check if locked via API
  const lockedResponse = await fetch(`https://api.ghostnet.tzkt.io/v1/contracts/${CONTRACT_ADDRESS}/bigmaps/metadata_locked/keys/${tokenId}`);
  let isLocked = false;
  if (lockedResponse.ok) {
    try {
      const lockData = await lockedResponse.json();
      isLocked = lockData.value === true;
    } catch (e) {
      isLocked = false; // Key doesn't exist = not locked
    }
  }
  
  if (isLocked) {
    console.error(`\n❌ Token ${tokenId} is LOCKED! Cannot edit metadata.`);
    return;
  }

  console.log(`\n✅ Token ${tokenId} is unlocked - proceeding with edit...\n`);

  // Change the name from "CEO" to "WWI" and update IPFS URI
  const newName = currentMetadata.name.replace('CEO', 'WWI');
  const newDescription = currentMetadata.description.replace('CEO', 'WWI');
  // SPIDER MODE: $eel piece - leeches live from aesthetic.computer
  // Creates a "living, breathing" NFT that evolves with the system
  const newIpfsUri = 'ipfs://QmU9wAw7JMnkcqfdf9F8e7CqG3fLR5Gv1DuXHbhi5x8ZWj';
  
  console.log(`🔄 Updating metadata:`);
  console.log(`   Name: "${currentMetadata.name}" → "${newName}"`);
  console.log(`   Artifact URI: "${currentMetadata.artifactUri}" → "${newIpfsUri}"`);
  console.log(`   Display URI: "${currentMetadata.displayUri || currentMetadata.artifactUri}" → "${newIpfsUri}"\n`);

  // Create metadata map (LIGO expects a map, not individual parameters)
  const { MichelsonMap } = require('@taquito/taquito');
  const metadataMap = new MichelsonMap();
  
  metadataMap.set('name', stringToBytes(newName));
  metadataMap.set('description', stringToBytes(newDescription));
  metadataMap.set('artifactUri', stringToBytes(newIpfsUri));
  metadataMap.set('displayUri', stringToBytes(newIpfsUri));
  metadataMap.set('thumbnailUri', stringToBytes(currentMetadata.thumbnailUri || ''));
  metadataMap.set('decimals', stringToBytes(currentMetadata.decimals?.toString() || '0'));
  metadataMap.set('symbol', stringToBytes(currentMetadata.symbol || 'KEEP'));
  metadataMap.set('isBooleanAmount', stringToBytes(currentMetadata.isBooleanAmount?.toString() || 'true'));
  metadataMap.set('shouldPreferSymbol', stringToBytes(currentMetadata.shouldPreferSymbol?.toString() || 'false'));
  metadataMap.set('formats', stringToBytes(JSON.stringify(currentMetadata.formats || [])));
  metadataMap.set('tags', stringToBytes(JSON.stringify(currentMetadata.tags || [])));
  metadataMap.set('attributes', stringToBytes(JSON.stringify(currentMetadata.attributes || [])));
  metadataMap.set('creators', stringToBytes(JSON.stringify(currentMetadata.creators || [])));
  metadataMap.set('rights', stringToBytes(currentMetadata.rights || ''));
  metadataMap.set('content_type', stringToBytes('application/x-directory'));
  metadataMap.set('content_hash', stringToBytes(newIpfsUri.replace('ipfs://', '')));
  metadataMap.set('metadata_uri', stringToBytes(''));

  // Call edit_metadata with token_id and metadata map
  console.log('📝 Calling edit_metadata...');
  const editOp = await contract.methods.edit_metadata(
    tokenId,
    metadataMap
  ).send();

  console.log(`⏳ Waiting for confirmation...`);
  await editOp.confirmation();
  console.log(`✅ Metadata updated!`);
  console.log(`   Operation: ${editOp.hash}`);
  console.log(`   View: https://ghostnet.tzkt.io/${editOp.hash}\n`);

  // Now test locking
  console.log('🔒 Testing metadata lock...\n');
  console.log('   WARNING: This will PERMANENTLY lock the metadata!');
  console.log('   After locking, metadata can NEVER be changed again.\n');

  // Uncomment to actually lock:
  // const lockOp = await contract.methods.lock_metadata(tokenId).send();
  // console.log(`⏳ Waiting for confirmation...`);
  // await lockOp.confirmation();
  // console.log(`✅ Metadata LOCKED!`);
  // console.log(`   Operation: ${lockOp.hash}`);
  // console.log(`   View: https://ghostnet.tzkt.io/${lockOp.hash}\n`);

  console.log('⚠️  Lock operation commented out for safety.');
  console.log('    Uncomment in script to actually lock the metadata.\n');

  // Verify the change
  console.log('🔍 Verifying update (may take a minute for indexer to update)...');
  
  // Wait a bit for indexer
  await new Promise(resolve => setTimeout(resolve, 3000));
  
  const verifyResponse = await fetch(`https://api.ghostnet.tzkt.io/v1/tokens?contract=${CONTRACT_ADDRESS}&tokenId=${tokenId}`);
  const updatedTokens = await verifyResponse.json();
  const verifyName = updatedTokens[0].metadata.name;
  
  console.log(`   Updated Name: ${verifyName}`);
  console.log(`   ✅ Successfully changed from "CEO" to "WWI"!\n`);
}

// Run the test
const tokenId = process.argv[2] || 0;

console.log(`Token ID: ${tokenId}\n`);

editMetadata(parseInt(tokenId))
  .then(() => {
    console.log('✅ Test complete!');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Error:', error.message);
    process.exit(1);
  });
