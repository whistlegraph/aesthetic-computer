const { TezosToolkit } = require('@taquito/taquito');
const { InMemorySigner } = require('@taquito/signer');
const fs = require('fs');
const path = require('path');
const archiver = require('archiver');
const FormData = require('form-data');
const fetch = require('node-fetch');
require('dotenv').config({ path: '../aesthetic-computer-vault/tezos/kidlisp/.env' });
require('dotenv').config({ path: '../aesthetic-computer-vault/.env.pinata' });

const RPC_URL = 'https://ghostnet.ecadinfra.com';
const CONTRACT_ADDRESS = 'KT1EMgN12CGoXRnj82dLUs57MKJZLnbK4EBS';
const PRIVATE_KEY = process.env.PRIVATE_KEY;
const PINATA_API_KEY = process.env.PINATA_API_KEY;
const PINATA_API_SECRET = process.env.PINATA_API_SECRET;
const IPFS_GATEWAY = process.env.IPFS_GATEWAY || 'https://ipfs.aesthetic.computer/';

// Helper to convert string to bytes
function stringToBytes(str) {
  return Buffer.from(str, 'utf8').toString('hex');
}

async function uploadToIPFS(acUrl, pieceName) {
  console.log('\n📦 Creating IPFS package...');
  
  // Fetch the piece code from aesthetic.computer at build time
  console.log(`   📥 Fetching $${pieceName} code from aesthetic.computer...`);
  const codeResponse = await fetch(`https://aesthetic.computer/api/store-kidlisp?code=${pieceName}`);
  const codeData = await codeResponse.json();
  const pieceCode = codeData.source;
  
  console.log(`   ✓ Got ${pieceCode.length} chars of KidLisp code`);
  
  // Fetch CSS at build time
  console.log('   📥 Fetching CSS...');
  const cssResponse = await fetch('https://aesthetic.computer/aesthetic.computer/style.css');
  const cssContent = await cssResponse.text();
  
  // Create a minimal KidLisp runtime that can execute the code
  // This will fetch from relative URLs (allowed by CSP 'self')
  const htmlContent = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no">
    <title>aesthetic.computer - $${pieceName}</title>
    <style>${cssContent}</style>
</head>
<body class="native-cursor">
    <canvas id="canvas"></canvas>
    <script type="module">
        // Load piece code from relative URL (CSP allows 'self')
        const pieceResponse = await fetch('./piece.json');
        const pieceData = await pieceResponse.json();
        console.log('Loaded piece:', pieceData);
        
        // TODO: Implement minimal KidLisp interpreter or use aesthetic.computer API
        // For now, just show we can load data through CSP
        document.body.innerHTML = '<pre style="color: white; padding: 20px;">' + 
            'Piece Code:\\n' + pieceData.code + '</pre>';
    </script>
</body>
</html>`;

  // Create piece data file
  const pieceDataContent = JSON.stringify({
    name: pieceName,
    code: pieceCode
  });




  // Create temporary directory structure
  const tempDir = path.join(__dirname, 'ipfs-temp', pieceName);
  if (!fs.existsSync(tempDir)) {
    fs.mkdirSync(tempDir, { recursive: true });
  }

  const htmlPath = path.join(tempDir, 'index.html');
  fs.writeFileSync(htmlPath, htmlContent);

  console.log(`   ✓ Created: ${htmlPath}`);
  console.log('📤 Uploading directory to Pinata...');

  // Create a readable stream for the directory
  const FormData = require('form-data');
  const formData = new FormData();
  
  // Add the index.html file
  formData.append('file', fs.createReadStream(htmlPath), {
    filepath: 'index.html'
  });
  
  const metadata = JSON.stringify({
    name: pieceName,
    keyvalues: {
      source: 'aesthetic.computer',
      type: 'fa2-token',
      platform: 'tezos',
      marketplace: 'objkt'
    }
  });
  
  formData.append('pinataMetadata', metadata);
  formData.append('pinataOptions', JSON.stringify({ 
    cidVersion: 1,
    wrapWithDirectory: true  // This creates a directory on IPFS
  }));

  const response = await fetch('https://api.pinata.cloud/pinning/pinFileToIPFS', {
    method: 'POST',
    headers: {
      'pinata_api_key': PINATA_API_KEY,
      'pinata_secret_api_key': PINATA_API_SECRET,
      ...formData.getHeaders()
    },
    body: formData
  });

  if (!response.ok) {
    const errorText = await response.text();
    throw new Error(`Pinata upload failed: ${errorText}`);
  }

  const result = await response.json();
  const ipfsHash = result.IpfsHash;
  
  console.log(`   ✓ IPFS Hash: ${ipfsHash}`);
  
  // Cleanup
  fs.unlinkSync(htmlPath);
  fs.rmdirSync(tempDir);
  
  return {
    cid: ipfsHash,
    ipfs_uri: `ipfs://${ipfsHash}`,
    gateway_url: `${IPFS_GATEWAY}ipfs/${ipfsHash}`,
    gateway_uri_for_metadata: `ipfs://${ipfsHash}`,  // Use ipfs:// for metadata
    size: result.PinSize || 0
  };
}

async function mint() {
  console.log('\n🎨 Minting aesthetic.computer Keep to Ghostnet with IPFS...\n');

  // Configuration
  const acUrl = 'https://aesthetic.computer/$ceo';
  const pieceName = 'ceo';
  const tokenName = 'Ghostnet Test Keep - CEO';
  const description = 'Testing IPFS upload workflow for aesthetic.computer Keeps on Ghostnet';
  const contentType = 'kidlisp';

  // Upload to IPFS first
  const ipfsResult = await uploadToIPFS(acUrl, pieceName);
  
  console.log('\nIPFS Upload Complete:');
  console.log(`  CID: ${ipfsResult.cid}`);
  console.log(`  Gateway URL: ${ipfsResult.gateway_url}`);
  console.log(`  Size: ${ipfsResult.size} bytes`);

  // Now mint the token
  const Tezos = new TezosToolkit(RPC_URL);
  Tezos.setProvider({ signer: await InMemorySigner.fromSecretKey(PRIVATE_KEY) });

  const owner = await Tezos.signer.publicKeyHash();
  console.log('\n📝 Minting token...');
  console.log('Owner:', owner);
  console.log('Contract:', CONTRACT_ADDRESS);

  const contract = await Tezos.contract.at(CONTRACT_ADDRESS);

  // TZIP-21 compliant metadata with ALL required fields
  // Use ipfs:// URIs for better compatibility with marketplaces
  const metadata = {
    name: stringToBytes(tokenName),
    description: stringToBytes(description),
    artifactUri: stringToBytes(ipfsResult.ipfs_uri),  // ipfs://CID format
    displayUri: stringToBytes(ipfsResult.ipfs_uri),
    thumbnailUri: stringToBytes(`${acUrl}?thumbnail=true`),
    decimals: stringToBytes('0'),
    symbol: stringToBytes('KEEP'),
    isBooleanAmount: stringToBytes('true'),
    shouldPreferSymbol: stringToBytes('false'),
    
    // Additional TZIP-21 fields as JSON
    formats: stringToBytes(JSON.stringify([{
      uri: ipfsResult.ipfs_uri,  // ipfs://CID format
      mimeType: 'application/x-directory',
      dimensions: { value: 'responsive', unit: 'viewport' }
    }])),
    
    tags: stringToBytes(JSON.stringify([contentType, 'aesthetic.computer', 'interactive'])),
    
    attributes: stringToBytes(JSON.stringify([
      { name: 'content_type', value: contentType },
      { name: 'piece_name', value: pieceName },
      { name: 'ac_url', value: acUrl },
      { name: 'content_hash', value: ipfsResult.cid }
    ])),
    
    creators: stringToBytes(JSON.stringify([owner])),
    rights: stringToBytes('© All rights reserved'),
    
    // aesthetic.computer specific fields
    content_type: stringToBytes(contentType),
    content_hash: stringToBytes(ipfsResult.cid),
    metadata_uri: stringToBytes(acUrl),
  };

  try {
    const op = await contract.methods.keep(
      metadata.name,
      metadata.description,
      metadata.artifactUri,
      metadata.displayUri,
      metadata.thumbnailUri,
      metadata.decimals,
      metadata.symbol,
      metadata.isBooleanAmount,
      metadata.shouldPreferSymbol,
      metadata.formats,
      metadata.tags,
      metadata.attributes,
      metadata.creators,
      metadata.rights,
      metadata.content_type,
      metadata.content_hash,
      metadata.metadata_uri,
      owner
    ).send();

    console.log('Operation hash:', op.hash);
    console.log('\n⏳ Waiting for confirmation...');
    
    await op.confirmation(1);
    console.log('✅ Token minted!');

    // Get token ID
    console.log('\n⏳ Waiting 10 seconds for indexing...');
    await new Promise(resolve => setTimeout(resolve, 10000));
    
    const response = await fetch(`https://api.ghostnet.tzkt.io/v1/contracts/${CONTRACT_ADDRESS}/bigmaps/token_metadata/keys`);
    const data = await response.json();
    
    if (data.length > 0) {
      const lastToken = data[data.length - 1];
      const tokenId = lastToken.key;
      console.log(`\n🎉 Success! Token ID: ${tokenId}`);
      console.log(`\n🔗 View on Objkt:`);
      console.log(`   https://ghostnet.objkt.com/asset/${CONTRACT_ADDRESS}/${tokenId}`);
      console.log(`\n🔗 View on TzKT:`);
      console.log(`   https://ghostnet.tzkt.io/KT1AUZQoMQCNLyNh6SforcHDAhPe8KwV3GrH/tokens/${tokenId}`);
    }
    
  } catch (error) {
    console.error('❌ Minting failed:', error.message);
    if (error.errors) {
      console.error('Errors:', JSON.stringify(error.errors, null, 2));
    }
    throw error;
  }
}

mint()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
