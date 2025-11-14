const fs = require('fs');
const path = require('path');
const archiver = require('archiver');
const FormData = require('form-data');
const fetch = require('node-fetch');
require('dotenv').config({ path: '../aesthetic-computer-vault/.env.pinata' });

const PINATA_API_KEY = process.env.PINATA_API_KEY;
const PINATA_API_SECRET = process.env.PINATA_API_SECRET;

async function uploadToIPFS(acSlug) {
  console.log(`\n📦 Creating IPFS package for ${acSlug}...`);
  console.log(`   Loading aesthetic.computer piece directly (no iframe)`);
  
  // Create bootstrap HTML that loads the piece directly from aesthetic.computer
  const htmlContent = `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>aesthetic.computer</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        body {
            background: black;
            overflow: hidden;
        }
        #console {
            display: none;
        }
    </style>
    <script>
      // Set up piece to load - aesthetic.computer will fetch everything live
      window.acSTARTING_PIECE = "${acSlug.replace(/^\$/, '')}"; // Strip $ prefix for regular pieces
      
      // SPIDER MODE: Leeches resources live from aesthetic.computer while running from IPFS
      // - Skips Auth0 and other network-dependent features
      // - Loads pieces, fonts, and assets directly from aesthetic.computer
      // - Creates a "living, breathing" NFT that evolves with the system
      window.acSPIDER = true;
    </script>
    <script
      crossorigin="anonymous"
      src="https://aesthetic.computer/aesthetic.computer/boot.mjs"
      type="module"
      defer
    ></script>
    <link
      rel="stylesheet"
      crossorigin="anonymous"
      href="https://aesthetic.computer/aesthetic.computer/style.css"
    />
</head>
<body class="native-cursor embed nogap">
    <div id="console">booting...</div>
</body>
</html>`;

  // Create temporary directory structure
  const tempDir = path.join(__dirname, 'ipfs-temp');
  if (!fs.existsSync(tempDir)) {
    fs.mkdirSync(tempDir, { recursive: true });
  }

  const htmlPath = path.join(tempDir, 'index.html');
  fs.writeFileSync(htmlPath, htmlContent);

  console.log('   ✓ Created index.html');

  // Create FormData with directory
  const form = new FormData();
  
  // Add the file with proper path structure
  form.append('file', fs.createReadStream(htmlPath), {
    filepath: 'index.html'
  });

  // Pinata options for directory wrapping
  const metadata = JSON.stringify({
    name: `aesthetic.computer-${acSlug}`,
  });
  form.append('pinataMetadata', metadata);

  const options = JSON.stringify({
    wrapWithDirectory: true,
  });
  form.append('pinataOptions', options);

  console.log('   📤 Uploading to IPFS via Pinata...');

  const response = await fetch('https://api.pinata.cloud/pinning/pinFileToIPFS', {
    method: 'POST',
    headers: {
      'pinata_api_key': PINATA_API_KEY,
      'pinata_secret_api_key': PINATA_API_SECRET,
      ...form.getHeaders(),
    },
    body: form,
  });

  if (!response.ok) {
    const error = await response.text();
    throw new Error(`Pinata upload failed: ${error}`);
  }

  const result = await response.json();
  const ipfsCid = result.IpfsHash;

  console.log(`   ✅ Uploaded to IPFS!`);
  console.log(`   CID: ${ipfsCid}`);
  console.log(`   Gateway URL: https://ipfs.aesthetic.computer/ipfs/${ipfsCid}`);
  console.log(`   IPFS URI: ipfs://${ipfsCid}\n`);

  // Cleanup
  fs.unlinkSync(htmlPath);
  fs.rmdirSync(tempDir);

  return {
    cid: ipfsCid,
    gatewayUrl: `https://ipfs.aesthetic.computer/ipfs/${ipfsCid}`,
    ipfsUri: `ipfs://${ipfsCid}`,
  };
}

// Run if called directly
if (require.main === module) {
  const slug = process.argv[2] || '$wwi';
  
  uploadToIPFS(slug)
    .then((result) => {
      console.log('✅ IPFS upload complete!');
      console.log(`   Use this URI: ${result.ipfsUri}`);
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Error:', error.message);
      process.exit(1);
    });
}

module.exports = { uploadToIPFS };
