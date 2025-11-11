#!/usr/bin/env python3
"""
Upload aesthetic.computer content to IPFS via Pinata

Creates a simple HTML iframe wrapper and uploads as ZIP to IPFS.
Returns the IPFS CID for use in FA2 token metadata.

Usage:
    python upload-to-ipfs.py "https://aesthetic.computer/kidlisp~example"
    python upload-to-ipfs.py "https://aesthetic.computer/!tape~recording"
    python upload-to-ipfs.py "https://aesthetic.computer/painting~abstract"
"""

import sys
import os
import json
import requests
import zipfile
from pathlib import Path
from dotenv import load_dotenv

# Load Pinata credentials from vault
vault_env = Path(__file__).parent.parent / "aesthetic-computer-vault" / ".env.pinata"
load_dotenv(vault_env)

PINATA_API_KEY = os.getenv("PINATA_API_KEY")
PINATA_API_SECRET = os.getenv("PINATA_API_SECRET")
PINATA_JWT = os.getenv("PINATA_JWT")
IPFS_GATEWAY = os.getenv("IPFS_GATEWAY", "https://ipfs.aesthetic.computer/")

if not PINATA_API_KEY or not PINATA_API_SECRET:
    print("❌ Error: Pinata credentials not found in vault/.env.pinata")
    sys.exit(1)


def create_iframe_html(ac_url: str) -> str:
    """Create a simple HTML page with fullscreen iframe to aesthetic.computer URL"""
    
    # Add token=true query param
    iframe_url = ac_url
    if '?' in iframe_url:
        iframe_url += '&token=true'
    else:
        iframe_url += '?token=true'
    
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>aesthetic.computer</title>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}
        html, body {{
            width: 100%;
            height: 100%;
            overflow: hidden;
        }}
        iframe {{
            width: 100%;
            height: 100%;
            border: none;
        }}
    </style>
</head>
<body>
    <iframe src="{iframe_url}" allow="camera; microphone; clipboard-write; clipboard-read"></iframe>
</body>
</html>"""


def create_zip(ac_url: str, output_dir: Path) -> Path:
    """
    Create a ZIP file with index.html containing the iframe
    
    Returns path to the created ZIP file
    """
    # Extract piece name from URL for filename
    piece_name = ac_url.split('aesthetic.computer/')[-1].split('?')[0].replace('/', '-')
    zip_path = output_dir / f"{piece_name}.zip"
    
    # Create HTML content
    html_content = create_iframe_html(ac_url)
    
    # Create ZIP file
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        zipf.writestr('index.html', html_content)
    
    return zip_path


def upload_to_pinata(zip_path: Path) -> dict:
    """
    Upload ZIP file to Pinata/IPFS
    
    Returns dict with CID and IPFS URLs
    """
    url = "https://api.pinata.cloud/pinning/pinFileToIPFS"
    
    headers = {
        "pinata_api_key": PINATA_API_KEY,
        "pinata_secret_api_key": PINATA_API_SECRET
    }
    
    # Prepare file for upload
    with open(zip_path, 'rb') as f:
        files = {
            'file': (zip_path.name, f, 'application/zip')
        }
        
        # Optional: Add metadata to help identify the pin
        metadata = json.dumps({
            "name": zip_path.stem,
            "keyvalues": {
                "source": "aesthetic.computer",
                "type": "objkt-package",
                "platform": "tezos",
                "marketplace": "objkt"
            }
        })
        
        data = {
            "pinataMetadata": metadata,
            "pinataOptions": json.dumps({"cidVersion": 1})
        }
        
        print(f"📤 Uploading {zip_path.name} to Pinata...")
        response = requests.post(url, headers=headers, files=files, data=data)
    
    if response.status_code != 200:
        raise Exception(f"Pinata upload failed: {response.text}")
    
    result = response.json()
    ipfs_hash = result["IpfsHash"]
    
    return {
        "cid": ipfs_hash,
        "ipfs_uri": f"ipfs://{ipfs_hash}",
        "gateway_url": f"{IPFS_GATEWAY}ipfs/{ipfs_hash}",
        "size": result.get("PinSize", 0)
    }


def main():
    if len(sys.argv) < 2:
        print("Usage: python upload-to-ipfs.py <aesthetic.computer-url>")
        print("\nExample:")
        print('  python upload-to-ipfs.py "https://aesthetic.computer/kidlisp~example"')
        sys.exit(1)
    
    ac_url = sys.argv[1]
    
    # Validate URL
    if "aesthetic.computer" not in ac_url:
        print("❌ Error: URL must be an aesthetic.computer URL")
        sys.exit(1)
    
    print("=" * 70)
    print("🎨 Aesthetic Computer → IPFS Upload")
    print("=" * 70)
    print(f"\n📍 Source URL: {ac_url}\n")
    
    # Create temp directory for ZIP
    temp_dir = Path(__file__).parent / "ipfs-temp"
    temp_dir.mkdir(exist_ok=True)
    
    try:
        # Create ZIP with iframe wrapper
        print("📦 Creating ZIP package...")
        zip_path = create_zip(ac_url, temp_dir)
        print(f"   ✓ Created: {zip_path}")
        print(f"   ✓ Size: {zip_path.stat().st_size:,} bytes")
        
        # Upload to Pinata
        result = upload_to_pinata(zip_path)
        
        print("\n" + "=" * 70)
        print("✅ Upload Successful!")
        print("=" * 70)
        print(f"\n📍 IPFS CID: {result['cid']}")
        print(f"🔗 IPFS URI: {result['ipfs_uri']}")
        print(f"🌐 Gateway URL: {result['gateway_url']}")
        print(f"📦 Pin Size: {result['size']:,} bytes")
        
        print("\n" + "=" * 70)
        print("🎯 Use in FA2 Token Metadata:")
        print("=" * 70)
        print(f"""
{{
  "artifactUri": "{result['gateway_url']}",
  "displayUri": "{result['gateway_url']}",
  ...
}}

Or with IPFS URI:
{{
  "artifactUri": "{result['ipfs_uri']}",
  "displayUri": "{result['ipfs_uri']}",
  ...
}}
""")
        
        # Save result to JSON for scripting
        result_file = temp_dir / f"{zip_path.stem}-ipfs.json"
        with open(result_file, 'w') as f:
            json.dump({
                "ac_url": ac_url,
                "zip_file": str(zip_path),
                **result
            }, f, indent=2)
        
        print(f"💾 Result saved to: {result_file}")
        print("\n" + "=" * 70)
        
        return result
        
    except Exception as e:
        print(f"\n❌ Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
