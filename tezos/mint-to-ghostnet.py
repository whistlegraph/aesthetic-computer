#!/usr/bin/env python3
"""
Mint aesthetic.computer content as FA2 token on Ghostnet

Complete workflow:
1. Fetch KidLisp source code (if applicable)
2. Upload iframe wrapper to IPFS
3. Mint token on Ghostnet with on-chain metadata

Usage:
    python mint-to-ghostnet.py "https://aesthetic.computer/\$ceo"
    python mint-to-ghostnet.py "https://aesthetic.computer/!tape~recording" --name "My Tape"
    python mint-to-ghostnet.py "https://aesthetic.computer/painting~abstract" --description "Cool art"
"""

import sys
import os
import json
import requests
from pathlib import Path
from datetime import datetime
from dotenv import load_dotenv
from pytezos import pytezos, Key

# Load environment from vault
vault_dir = Path(__file__).parent.parent / "aesthetic-computer-vault"
load_dotenv(vault_dir / "tezos" / "aesthetic" / ".env")
load_dotenv(vault_dir / ".env.pinata")

AESTHETIC_ADDRESS = os.getenv("AESTHETIC_ADDRESS")
PINATA_API_KEY = os.getenv("PINATA_API_KEY")
PINATA_API_SECRET = os.getenv("PINATA_API_SECRET")
IPFS_GATEWAY = os.getenv("IPFS_GATEWAY", "https://ipfs.aesthetic.computer/")

# Ghostnet configuration
GHOSTNET_RPC = "https://ghostnet.ecadinfra.com"
CONTRACT_ADDRESS = None  # Set after deployment

def extract_piece_info(ac_url: str) -> dict:
    """
    Extract piece information from aesthetic.computer URL
    
    Returns content_type, piece_name, and full URL
    """
    # Normalize URL - add https://aesthetic.computer/ if missing
    if not ac_url.startswith('http'):
        if ac_url.startswith('aesthetic.computer/'):
            ac_url = f"https://{ac_url}"
        elif ac_url.startswith('$'):
            # KidLisp piece
            ac_url = f"https://aesthetic.computer/{ac_url}"
        else:
            ac_url = f"https://aesthetic.computer/{ac_url.lstrip('/')}"
    
    url_path = ac_url.split('aesthetic.computer/')[-1]
    
    # Detect content type
    if url_path.startswith('$'):
        content_type = "kidlisp"
        piece_name = url_path.lstrip('$').split('?')[0].split('~')[0]
    elif url_path.startswith('!'):
        content_type = "tape"
        piece_name = url_path.lstrip('!').split('~')[-1].split('?')[0]
    elif url_path.startswith('#'):
        content_type = "painting"
        piece_name = url_path.lstrip('#').split('~')[-1].split('?')[0]
    else:
        content_type = "piece"
        piece_name = url_path.split('?')[0]
    
    return {
        "content_type": content_type,
        "piece_name": piece_name,
        "ac_url": ac_url
    }


def fetch_kidlisp_source(piece_name: str) -> str:
    """
    Fetch KidLisp source code from the store-kidlisp API
    
    Returns the source code or None if not found
    """
    # Clean the piece name - remove $ prefix and any path components
    clean_name = piece_name.lstrip('$').split('~')[0].lower()
    url = f"https://aesthetic.computer/.netlify/functions/store-kidlisp?code={clean_name}"
    
    print(f"   Fetching from: {url}")
    
    try:
        response = requests.get(url, timeout=10)
        if response.status_code == 200:
            data = response.json()
            source = data.get('source', '')
            if source:
                return source
        return None
    except Exception as e:
        print(f"   ⚠️  Error: {e}")
        return None


def upload_to_ipfs(ac_url: str) -> dict:
    """
    Upload iframe wrapper ZIP to IPFS via Pinata
    
    Returns dict with CID and URLs
    """
    import zipfile
    
    temp_dir = Path(__file__).parent / "ipfs-temp"
    temp_dir.mkdir(exist_ok=True)
    
    print("📦 Creating IPFS package...")
    
    # Add token=true query param
    iframe_url = ac_url
    if '?' in iframe_url:
        iframe_url += '&token=true'
    else:
        iframe_url += '?token=true'
    
    print(f"   iframe URL: {iframe_url}")
    
    # Create HTML iframe wrapper
    html_content = f"""<!DOCTYPE html>
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
    
    # Create ZIP
    piece_name = ac_url.split('aesthetic.computer/')[-1].split('?')[0].replace('/', '-')
    zip_path = temp_dir / f"{piece_name}.zip"
    
    with zipfile.ZipFile(zip_path, 'w', zipfile.ZIP_DEFLATED) as zipf:
        zipf.writestr('index.html', html_content)
    
    print(f"   ✓ Created: {zip_path}")
    
    # Upload to Pinata
    print("📤 Uploading to Pinata...")
    url = "https://api.pinata.cloud/pinning/pinFileToIPFS"
    
    headers = {
        "pinata_api_key": PINATA_API_KEY,
        "pinata_secret_api_key": PINATA_API_SECRET
    }
    
    with open(zip_path, 'rb') as f:
        files = {
            'file': (zip_path.name, f, 'application/zip')
        }
        
        metadata = json.dumps({
            "name": zip_path.stem,
            "keyvalues": {
                "source": "aesthetic.computer",
                "type": "fa2-token",
                "platform": "tezos",
                "marketplace": "objkt"
            }
        })
        
        data = {
            "pinataMetadata": metadata,
            "pinataOptions": json.dumps({"cidVersion": 1})
        }
        
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


def create_token_metadata(
    piece_info: dict,
    ipfs_result: dict,
    name: str = None,
    description: str = None,
    kidlisp_source: str = None
) -> dict:
    """
    Create TZIP-21 compliant token metadata
    
    This metadata will be stored ON-CHAIN in the token_metadata big_map
    """
    metadata = {
        "name": name or f"{piece_info['piece_name']}",
        "description": description or f"aesthetic.computer {piece_info['content_type']}",
        "decimals": "0",  # NFT
        "isBooleanAmount": "true",  # NFT
        "shouldPreferSymbol": "false",
        "artifactUri": ipfs_result['gateway_url'],
        "displayUri": ipfs_result['gateway_url'],
        "thumbnailUri": f"{piece_info['ac_url']}?thumbnail=true",
        "tags": [piece_info['content_type'], "aesthetic.computer", "interactive"],
        "attributes": [
            {"name": "content_type", "value": piece_info['content_type']},
            {"name": "piece_name", "value": piece_info['piece_name']},
            {"name": "ac_url", "value": piece_info['ac_url']},
            {"name": "created", "value": datetime.utcnow().isoformat()},
        ],
        "formats": [
            {
                "uri": ipfs_result['gateway_url'],
                "mimeType": "application/zip",
                "dimensions": {"value": "responsive", "unit": "viewport"}
            }
        ],
        "creators": [AESTHETIC_ADDRESS],
        "rights": "© All rights reserved",
        "rightUri": piece_info['ac_url'],
    }
    
    # Add KidLisp source code to metadata if available
    if kidlisp_source and piece_info['content_type'] == 'kidlisp':
        metadata["attributes"].append({
            "name": "kidlisp_source",
            "value": kidlisp_source
        })
    
    return metadata


def metadata_to_bytes(metadata: dict) -> str:
    """
    Convert metadata dict to Michelson bytes format
    
    This is how it's stored in the token_metadata big_map
    """
    metadata_json = json.dumps(metadata, separators=(',', ':'))
    metadata_hex = metadata_json.encode('utf-8').hex()
    return f"0x{metadata_hex}"


def mint_token(
    contract_address: str,
    ac_url: str,
    name: str = None,
    description: str = None
):
    """
    Complete minting workflow:
    1. Extract piece info
    2. Fetch KidLisp source (if applicable)
    3. Upload to IPFS
    4. Mint token with on-chain metadata
    """
    
    print("=" * 70)
    print("🎨 Aesthetic Computer → Tezos FA2 Token")
    print("=" * 70)
    print(f"\n📍 Source: {ac_url}\n")
    
    # Step 1: Extract piece info
    piece_info = extract_piece_info(ac_url)
    print(f"✓ Content Type: {piece_info['content_type']}")
    print(f"✓ Piece Name: {piece_info['piece_name']}\n")
    
    # Step 2: Fetch KidLisp source if applicable
    kidlisp_source = None
    if piece_info['content_type'] == 'kidlisp':
        print("📖 Fetching KidLisp source code...")
        kidlisp_source = fetch_kidlisp_source(piece_info['piece_name'])
        if kidlisp_source:
            print(f"✓ Source: {kidlisp_source[:60]}...\n")
        else:
            print("⚠️  Source not found (will mint without source)\n")
    
    # Step 3: Upload to IPFS
    ipfs_result = upload_to_ipfs(ac_url)
    print(f"\n✓ IPFS CID: {ipfs_result['cid']}")
    print(f"✓ Gateway URL: {ipfs_result['gateway_url']}\n")
    
    # Step 4: Create metadata
    print("📝 Creating token metadata...")
    metadata = create_token_metadata(
        piece_info, ipfs_result, name, description, kidlisp_source
    )
    
    print("\n" + "=" * 70)
    print("📋 Token Metadata (On-Chain):")
    print("=" * 70)
    print(json.dumps(metadata, indent=2))
    
    # Step 5: Mint (would require contract deployment first)
    print("\n" + "=" * 70)
    print("🚀 Ready to Mint!")
    print("=" * 70)
    
    if not contract_address:
        print("\n⚠️  No contract deployed yet. Deploy the FA2 contract first:")
        print("   python deploy-to-ghostnet.py\n")
        
        # Save mint parameters for later
        mint_params_file = Path(__file__).parent / "ipfs-temp" / f"{piece_info['piece_name']}-mint-params.json"
        mint_params = {
            "ac_url": ac_url,
            "content_hash": ipfs_result['cid'],
            "content_type": piece_info['content_type'],
            "metadata_uri": f"ipfs://{ipfs_result['cid']}",
            "owner": AESTHETIC_ADDRESS,
            "metadata": metadata,
            "metadata_bytes": metadata_to_bytes(metadata),
            "kidlisp_source": kidlisp_source,
        }
        
        with open(mint_params_file, 'w') as f:
            json.dump(mint_params, f, indent=2)
        
        print(f"💾 Mint parameters saved to: {mint_params_file}")
        print("\nMichelson keep() parameters:")
        print("=" * 70)
        print(f"(Pair (Pair \"{ac_url}\" \"{ipfs_result['cid']}\") (Pair \"{piece_info['content_type']}\" (Pair \"ipfs://{ipfs_result['cid']}\" \"{AESTHETIC_ADDRESS}\")))")
        
    else:
        print(f"\n🎯 Minting to contract: {contract_address}")
        print("   (Minting code would go here)")
    
    return {
        "piece_info": piece_info,
        "ipfs_result": ipfs_result,
        "metadata": metadata,
        "kidlisp_source": kidlisp_source
    }


def main():
    import argparse
    
    parser = argparse.ArgumentParser(description="Mint aesthetic.computer content to Ghostnet")
    parser.add_argument("url", help="aesthetic.computer URL")
    parser.add_argument("--name", help="Token name (default: piece name)")
    parser.add_argument("--description", help="Token description")
    parser.add_argument("--contract", help="FA2 contract address (if deployed)")
    
    args = parser.parse_args()
    
    global CONTRACT_ADDRESS
    CONTRACT_ADDRESS = args.contract
    
    if not AESTHETIC_ADDRESS:
        print("❌ Error: AESTHETIC_ADDRESS not found in vault/tezos/aesthetic/.env")
        sys.exit(1)
    
    if not PINATA_API_KEY:
        print("❌ Error: Pinata credentials not found in vault/.env.pinata")
        sys.exit(1)
    
    try:
        result = mint_token(CONTRACT_ADDRESS, args.url, args.name, args.description)
        print("\n✅ Preparation complete!")
        
    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
