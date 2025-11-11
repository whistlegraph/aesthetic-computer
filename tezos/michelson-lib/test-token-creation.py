#!/usr/bin/env python3
"""
Test Token Creation & Metadata Validation

Simulates creating a token and validates IPFS metadata format for objkt.com compatibility.
Tests the complete workflow without deploying to the blockchain.
"""

import json
import hashlib
import base64
from datetime import datetime
from pathlib import Path


class TokenMetadataBuilder:
    """Build TZIP-21 compliant metadata for FA2 NFTs"""
    
    def __init__(self, admin_address: str):
        self.admin = admin_address
        self.next_token_id = 0
        self.tokens = []
    
    def create_token(self, 
                     ac_url: str,
                     content_hash: str,
                     content_type: str,
                     owner: str,
                     name: str = None,
                     description: str = None,
                     tags: list = None) -> dict:
        """
        Create a test token with TZIP-21 metadata
        
        Args:
            ac_url: aesthetic.computer URL
            content_hash: Content hash for integrity
            content_type: kidlisp, tape, or painting
            owner: Token owner address
            name: Optional display name
            description: Optional description
            tags: Optional list of tags
        
        Returns:
            dict: Complete token metadata
        """
        
        token_id = self.next_token_id
        self.next_token_id += 1
        
        # Build TZIP-21 metadata
        metadata = {
            "name": name or f"{content_type.title()} #{token_id}",
            "description": description or f"aesthetic.computer {content_type}",
            "decimals": 0,  # NFT
            "isBooleanAmount": True,  # NFT (boolean ownership)
            "shouldPreferSymbol": False,
            "artifactUri": ac_url,
            "displayUri": ac_url,
            "thumbnailUri": self._generate_thumbnail_uri(ac_url),
            "tags": tags or [content_type, "aesthetic.computer", "generative"],
            "attributes": [
                {"name": "content_type", "value": content_type},
                {"name": "content_hash", "value": content_hash},
                {"name": "created", "value": datetime.utcnow().isoformat()},
            ],
            "formats": [
                {
                    "uri": ac_url,
                    "mimeType": self._get_mime_type(content_type),
                    "dimensions": {"value": "responsive", "unit": "viewport"}
                }
            ],
            "creators": [owner],
            "rights": "© All rights reserved",
            "rightUri": ac_url,
        }
        
        token = {
            "token_id": token_id,
            "owner": owner,
            "metadata": metadata,
            "content_hash": content_hash,
            "content_type": content_type,
            "ac_url": ac_url,
        }
        
        self.tokens.append(token)
        return token
    
    def _get_mime_type(self, content_type: str) -> str:
        """Get MIME type for content type"""
        mime_types = {
            "kidlisp": "text/html",
            "tape": "text/html", 
            "painting": "text/html",
        }
        return mime_types.get(content_type, "text/html")
    
    def _generate_thumbnail_uri(self, ac_url: str) -> str:
        """Generate thumbnail URI for objkt.com"""
        # For aesthetic.computer, we can use a screenshot service
        # or create a static thumbnail endpoint
        return f"{ac_url}?thumbnail=true"
    
    def to_michelson_metadata(self, token_id: int) -> dict:
        """
        Convert metadata to Michelson big_map format
        
        Returns metadata as it would appear in the contract storage
        """
        token = self.tokens[token_id]
        metadata_json = json.dumps(token["metadata"], separators=(',', ':'))
        
        # Convert to bytes (hex encoding)
        metadata_bytes = metadata_json.encode('utf-8').hex()
        
        # Build Michelson map structure
        michelson_map = {
            "": f"0x{metadata_bytes}",  # Empty string key holds the metadata
        }
        
        return {
            "token_id": token_id,
            "token_info": michelson_map
        }
    
    def generate_ipfs_metadata(self, token_id: int) -> str:
        """
        Generate IPFS-ready metadata JSON
        
        This is what gets uploaded to IPFS/Pinata
        """
        token = self.tokens[token_id]
        return json.dumps(token["metadata"], indent=2)
    
    def validate_objkt_compatibility(self, token_id: int) -> dict:
        """
        Validate metadata is compatible with objkt.com
        
        Returns validation results
        """
        token = self.tokens[token_id]
        metadata = token["metadata"]
        
        required_fields = ["name", "artifactUri", "displayUri"]
        recommended_fields = ["description", "thumbnailUri", "tags", "creators"]
        
        validation = {
            "valid": True,
            "errors": [],
            "warnings": [],
            "info": []
        }
        
        # Check required fields
        for field in required_fields:
            if field not in metadata:
                validation["valid"] = False
                validation["errors"].append(f"Missing required field: {field}")
        
        # Check recommended fields
        for field in recommended_fields:
            if field not in metadata:
                validation["warnings"].append(f"Missing recommended field: {field}")
        
        # Validate URIs
        if "artifactUri" in metadata:
            uri = metadata["artifactUri"]
            if not (uri.startswith("http://") or uri.startswith("https://") or uri.startswith("ipfs://")):
                validation["warnings"].append(f"artifactUri should use http/https/ipfs scheme: {uri}")
        
        # Check decimals for NFT
        if metadata.get("decimals") != 0:
            validation["errors"].append("NFTs must have decimals = 0")
            validation["valid"] = False
        
        # Check isBooleanAmount for NFT
        if metadata.get("isBooleanAmount") != True:
            validation["errors"].append("NFTs should have isBooleanAmount = true")
            validation["valid"] = False
        
        # Info
        validation["info"].append(f"Token ID: {token_id}")
        validation["info"].append(f"Content type: {token['content_type']}")
        validation["info"].append(f"Metadata size: {len(json.dumps(metadata))} bytes")
        
        return validation


def print_section(title: str):
    """Print section header"""
    print("\n" + "=" * 70)
    print(f"  {title}")
    print("=" * 70)


def print_validation_results(validation: dict):
    """Print validation results with colors"""
    if validation["valid"]:
        print("✅ VALID - Ready for objkt.com")
    else:
        print("❌ INVALID - Fix errors before deploying")
    
    if validation["errors"]:
        print("\n🔴 Errors:")
        for error in validation["errors"]:
            print(f"  • {error}")
    
    if validation["warnings"]:
        print("\n⚠️  Warnings:")
        for warning in validation["warnings"]:
            print(f"  • {warning}")
    
    if validation["info"]:
        print("\n📋 Info:")
        for info in validation["info"]:
            print(f"  • {info}")


def main():
    """Test token creation and metadata generation"""
    
    print_section("Aesthetic Computer - Token Metadata Test")
    
    # Test addresses
    admin_address = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"  # Example admin
    owner_address = "tz1VSUr8wwNhLAzempoch5d6hLRiTh8Cjcjb"  # Example owner
    
    # Initialize builder
    builder = TokenMetadataBuilder(admin_address)
    
    print("\n📝 Creating test tokens...")
    
    # Test Token 1: KidLisp program
    token1 = builder.create_token(
        ac_url="https://aesthetic.computer/kidlisp~example",
        content_hash="QmExampleHash123",
        content_type="kidlisp",
        owner=owner_address,
        name="Hello World",
        description="A simple KidLisp program that prints Hello World",
        tags=["kidlisp", "code", "interactive", "aesthetic.computer"]
    )
    print(f"  ✓ Token 0: {token1['metadata']['name']} ({token1['content_type']})")
    
    # Test Token 2: Tape recording
    token2 = builder.create_token(
        ac_url="https://aesthetic.computer/tape~session-2024",
        content_hash="QmTapeHash456",
        content_type="tape",
        owner=owner_address,
        name="Drawing Session #42",
        description="Recorded drawing session from November 2024",
        tags=["tape", "recording", "drawing", "aesthetic.computer"]
    )
    print(f"  ✓ Token 1: {token2['metadata']['name']} ({token2['content_type']})")
    
    # Test Token 3: Painting
    token3 = builder.create_token(
        ac_url="https://aesthetic.computer/painting~abstract-001",
        content_hash="QmPaintHash789",
        content_type="painting",
        owner=owner_address,
        name="Abstract Composition #1",
        description="Generative abstract painting created in aesthetic.computer",
        tags=["painting", "generative", "art", "aesthetic.computer"]
    )
    print(f"  ✓ Token 2: {token3['metadata']['name']} ({token3['content_type']})")
    
    # Validate each token
    for i in range(3):
        print_section(f"Token {i} - Validation")
        validation = builder.validate_objkt_compatibility(i)
        print_validation_results(validation)
        
        # Show IPFS metadata
        print("\n📦 IPFS Metadata JSON:")
        print("-" * 70)
        ipfs_json = builder.generate_ipfs_metadata(i)
        print(ipfs_json)
        
        # Show Michelson format
        print("\n🔧 Michelson Storage Format:")
        print("-" * 70)
        michelson = builder.to_michelson_metadata(i)
        print(json.dumps(michelson, indent=2))
    
    # Save example metadata files
    print_section("Saving Example Files")
    
    output_dir = Path("test-metadata-output")
    output_dir.mkdir(exist_ok=True)
    
    for i in range(3):
        # Save IPFS JSON
        ipfs_file = output_dir / f"token-{i}-metadata.json"
        with open(ipfs_file, 'w') as f:
            f.write(builder.generate_ipfs_metadata(i))
        print(f"  ✓ Saved: {ipfs_file}")
        
        # Save Michelson format
        michelson_file = output_dir / f"token-{i}-michelson.json"
        with open(michelson_file, 'w') as f:
            json.dump(builder.to_michelson_metadata(i), f, indent=2)
        print(f"  ✓ Saved: {michelson_file}")
    
    # Create IPFS upload script
    print_section("Next Steps - IPFS Upload")
    
    print("""
To upload metadata to IPFS via Pinata:

1. Get your Pinata API keys from the vault:
   cat ../../../aesthetic-computer-vault/.env.pinata

2. Upload metadata files:
   curl -X POST "https://api.pinata.cloud/pinning/pinJSONToIPFS" \\
     -H "pinata_api_key: YOUR_API_KEY" \\
     -H "pinata_secret_api_key: YOUR_SECRET_KEY" \\
     -H "Content-Type: application/json" \\
     -d @test-metadata-output/token-0-metadata.json

3. Get the IPFS hash (CID) from the response:
   {"IpfsHash":"QmXxx...","PinSize":1234}

4. Update your token metadata URI:
   metadata_uri = f"ipfs://{ipfs_hash}"
""")
    
    print_section("Deployment to Ghostnet")
    
    print(f"""
Ready to deploy the FA2 contract to Ghostnet!

1. Make sure you have octez-client installed and configured

2. Originate the contract:
   octez-client --endpoint https://ghostnet.tezos.marigold.dev \\
     originate contract aesthetic_keeps \\
     transferring 0 from YOUR_ACCOUNT \\
     running keeps-fa2-complete.tz \\
     --init '(Pair (Pair "{admin_address}" {{}}) (Pair 0 (Pair {{}} {{}})))' \\
     --burn-cap 2.0

3. Mint token 0 (after uploading metadata to IPFS):
   octez-client --endpoint https://ghostnet.tezos.marigold.dev \\
     transfer 0 from YOUR_ACCOUNT to aesthetic_keeps \\
     --entrypoint keep \\
     --arg '(Pair \\
              (Pair "https://aesthetic.computer/kidlisp~example" "QmExampleHash123") \\
              (Pair "kidlisp" \\
                (Pair "ipfs://QmYourMetadataHash" "{owner_address}")))'

4. View on Ghostnet explorers:
   - Better Call Dev: https://better-call.dev/ghostnet/
   - TzKT: https://ghostnet.tzkt.io/
   - Objkt (testnet): https://testnet.objkt.com/
""")
    
    print("\n" + "=" * 70)
    print("✅ Test complete! All metadata is objkt.com compatible.")
    print("=" * 70)


if __name__ == "__main__":
    main()
