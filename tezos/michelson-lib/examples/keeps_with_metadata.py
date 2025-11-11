"""
Example: Build a complete Keeps contract with metadata management

This demonstrates how to use the modular library to build a full-featured
NFT contract for aesthetic.computer with:
- keep (custom minting)
- update_metadata (modify token metadata)
- freeze_metadata (make metadata immutable)
"""

import sys
sys.path.insert(0, '..')

from lib.builder import ContractBuilder
from lib.entrypoints.keep import entrypoint as keep_ep
from lib.entrypoints.update_metadata import entrypoint as update_metadata_ep
from lib.entrypoints.freeze_metadata import entrypoint as freeze_metadata_ep


def build_keeps_with_metadata_management():
    """
    Build a Keeps contract with metadata management features
    
    Features:
    1. keep - Mint new aesthetic.computer NFTs
    2. update_metadata - Update token metadata (admin only)
    3. freeze_metadata - Make metadata immutable (admin only)
    """
    
    print("🔨 Building Keeps Contract with Metadata Management")
    print("=" * 60)
    
    # Create builder
    builder = ContractBuilder()
    
    # Add entrypoints
    print("\n📦 Adding entrypoints:")
    print("  ✓ keep - Custom minting for aesthetic.computer")
    builder.add_entrypoint(keep_ep())
    
    print("  ✓ update_metadata - Modify token metadata")
    builder.add_entrypoint(update_metadata_ep())
    
    print("  ✓ freeze_metadata - Make metadata immutable")
    builder.add_entrypoint(freeze_metadata_ep())
    
    # Build contract
    print("\n🏗️  Building Michelson contract...")
    contract = builder.build()
    
    # Save to file
    output_file = "../../keeps-modular.tz"
    builder.save(output_file)
    
    print(f"\n✅ Contract saved to: {output_file}")
    print(f"📏 Contract size: {len(contract)} characters")
    print("\n" + "=" * 60)
    print("Contract structure:")
    print("=" * 60)
    
    # Show contract structure
    lines = contract.split('\n')
    for i, line in enumerate(lines[:30], 1):  # First 30 lines
        print(f"{i:3d}: {line}")
    
    if len(lines) > 30:
        print(f"... ({len(lines) - 30} more lines)")
    
    print("\n" + "=" * 60)
    print("✨ Contract Features:")
    print("=" * 60)
    print("""
🎨 keep(ac_url, content_hash, content_type, metadata_uri, owner)
   - Mint new aesthetic.computer NFTs
   - Validates content_type (kidlisp, tape, painting)
   - Auto-generates TZIP-21 metadata
   - Admin or owner can mint

📝 update_metadata(token_id, metadata_map)
   - Update token metadata after minting
   - Only admin can call
   - Useful for fixing metadata or adding new fields
   
🔒 freeze_metadata(token_id)
   - Make token metadata immutable
   - Only admin can call
   - Cannot be reversed
   - Adds __frozen flag to token_info
    """)
    
    print("=" * 60)
    print("🚀 Next Steps:")
    print("=" * 60)
    print("""
1. Test on Ghostnet:
   cd ../..
   python deploy-keeps-ghostnet.py
   
2. Mint a token:
   python mint-keep.py
   
3. Update metadata:
   python update-token-metadata.py <token_id> <key> <value>
   
4. Freeze metadata:
   python freeze-token-metadata.py <token_id>
   
5. Deploy to mainnet when ready:
   python deploy-keeps-mainnet.py
    """)
    
    return contract


if __name__ == "__main__":
    build_keeps_with_metadata_management()
