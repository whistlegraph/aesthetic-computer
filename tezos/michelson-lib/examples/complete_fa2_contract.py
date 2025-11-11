#!/usr/bin/env python3
"""
Complete FA2 NFT Contract Example

Demonstrates building a full FA2-compliant contract with:
- Custom keep (mint) entrypoint
- Metadata management (update & freeze)
- Standard FA2 entrypoints (transfer, balance_of, update_operators)
"""

import sys
import os
sys.path.insert(0, os.path.dirname(__file__) + '/..')

from lib.builder import ContractBuilder
from lib.entrypoints.keep import entrypoint as keep_ep
from lib.entrypoints.update_metadata import entrypoint as update_metadata_ep
from lib.entrypoints.freeze_metadata import entrypoint as freeze_metadata_ep
from lib.entrypoints.transfer import entrypoint as transfer_ep
from lib.entrypoints.balance_of import entrypoint as balance_of_ep
from lib.entrypoints.update_operators import entrypoint as update_operators_ep


def main():
    """Build complete FA2 NFT contract"""
    
    print("=" * 70)
    print("Building Complete FA2 NFT Contract")
    print("=" * 70)
    print()
    
    # Initialize builder
    builder = ContractBuilder()
    
    # Add custom entrypoints
    print("Adding custom entrypoints...")
    builder.add_entrypoint(keep_ep())
    builder.add_entrypoint(update_metadata_ep())
    builder.add_entrypoint(freeze_metadata_ep())
    print("  ✓ keep (custom minting)")
    print("  ✓ update_metadata (admin-only)")
    print("  ✓ freeze_metadata (make immutable)")
    print()
    
    # Add FA2 standard entrypoints
    print("Adding FA2 standard entrypoints...")
    builder.add_entrypoint(transfer_ep())
    builder.add_entrypoint(balance_of_ep())
    builder.add_entrypoint(update_operators_ep())
    print("  ✓ transfer (batch transfers)")
    print("  ✓ balance_of (balance queries)")
    print("  ✓ update_operators (operator permissions)")
    print()
    
    # Build contract
    print("Generating Michelson code...")
    contract = builder.build()
    print(f"  ✓ Contract size: {len(contract)} characters")
    print()
    
    # Save to file
    output_file = "keeps-fa2-complete.tz"
    builder.save(output_file)
    print(f"  ✓ Saved to: {output_file}")
    print()
    
    # Show contract preview
    print("Contract Preview (first 40 lines):")
    print("-" * 70)
    lines = contract.split('\n')
    for line in lines[:40]:
        print(line)
    if len(lines) > 40:
        print(f"... ({len(lines) - 40} more lines)")
    print("-" * 70)
    print()
    
    # Show entrypoint summary
    print("Entrypoint Summary:")
    print("-" * 70)
    print("Custom Entrypoints:")
    print("  • keep - Mint NFTs with content_type validation (kidlisp, tape, painting)")
    print("  • update_metadata - Update token metadata (admin only)")
    print("  • freeze_metadata - Make token metadata immutable (admin only)")
    print()
    print("FA2 Standard Entrypoints:")
    print("  • transfer - Transfer tokens with operator support")
    print("  • balance_of - Query token balances via callback")
    print("  • update_operators - Manage operator permissions")
    print("-" * 70)
    print()
    
    # Deployment instructions
    print("Next Steps:")
    print("1. Test contract on Ghostnet:")
    print("   octez-client originate contract keeps \\")
    print(f"     transferring 0 from <account> \\")
    print(f"     --init '(Pair (Pair \"<admin>\" {{}}) (Pair 0 (Pair {{}} {{}})))' \\")
    print(f"     --burn-cap 1.0")
    print()
    print("2. Mint a token:")
    print("   octez-client transfer 0 from <account> to keeps \\")
    print("     --entrypoint keep \\")
    print("     --arg '(Pair (Pair \"https://...\" \"hash\") (Pair \"kidlisp\" (Pair \"ipfs://...\" \"<owner>\")))'")
    print()
    print("3. Transfer a token:")
    print("   octez-client transfer 0 from <account> to keeps \\")
    print("     --entrypoint transfer \\")
    print("     --arg '{Pair \"<from>\" {Pair \"<to>\" (Pair 0 1)}}'")
    print()
    print(f"✓ Complete FA2 contract ready: {output_file}")


if __name__ == "__main__":
    main()
