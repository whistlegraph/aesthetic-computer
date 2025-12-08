"""
Deploy FA2 contract to Ghostnet using PyTezos (pure Python, no octez-client needed)
"""

import sys
from pathlib import Path
from pytezos import pytezos, Key

# Load kidlisp wallet credentials
vault_dir = Path(__file__).parent.parent / "aesthetic-computer-vault" / "tezos" / "kidlisp"
env_file = vault_dir / ".env"

KIDLISP_ADDRESS = None
KIDLISP_KEY = None

if env_file.exists():
    with open(env_file) as f:
        for line in f:
            if line.startswith('KIDLISP_ADDRESS='):
                KIDLISP_ADDRESS = line.split('=')[1].strip().strip('"')
            elif line.startswith('KIDLISP_KEY='):
                KIDLISP_KEY = line.split('=')[1].strip().strip('"')

if not KIDLISP_ADDRESS or not KIDLISP_KEY:
    print("❌ Error: KIDLISP credentials not found in vault/tezos/kidlisp/.env")
    sys.exit(1)

# Ghostnet configuration
GHOSTNET_RPC = "https://ghostnet.ecadinfra.com"


def deploy_contract():
    print("=" * 70)
    print("🚀 Deploying FA2 Contract to Ghostnet (PyTezos)")
    print("=" * 70)
    print()
    
    # Connect to Ghostnet with kidlisp key
    print("🔧 Connecting to Ghostnet...")
    ptz = pytezos.using(shell=GHOSTNET_RPC, key=Key.from_encoded_key(KIDLISP_KEY))
    
    print(f"   ✓ RPC: {GHOSTNET_RPC}")
    print(f"   ✓ Address: {ptz.key.public_key_hash()}")
    
    # Check balance
    print("\n💰 Checking balance...")
    balance = ptz.balance()
    balance_xtz = balance / 1_000_000
    print(f"   ✓ Balance: {balance_xtz:.6f} XTZ")
    
    if balance_xtz < 1:
        print("   ⚠️  Low balance! Get testnet tez from: https://faucet.ghostnet.teztnets.com/")
    
    # Load contract
    print("\n📄 Loading contract...")
    contract_file = Path(__file__).parent / "michelson-lib" / "keeps-fa2-complete.tz"
    
    if not contract_file.exists():
        print(f"❌ Contract file not found: {contract_file}")
        sys.exit(1)
    
    contract_code = contract_file.read_text()
    print(f"   ✓ Loaded: {contract_file.name}")
    
    # Initial storage (Michelson format)
    # Structure: (pair (pair address ledger) (pair next_token_id (pair operators token_metadata)))
    admin_address = ptz.key.public_key_hash()
    initial_storage = f'(Pair (Pair "{admin_address}" {{}}) (Pair 0 (Pair {{}} {{}})))'
    
    print(f"\n💾 Initial storage:")
    print(f"   ✓ Administrator: {admin_address}")
    print(f"   ✓ Next token ID: 0")
    
    # Deploy
    print("\n📤 Deploying contract...")
    print("   (This may take 1-2 minutes...)")
    
    try:
        # Originate
        op = ptz.origination(script={
            'code': contract_code,
            'storage': initial_storage
        }).autofill().sign()
        
        print(f"   ⏳ Operation: {op.hash}")
        print("   ⏳ Waiting for confirmation...")
        
        # Inject and wait
        result = op.inject(_async=False)
        
        # Get contract address from result
        contract_address = None
        if hasattr(result, 'contents'):
            for content in result.contents:
                if hasattr(content, 'metadata') and content.metadata:
                    results = content.metadata.get('operation_result', {}).get('originated_contracts', [])
                    if results:
                        contract_address = results[0]
                        break
        
        if not contract_address:
            # Try to find from operation receipt
            print("\n⚠️  Looking for contract address in operation receipt...")
            op_result = ptz.shell.blocks['head'].operations[3][op.hash].get()
            for content in op_result:
                if 'metadata' in content:
                    results = content['metadata'].get('operation_result', {}).get('originated_contracts', [])
                    if results:
                        contract_address = results[0]
                        break
        
        if contract_address:
            print("\n" + "=" * 70)
            print("✅ Contract Deployed Successfully!")
            print("=" * 70)
            print()
            print(f"📍 Contract Address: {contract_address}")
            print(f"🔍 View on TzKT: https://ghostnet.tzkt.io/{contract_address}")
            print()
            
            # Save contract address
            address_file = Path(__file__).parent / "contract-address.txt"
            address_file.write_text(contract_address)
            print(f"💾 Saved to: {address_file}")
            
            return contract_address
        else:
            print("\n⚠️  Deployment succeeded but couldn't extract contract address")
            print(f"   Check operation: https://ghostnet.tzkt.io/{op.hash}")
            
    except Exception as e:
        print(f"\n❌ Deployment failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    deploy_contract()
