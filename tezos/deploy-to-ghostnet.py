#!/usr/bin/env python3
"""
Deploy FA2 contract to Ghostnet using octez-client
"""

import json
import subprocess
import sys
import re
from pathlib import Path

# Load kidlisp wallet credentials
env_file = Path(__file__).parent.parent / "aesthetic-computer-vault" / "tezos" / "kidlisp" / ".env"
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
OCTEZ_CLIENT = "octez-client"
TEZOS_CLIENT_DIR = Path(__file__).parent / ".tezos-client"

# Ensure client directory exists
TEZOS_CLIENT_DIR.mkdir(exist_ok=True)


def run_command(cmd, input_text=None):
    """Run a shell command and return output"""
    try:
        # Set custom Tezos client directory
        env = {'TEZOS_CLIENT_DIR': str(TEZOS_CLIENT_DIR)}
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            input=input_text,
            env={**subprocess.os.environ, **env}
        )
        return result.stdout, result.stderr, result.returncode
    except Exception as e:
        print(f"❌ Command failed: {e}")
        sys.exit(1)


def setup_octez_client():
    """Configure octez-client for Ghostnet"""
    print("🔧 Setting up octez-client...")
    
    # Set endpoint
    cmd = f"{OCTEZ_CLIENT} --endpoint {GHOSTNET_RPC} config update"
    _, _, code = run_command(cmd)
    if code != 0:
        print("   ⚠️  Could not update config (continuing anyway)")
    
    # Import key if not already imported
    cmd = f"{OCTEZ_CLIENT} --endpoint {GHOSTNET_RPC} list known addresses"
    stdout, _, _ = run_command(cmd)
    
    if 'kidlisp' not in stdout:
        print("   Importing kidlisp key...")
        cmd = f"{OCTEZ_CLIENT} --endpoint {GHOSTNET_RPC} import secret key kidlisp unencrypted:{KIDLISP_KEY} --force"
        _, stderr, code = run_command(cmd)
        if code != 0:
            print(f"   ⚠️  Key import warning: {stderr}")
    
    print(f"   ✓ Configured for {GHOSTNET_RPC}")
    print(f"   ✓ Using account: kidlisp ({KIDLISP_ADDRESS})")


def check_balance():
    """Check wallet balance"""
    cmd = f"{OCTEZ_CLIENT} --endpoint {GHOSTNET_RPC} get balance for kidlisp"
    stdout, _, code = run_command(cmd)
    
    if code == 0:
        balance_match = re.search(r'([\d.]+) ꜩ', stdout)
        if balance_match:
            balance = float(balance_match.group(1))
            print(f"   ✓ Balance: {balance:.6f} tez")
            if balance < 1.0:
                print("   ⚠️  Low balance! Consider adding more tez.")
            return balance
    
    print("   ⚠️  Could not check balance")
    return 0


def load_contract():
    """Load the Michelson contract"""
    contract_file = Path(__file__).parent / "michelson-lib" / "keeps-fa2-complete.tz"
    
    if not contract_file.exists():
        print(f"❌ Contract not found: {contract_file}")
        print("   Run: cd michelson-lib && pytest test_fa2.py")
        sys.exit(1)
    
    return contract_file


def create_initial_storage():
    """Create initial storage Michelson expression"""
    # Storage structure: (pair (pair address (big_map nat address)) (pair nat (pair (big_map ...) (big_map ...))))
    return f'(Pair (Pair "{KIDLISP_ADDRESS}" {{}}) (Pair 0 (Pair {{}} {{}})))'


def deploy_contract():
    """Deploy the FA2 contract using octez-client"""
    print("=" * 70)
    print("🚀 Deploying FA2 Contract to Ghostnet")
    print("=" * 70)
    print()
    
    # Setup
    setup_octez_client()
    print()
    
    # Check balance
    print("💰 Checking balance...")
    balance = check_balance()
    print()
    
    # Note: balance check may fail but we can still proceed
    # if balance < 0.5:
    #     print("❌ Insufficient balance for deployment!")
    #     print("   Get testnet tez from: https://faucet.ghostnet.teztnets.com/")
    #     sys.exit(1)
    
    # Load contract
    print("📄 Loading contract...")
    contract_file = load_contract()
    print(f"   ✓ {contract_file.name}")
    print()
    
    # Create initial storage
    print("💾 Creating initial storage...")
    initial_storage = create_initial_storage()
    print(f"   ✓ Administrator: {KIDLISP_ADDRESS}")
    print(f"   ✓ Initial token ID: 0")
    print()
    
    # Originate contract
    print("📤 Deploying contract to Ghostnet...")
    print("   (This may take 1-2 minutes...)")
    print()
    
    cmd = (
        f'{OCTEZ_CLIENT} --endpoint {GHOSTNET_RPC} '
        f'originate contract keeps_fa2 '
        f'transferring 0 from kidlisp '
        f'running {contract_file.absolute()} '
        f'--init \'{initial_storage}\' '
        f'--burn-cap 10 '
        f'--force'
    )
    
    stdout, stderr, code = run_command(cmd)
    
    if code != 0:
        print("❌ Deployment failed!")
        print(f"Exit code: {code}")
        if stderr:
            print(f"Error: {stderr}")
        if stdout:
            print(f"Output: {stdout}")
        sys.exit(1)
    
    # Extract contract address from output
    contract_address = None
    for line in stdout.split('\n'):
        if 'New contract' in line:
            match = re.search(r'(KT1[a-zA-Z0-9]{33})', line)
            if match:
                contract_address = match.group(1)
                break
    
    if not contract_address:
        print("❌ Could not find contract address in output!")
        print(stdout)
        sys.exit(1)
    
    print("=" * 70)
    print("✅ Contract Deployed Successfully!")
    print("=" * 70)
    print()
    print(f"📍 Contract Address: {contract_address}")
    print(f"🔍 View on TzKT: https://ghostnet.tzkt.io/{contract_address}")
    print()
    
    # Save contract address
    config_file = Path(__file__).parent / "contract-address.txt"
    with open(config_file, 'w') as f:
        f.write(contract_address)
    
    print(f"💾 Saved to: {config_file}")
    print()
    print("=" * 70)
    print("Next Steps:")
    print("=" * 70)
    print()
    print("1. Mint your first token:")
    print(f'   python mint-to-ghostnet.py "https://aesthetic.computer/$ceo" --contract {contract_address}')
    print()
    print("2. View on explorers:")
    print(f"   • TzKT: https://ghostnet.tzkt.io/{contract_address}")
    print(f"   • Better Call Dev: https://better-call.dev/ghostnet/{contract_address}")
    print()
    
    return contract_address


if __name__ == "__main__":
    deploy_contract()
