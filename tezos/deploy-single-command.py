#!/usr/bin/env python3
"""
Deploy FA2 contract to Ghostnet using a single Docker command

This approach avoids volume mount issues by doing everything in one container run:
1. Import key (stays in container's memory)
2. Deploy contract (via stdin)
All in a single docker run command with no persistent volumes needed.
"""

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

# Configuration
GHOSTNET_RPC = "https://ghostnet.ecadinfra.com"
OCTEZ_IMAGE = "tezos/tezos:master"
CONTRACT_FILE = Path(__file__).parent / "michelson-lib" / "keeps-fa2-complete.tz"

def deploy_contract():
    """Deploy contract in a single Docker command"""
    
    print("=" * 70)
    print("🚀 Deploying FA2 Contract to Ghostnet (Single Command)")
    print("=" * 70)
    print()
    
    # Load contract
    if not CONTRACT_FILE.exists():
        print(f"❌ Contract not found: {CONTRACT_FILE}")
        sys.exit(1)
    
    with open(CONTRACT_FILE) as f:
        contract_code = f.read()
    
    # Create initial storage
    initial_storage = f'(Pair (Pair "{KIDLISP_ADDRESS}" {{}}) (Pair 0 (Pair {{}} {{}})))'
    
    print(f"📄 Contract: {CONTRACT_FILE.name}")
    print(f"👤 Admin: {KIDLISP_ADDRESS}")
    print(f"🌐 Network: Ghostnet")
    print()
    print("🚀 Deploying (this takes 1-2 minutes)...")
    print()
    
    # Create a shell script that runs inside the container
    # This script will:
    # 1. Import the key
    # 2. Originate the contract (reading from stdin)
    shell_script = f'''#!/bin/sh
set -e

# Import key
/usr/local/bin/octez-client import secret key kidlisp "unencrypted:{KIDLISP_KEY}" --force 2>&1 | grep -v "Failed to acquire"

# Deploy contract (reading from stdin)
/usr/local/bin/octez-client \\
  --endpoint {GHOSTNET_RPC} \\
  originate contract keeps_fa2 \\
  transferring 0 from kidlisp \\
  running /dev/stdin \\
  --init '{initial_storage}' \\
  --burn-cap 10 \\
  --force
'''
    
    # Run Docker with the script and pipe the contract via stdin
    docker_cmd = [
        "docker", "run", "--rm", "-i",
        "--entrypoint", "sh",
        OCTEZ_IMAGE,
        "-c", shell_script
    ]
    
    try:
        result = subprocess.run(
            docker_cmd,
            input=contract_code,
            text=True,
            capture_output=True
        )
        
        output = result.stdout + result.stderr
        print(output)
        
        if result.returncode != 0:
            print(f"\n❌ Deployment failed! Exit code: {result.returncode}")
            sys.exit(1)
        
        # Extract contract address
        match = re.search(r"New contract (KT1\w+)", output)
        if match:
            address = match.group(1)
            print()
            print("=" * 70)
            print(f"✅ CONTRACT DEPLOYED: {address}")
            print("=" * 70)
            print()
            print(f"View on TzKT: https://ghostnet.tzkt.io/{address}")
            print(f"View on Better Call Dev: https://better-call.dev/ghostnet/{address}")
            print()
            
            # Save address for minting
            address_file = Path(__file__).parent / ".contract-address"
            address_file.write_text(address)
            print(f"💾 Address saved to: {address_file.name}")
            
            return address
        else:
            print("\n⚠️ Could not extract contract address from output")
            return None
            
    except Exception as e:
        print(f"❌ Docker command failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    deploy_contract()
