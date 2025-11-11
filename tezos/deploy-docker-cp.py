#!/usr/bin/env python3
"""
Deploy FA2 contract to Ghostnet using docker cp approach

This avoids volume mount issues by:
1. Starting a container in detached mode
2. Copying the contract file into it
3. Running commands inside it
4. Cleaning up
"""

import subprocess
import sys
import re
import time
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
CONTAINER_NAME = f"octez-deploy-{int(time.time())}"

def run_cmd(cmd, check=True):
    """Run a command and return output"""
    result = subprocess.run(cmd, shell=isinstance(cmd, str), capture_output=True, text=True)
    if check and result.returncode != 0:
        print(f"❌ Command failed: {' '.join(cmd) if isinstance(cmd, list) else cmd}")
        print(f"Error: {result.stderr}")
        sys.exit(1)
    return result.stdout, result.stderr, result.returncode

def deploy_contract():
    """Deploy contract using docker cp method"""
    
    print("=" * 70)
    print("🚀 Deploying FA2 Contract to Ghostnet (Docker CP Method)")
    print("=" * 70)
    print()
    
    if not CONTRACT_FILE.exists():
        print(f"❌ Contract not found: {CONTRACT_FILE}")
        sys.exit(1)
    
    initial_storage = f'(Pair (Pair "{KIDLISP_ADDRESS}" {{}}) (Pair 0 (Pair {{}} {{}})))'
    
    print(f"📄 Contract: {CONTRACT_FILE.name}")
    print(f"👤 Admin: {KIDLISP_ADDRESS}")
    print(f"🌐 Network: Ghostnet")
    print()
    
    container_id = None
    
    try:
        # Start a long-running container
        print("🐳 Starting Docker container...")
        stdout, _, _ = run_cmd([
            "docker", "run", "-d",
            "--name", CONTAINER_NAME,
            "--entrypoint", "sleep",
            OCTEZ_IMAGE,
            "300"  # Sleep for 5 minutes
        ])
        container_id = stdout.strip()
        print(f"   ✓ Container: {container_id[:12]}")
        print()
        
        # Copy contract into container
        print("📋 Copying contract into container...")
        run_cmd([
            "docker", "cp",
            str(CONTRACT_FILE),
            f"{container_id}:/contract.tz"
        ])
        print("   ✓ Contract copied")
        print()
        
        # Import key
        print("🔑 Importing key...")
        run_cmd([
            "docker", "exec", container_id,
            "/usr/local/bin/octez-client",
            "import", "secret", "key", "kidlisp",
            f"unencrypted:{KIDLISP_KEY}",
            "--force"
        ], check=False)  # May warn about node connection
        print("   ✓ Key imported")
        print()
        
        # Deploy contract
        print("🚀 Deploying contract (1-2 minutes)...")
        print()
        
        stdout, stderr, code = run_cmd([
            "docker", "exec", container_id,
            "/usr/local/bin/octez-client",
            "--endpoint", GHOSTNET_RPC,
            "originate", "contract", "keeps_fa2",
            "transferring", "0", "from", "kidlisp",
            "running", "/contract.tz",
            "--init", initial_storage,
            "--burn-cap", "10",
            "--force"
        ], check=False)
        
        output = stdout + stderr
        print(output)
        
        if code != 0:
            print(f"\n❌ Deployment failed! Exit code: {code}")
            return None
        
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
            
            # Save address
            address_file = Path(__file__).parent / ".contract-address"
            address_file.write_text(address)
            print(f"💾 Address saved to: {address_file.name}")
            
            return address
        else:
            print("\n⚠️ Could not extract contract address from output")
            return None
            
    except Exception as e:
        print(f"❌ Error: {e}")
        return None
        
    finally:
        # Cleanup
        if container_id:
            print()
            print("🧹 Cleaning up...")
            subprocess.run(["docker", "rm", "-f", container_id], 
                         capture_output=True, check=False)
            print("   ✓ Container removed")


if __name__ == "__main__":
    deploy_contract()
