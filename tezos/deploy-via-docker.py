#!/usr/bin/env python3
"""
Deploy FA2 contract to Ghostnet using Octez via Docker

This uses the latest Octez Docker image to avoid protocol version issues
with the outdated octez-client binary in the devcontainer.
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
OCTEZ_VERSION = "master"  # Latest build with Ghostnet protocol support
OCTEZ_IMAGE = f"tezos/tezos:{OCTEZ_VERSION}"
TEZOS_CLIENT_DIR = Path(__file__).parent / ".tezos-client"

# Ensure client directory exists
TEZOS_CLIENT_DIR.mkdir(exist_ok=True)


def run_octez_docker(command: list, capture_output=True):
    """
    Run octez-client command via Docker
    
    Uses the latest Octez Docker image which supports current Ghostnet protocol
    """
    docker_cmd = [
        "docker", "run", "--rm",
        "-v", f"{Path.cwd()}:/workspace",
        "-v", f"{TEZOS_CLIENT_DIR.absolute()}:/root/.tezos-client",
        "-w", "/workspace",
        OCTEZ_IMAGE,
    ] + command
    
    try:
        if capture_output:
            result = subprocess.run(docker_cmd, capture_output=True, text=True)
            return result.stdout, result.stderr, result.returncode
        else:
            result = subprocess.run(docker_cmd)
            return "", "", result.returncode
    except Exception as e:
        print(f"❌ Docker command failed: {e}")
        sys.exit(1)


def check_docker():
    """Check if Docker is available"""
    try:
        result = subprocess.run(["docker", "--version"], capture_output=True, text=True)
        if result.returncode != 0:
            print("❌ Error: Docker not available")
            print("Docker should be pre-installed in the devcontainer.")
            print("If missing, run: sudo dnf install -y docker")
            sys.exit(1)
        print(f"✓ {result.stdout.strip()}")
    except FileNotFoundError:
        print("❌ Error: Docker not installed")
        print("Docker should be pre-installed in the devcontainer.")
        print("If missing, run: sudo dnf install -y docker")
        sys.exit(1)


def pull_octez_image():
    """Pull the latest Octez Docker image"""
    print(f"📦 Pulling {OCTEZ_IMAGE}...")
    result = subprocess.run(["docker", "pull", OCTEZ_IMAGE], capture_output=True, text=True)
    if result.returncode != 0:
        print(f"❌ Failed to pull image: {result.stderr}")
        sys.exit(1)
    print("✓ Image ready")


def setup_octez_client():
    """Configure octez-client for Ghostnet"""
    print("🔧 Setting up octez-client...")
    
    # Set endpoint
    cmd = ["octez-client", "--endpoint", GHOSTNET_RPC, "config", "update"]
    _, _, code = run_octez_docker(cmd)
    if code != 0:
        print("   ⚠️  Could not update config (continuing anyway)")
    
    # Import key if not already imported
    cmd = ["octez-client", "--endpoint", GHOSTNET_RPC, "list", "known", "addresses"]
    stdout, _, _ = run_octez_docker(cmd)
    
    if 'kidlisp' not in stdout:
        print("   Importing kidlisp key...")
        cmd = [
            "octez-client", "--endpoint", GHOSTNET_RPC,
            "import", "secret", "key", "kidlisp", 
            f"unencrypted:{KIDLISP_KEY}", "--force"
        ]
        _, stderr, code = run_octez_docker(cmd)
        if code != 0:
            print(f"   ⚠️  Key import warning: {stderr}")
    
    print(f"   ✓ Configured for {GHOSTNET_RPC}")
    print(f"   ✓ Using account: kidlisp ({KIDLISP_ADDRESS})")


def check_balance():
    """Check wallet balance"""
    cmd = ["octez-client", "--endpoint", GHOSTNET_RPC, "get", "balance", "for", "kidlisp"]
    stdout, _, code = run_octez_docker(cmd)
    
    if code == 0:
        balance_match = re.search(r'([\d.]+) ꜩ', stdout)
        if balance_match:
            balance = float(balance_match.group(1))
            print(f"   ✓ Balance: {balance:.6f} tez")
            if balance < 1.0:
                print("   ⚠️  Low balance! Get more from: https://faucet.ghostnet.teztnets.com/")
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
    return f'(Pair (Pair "{KIDLISP_ADDRESS}" {{}}) (Pair 0 (Pair {{}} {{}})))'


def deploy_contract():
    """Deploy the FA2 contract using Octez via Docker"""
    print("=" * 70)
    print("🚀 Deploying FA2 Contract to Ghostnet via Docker")
    print("=" * 70)
    print()
    
    # Check Docker
    check_docker()
    print()
    
    # Pull image
    pull_octez_image()
    print()
    
    # Setup
    setup_octez_client()
    print()
    
    # Check balance
    print("💰 Checking balance...")
    balance = check_balance()
    print()
    
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
    
    cmd = [
        "octez-client", "--endpoint", GHOSTNET_RPC,
        "originate", "contract", "keeps_fa2",
        "transferring", "0", "from", "kidlisp",
        "running", f"/workspace/{contract_file.relative_to(Path.cwd())}",
        "--init", initial_storage,
        "--burn-cap", "10",
        "--force"
    ]
    
    stdout, stderr, code = run_octez_docker(cmd, capture_output=False)
    
    if code != 0:
        print("❌ Deployment failed!")
        print(f"Exit code: {code}")
        sys.exit(1)
    
    # The contract address should be in the output
    # We'll need to check the terminal output
    print("\n✅ Deployment initiated!")
    print("\nCheck the output above for the contract address (KT1...)")
    print("Save it to continue with minting.")
    
    return None


if __name__ == "__main__":
    deploy_contract()
