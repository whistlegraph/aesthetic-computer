#!/usr/bin/env python3
"""
Create KidLisp Wallet for Tezos
Generates a new wallet for the KidLisp project
"""

from pytezos import Key
from pytezos.crypto.key import Mnemonic
import os

def create_wallet():
    """Generate a new Tezos wallet for KidLisp"""
    
    print("🔑 Creating new KidLisp wallet...")
    print("=" * 50)
    
    # Generate a new mnemonic
    mnemonic = Mnemonic()
    mnemonic_words = mnemonic.generate()
    
    # Create key from mnemonic
    key = Key.from_mnemonic(mnemonic=mnemonic_words, passphrase='')
    
    # Get the various key formats
    public_key = key.public_key()
    public_key_hash = key.public_key_hash()
    secret_key = key.secret_key()
    
    print(f"\n✅ Wallet generated successfully!")
    print("\n📋 WALLET DETAILS:")
    print("=" * 50)
    print(f"Address (tz1...):     {public_key_hash}")
    print(f"Public Key:           {public_key}")
    print(f"Private Key (edsk...): {secret_key}")
    print(f"Mnemonic Phrase:      {mnemonic_words}")
    print("=" * 50)
    
    # Save to vault
    vault_dir = "../aesthetic-computer-vault/tezos"
    if os.path.exists("../aesthetic-computer-vault"):
        os.makedirs(vault_dir, exist_ok=True)
        
        env_content = f"""# KidLisp Tezos Wallet Configuration
# 🔐 PRIVATE - Safe to commit to private aesthetic-computer-vault repo
# Generated: November 5, 2025

# KidLisp Wallet Address (public)
KIDLISP_ADDRESS={public_key_hash}

# KidLisp Public Key
KIDLISP_PUBLIC_KEY={public_key}

# KidLisp Private Key (KEEP SECRET)
KIDLISP_KEY={secret_key}

# Mnemonic Phrase (for wallet recovery)
KIDLISP_MNEMONIC={mnemonic_words}

# Network Configuration
TEZOS_MAINNET_RPC=https://mainnet.api.tez.ie
TEZOS_GHOSTNET_RPC=https://ghostnet.ecadinfra.com

# Domain
KIDLISP_DOMAIN=kidlisp.tez (to be registered)

# Links:
# - Ghostnet Faucet: https://faucet.ghostnet.teztnets.com/
# - Tezos Domains: https://tezos.domains/
# - Explorer: https://tzkt.io/{public_key_hash}
"""
        
        env_path = os.path.join(vault_dir, ".env")
        with open(env_path, "w") as f:
            f.write(env_content)
        
        print(f"\n💾 Saved to: {env_path}")
        print("✅ Safe to commit to private aesthetic-computer-vault repo")
    
    print(f"\n⚠️  SECURITY INSTRUCTIONS:")
    print("1. ✅ Keys saved to aesthetic-computer-vault/tezos/.env")
    print("2. ✅ Safe to commit to private vault repo")
    print("3. ❌ NEVER commit to public aesthetic-computer repo")
    print("4. 📝 Write down mnemonic phrase on paper as backup")
    print("5. 💰 Fund this address with XTZ for usage")
    
    print(f"\n📍 NEXT STEPS:")
    print("1. Get Ghostnet XTZ from faucet:")
    print(f"   https://faucet.ghostnet.teztnets.com/")
    print(f"   Address: {public_key_hash}")
    print()
    print("2. Register kidlisp.tez domain:")
    print("   Visit: https://tezos.domains/")
    print(f"   Using address: {public_key_hash}")
    print()
    print("3. Commit to vault:")
    print("   cd ../aesthetic-computer-vault")
    print("   git add tezos/.env")
    print("   git commit -m 'Add KidLisp Tezos wallet'")
    print("   git push")
    
    return {
        'address': public_key_hash,
        'public_key': public_key,
        'secret_key': secret_key,
        'mnemonic': mnemonic_words
    }

if __name__ == "__main__":
    wallet = create_wallet()
