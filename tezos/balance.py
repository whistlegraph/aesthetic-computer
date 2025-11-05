#!/usr/bin/env python3
"""
Check Tezos wallet balances on Ghostnet
Supports both aesthetic.tez (personal) and kidlisp (project) wallets
"""

import sys
from pytezos import pytezos

WALLETS = {
    'aesthetic': {
        'address': 'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE',
        'name': 'aesthetic.tez',
        'description': 'Personal AC wallet'
    },
    'kidlisp': {
        'address': 'tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC',
        'name': 'KidLisp',
        'description': 'KidLisp project wallet'
    }
}

def check_balance(wallet_key='aesthetic'):
    """Check wallet balance for specified wallet"""
    
    if wallet_key not in WALLETS:
        print(f"❌ Unknown wallet: {wallet_key}")
        print(f"Available wallets: {', '.join(WALLETS.keys())}")
        return False
    
    wallet = WALLETS[wallet_key]
    
    try:
        # Connect to Ghostnet
        client = pytezos.using(shell='https://ghostnet.ecadinfra.com')
        address = wallet['address']
        
        # Get account info
        account_info = client.account(address)
        balance = int(account_info['balance'])
        
        # Display results
        print(f"💰 {wallet['name']} wallet status:")
        print(f"   {wallet['description']}")
        print("=" * 50)
        print(f"Address: {address}")
        print(f"Balance: {balance / 1000000:.6f} XTZ")
        print(f"Mutez:   {balance:,}")
        print(f"Network: Ghostnet")
        print()
        
        # Status check
        if balance > 1000000:  # > 1 XTZ
            print("✅ Sufficient funds for contract deployment")
        elif balance > 100000:  # > 0.1 XTZ
            print("⚠️  Low funds - consider adding more XTZ")
        else:
            print("❌ Insufficient funds - need more XTZ")
            print(f"💡 Get funds: https://faucet.ghostnet.teztnets.com/")
            
        return True
        
    except Exception as e:
        print(f"❌ Error checking balance: {e}")
        print("💡 Check internet connection and try again")
        return False

def check_all_wallets():
    """Check all configured wallets"""
    for wallet_key in WALLETS:
        check_balance(wallet_key)
        print()

if __name__ == "__main__":
    if len(sys.argv) > 1:
        wallet = sys.argv[1]
        check_balance(wallet)
    else:
        # Default: show all wallets
        check_all_wallets()
