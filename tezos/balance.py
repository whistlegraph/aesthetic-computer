#!/usr/bin/env python3
"""
Check aesthetic.tez wallet balance on Ghostnet
Simple utility for the minimal tezos system
"""

from pytezos import pytezos

def check_balance():
    """Check aesthetic.tez wallet balance"""
    
    try:
        # Connect to Ghostnet
        client = pytezos.using(shell='https://ghostnet.ecadinfra.com')
        address = 'tz1gkf8EexComFBJvjtT1zdsisdah791KwBE'
        
        # Get account info
        account_info = client.account(address)
        balance = int(account_info['balance'])
        
        # Display results
        print("💰 aesthetic.tez wallet status:")
        print("=" * 35)
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
            
        return True
        
    except Exception as e:
        print(f"❌ Error checking balance: {e}")
        print("💡 Check internet connection and try again")
        return False

if __name__ == "__main__":
    check_balance()
