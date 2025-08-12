"""
KidLisp - FA2 Smart Contract using official FA2 library
"""

import smartpy as sp

# Import the FA2 library we saved locally
exec(open('lib/fa2_lib.py').read())

@sp.module
def main():
    
    class KidLisp(
        main.Fungible,
        main.Admin,
        main.MintFungible,
        main.OffchainviewTokenMetadata,
        main.OnchainviewBalanceOf,
    ):
        """KidLisp FA2 Contract with custom minting logic"""
        
        def __init__(self, admin):
            # Initialize metadata
            metadata = sp.big_map({
                "": sp.bytes("0x74657a6f732d73746f726167653a636f6e74656e74"),  # "tezos-storage:content"
                "content": sp.bytes("0x7b226e616d65223a20224b69644c697370222c20226465736372697074696f6e223a202246413220746f6b656e7320666f72206165737468657469632e636f6d7075746572204b69644c69737020636f646520736e69707065747322207d")
            })
            
            # Initialize with empty ledger and token metadata
            main.Fungible.__init__(self, metadata, {}, [])
            main.Admin.__init__(self, admin)
            main.MintFungible.__init__(self)
            main.OffchainviewTokenMetadata.__init__(self)
            main.OnchainviewBalanceOf.__init__(self)
            
            # KidLisp specific storage
            self.data.code_hashes = sp.big_map()  # token_id -> code_hash
            self.data.hash_to_token = sp.big_map()  # code_hash -> token_id 
            self.data.creators = sp.big_map()  # token_id -> creator_address
            self.data.royalty_percentage = sp.nat(500)  # 5%
        
        @sp.entrypoint
        def mint_kidlisp_token(self, code_hash, creator, metadata):
            """Mint KidLisp tokens for a code snippet"""
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            
            # Check if token already exists for this code hash
            if self.data.hash_to_token.contains(code_hash):
                # Token exists, mint more to creator
                token_id = self.data.hash_to_token[code_hash]
                existing_creator = self.data.creators[token_id]
                
                # Mint to the original creator
                self.data.supply[token_id] += 1000000
                ledger_key = (existing_creator, token_id)
                self.data.ledger[ledger_key] = (
                    self.data.ledger.get(ledger_key, default=0) + 1000000
                )
            else:
                # Create new token
                token_id = self.data.next_token_id
                
                # Store metadata
                self.data.token_metadata[token_id] = sp.record(
                    token_id=token_id, 
                    token_info=metadata
                )
                
                # Store KidLisp mappings
                self.data.code_hashes[token_id] = code_hash
                self.data.hash_to_token[code_hash] = token_id
                self.data.creators[token_id] = creator
                
                # Initialize supply and mint to creator
                self.data.supply[token_id] = 1000000
                ledger_key = (creator, token_id)
                self.data.ledger[ledger_key] = 1000000
                
                # Increment next token ID
                self.data.next_token_id += 1
        
        @sp.onchain_view
        def get_token_for_hash(self, code_hash):
            """Get token ID for a code hash"""
            if self.data.hash_to_token.contains(code_hash):
                return self.data.hash_to_token[code_hash]
            else:
                # Return None equivalent - we'll handle this in the integration
                return sp.nat(999999999)  # Use a sentinel value for "not found"

# Test
@sp.add_test()
def test():
    admin = sp.test_account("admin")
    alice = sp.test_account("alice")
    
    c = main.KidLisp(admin.address)
    
    scenario = sp.test_scenario("KidLisp", [main])
    scenario.h1("KidLisp FA2 Contract")
    scenario += c
    
    # Test minting
    scenario.h2("Mint KidLisp Token")
    metadata = sp.map({
        "name": sp.bytes("0x4b69644c69737020546573740"),  # "KidLisp Test"
        "symbol": sp.bytes("0x4b4c53500"),  # "KLSP"
        "description": sp.bytes("0x546573742546f6b656e0"),  # "Test token"
    })
    
    c.mint_kidlisp_token(
        code_hash="abc123hash",
        creator=alice.address,
        metadata=metadata,
        _sender=admin
    )
    
    # Test that token exists
    scenario.verify(c.get_token_for_hash("abc123hash") == 0)
    
    # Test balance
    scenario.verify(c.get_balance(sp.record(owner=alice.address, token_id=0)) == 1000000)
