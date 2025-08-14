"""
KidLisp - FA2 Smart Contract using official FA2 library
"""

import smartpy as sp

# Import the FA2 library (assuming it's in the same directory or available)
FA2 = sp.io.import_script_from_url("file://fa2_lib.py")

@sp.module
def main():
    import FA2

    class KidLisp(
        FA2.main.Fungible,
        FA2.main.Admin,
        FA2.main.MintFungible,
        FA2.main.OffchainviewTokenMetadata,
        FA2.main.OnchainviewBalanceOf,
    ):
        """KidLisp FA2 Contract with custom minting logic"""
        
        def __init__(self, admin):
            # Initialize metadata
            metadata = sp.big_map({
                "": sp.utils.bytes_of_string("tezos-storage:content"),
                "content": sp.utils.bytes_of_string('{"name": "KidLisp", "description": "FA2 tokens for aesthetic.computer KidLisp code snippets"}')
            })
            
            # Initialize with empty ledger and token metadata
            FA2.main.Fungible.__init__(self, metadata, {}, [])
            FA2.main.Admin.__init__(self, admin)
            FA2.main.MintFungible.__init__(self)
            FA2.main.OffchainviewTokenMetadata.__init__(self)
            FA2.main.OnchainviewBalanceOf.__init__(self)
            
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
    
    scenario = sp.test_scenario("KidLisp", [main, FA2])
    scenario.h1("KidLisp FA2 Contract")
    scenario += c
    
    # Test minting
    scenario.h2("Mint KidLisp Token")
    metadata = sp.map({
        "name": sp.utils.bytes_of_string("KidLisp Test"),
        "symbol": sp.utils.bytes_of_string("KLSP"),
        "description": sp.utils.bytes_of_string("Test token"),
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
