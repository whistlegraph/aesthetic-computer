"""
KidLisp - Minimal Working FA2 Smart Contract
"""

import smartpy as sp

@sp.module  
def main():
    
    # Minimal types for KidLisp contract
    mint_params_type = sp.record(
        code_hash=sp.string,
        creator=sp.address,
        metadata=sp.map[sp.string, sp.bytes]
    )
    
    class KidLisp(sp.Contract):
        def __init__(self, admin):
            # Simplified storage for MVP
            self.data.administrator = admin
            self.data.next_token_id = 0
            self.data.hash_to_token = sp.cast({}, sp.map[sp.string, sp.nat])
            self.data.token_creators = sp.cast({}, sp.map[sp.nat, sp.address])
            
        @sp.entrypoint
        def mint_kidlisp_token(self, params):
            sp.cast(params, mint_params_type)
            
            # Only admin can mint
            assert sp.sender == self.data.administrator
            
            # Check if token already exists
            if params.code_hash in self.data.hash_to_token:
                # Token exists, just return the existing token ID
                pass
            else:
                # Create new token
                token_id = self.data.next_token_id
                self.data.hash_to_token[params.code_hash] = token_id
                self.data.token_creators[token_id] = params.creator
                self.data.next_token_id += 1
        
        @sp.onchain_view
        def get_token_for_hash(self, code_hash):
            sp.cast(code_hash, sp.string)
            if code_hash in self.data.hash_to_token:
                return self.data.hash_to_token[code_hash]
            else:
                return -1  # Return -1 if not found

# Test
if "main" in __name__:
    @sp.add_test()
    def test():
        scenario = sp.test_scenario("KidLisp", [main])
        scenario.h1("KidLisp MVP")
        
        admin = sp.test_account("admin")
        alice = sp.test_account("alice")
        
        c = main.KidLisp(admin.address)
        scenario += c
        
        # Test minting
        scenario.h2("Mint Token")
        c.mint_kidlisp_token(
            sp.record(
                code_hash="abc123hash",
                creator=alice.address,
                metadata={}
            ),
            _sender=admin
        )
        
        # Test view
        scenario.h2("Check Token")
        scenario.verify(c.get_token_for_hash("abc123hash") == 0)
        scenario.verify(c.get_token_for_hash("nonexistent") == -1)
