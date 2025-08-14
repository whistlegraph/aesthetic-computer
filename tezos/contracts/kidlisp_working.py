"""
KidLisp - Simple working FA2 contract for testnet deployment
"""

import smartpy as sp

@sp.module
def main():
    
    class KidLisp(sp.Contract):
        """Simple KidLisp FA2-like contract"""
        
        def __init__(self, admin):
            # Basic FA2 storage
            self.data.balances = sp.cast(sp.big_map(), sp.big_map[sp.pair[sp.address, sp.nat], sp.nat])
            self.data.token_metadata = sp.cast(sp.big_map(), sp.big_map[sp.nat, sp.bytes])
            self.data.operators = sp.cast(sp.big_map(), sp.big_map[sp.record(owner=sp.address, operator=sp.address, token_id=sp.nat), sp.unit])
            self.data.metadata = sp.cast(sp.big_map(), sp.big_map[sp.string, sp.bytes])
            
            # Admin and control
            self.data.administrator = admin
            self.data.next_token_id = sp.nat(0)
            
            # KidLisp specific storage
            self.data.code_hashes = sp.cast(sp.big_map(), sp.big_map[sp.nat, sp.string])
            self.data.hash_to_token = sp.cast(sp.big_map(), sp.big_map[sp.string, sp.nat])
            self.data.creators = sp.cast(sp.big_map(), sp.big_map[sp.nat, sp.address])
            
        @sp.entrypoint
        def mint_kidlisp_token(self, code_hash, creator, metadata):
            """Mint KidLisp tokens for a code snippet"""
            assert sp.sender == self.data.administrator, "Only admin can mint"
            
            # Check if token already exists for this code hash
            if self.data.hash_to_token.contains(code_hash):
                # Token exists, mint more to creator
                token_id = self.data.hash_to_token[code_hash]
                existing_creator = self.data.creators[token_id]
                
                # Mint to the original creator
                ledger_key = (existing_creator, token_id)
                self.data.balances[ledger_key] = (
                    self.data.balances.get(ledger_key, default=0) + 1000000
                )
            else:
                # Create new token
                token_id = self.data.next_token_id
                
                # Store metadata  
                self.data.token_metadata[token_id] = metadata
                
                # Store KidLisp mappings
                self.data.code_hashes[token_id] = code_hash
                self.data.hash_to_token[code_hash] = token_id
                self.data.creators[token_id] = creator
                
                # Mint initial supply to creator
                ledger_key = (creator, token_id)
                self.data.balances[ledger_key] = 1000000
                
                # Increment next token ID
                self.data.next_token_id += 1
        
        @sp.entrypoint
        def transfer(self, transfers):
            """Basic transfer functionality"""
            for transfer in transfers:
                for tx in transfer.txs:
                    from_key = (transfer.from_, tx.token_id)
                    to_key = (tx.to_, tx.token_id)
                    
                    # Basic authorization check
                    assert (sp.sender == transfer.from_), "Not authorized"
                    
                    # Check balance
                    from_balance = self.data.balances.get(from_key, default=0)
                    assert from_balance >= tx.amount, "Insufficient balance"
                    
                    # Transfer
                    self.data.balances[from_key] = sp.as_nat(from_balance - tx.amount)
                    self.data.balances[to_key] = (
                        self.data.balances.get(to_key, default=0) + tx.amount
                    )
        
        @sp.onchain_view
        def get_token_for_hash(self, code_hash):
            """Get token ID for a code hash"""
            if self.data.hash_to_token.contains(code_hash):
                return self.data.hash_to_token[code_hash]
            else:
                return sp.nat(999999999)  # Sentinel for "not found"
        
        @sp.onchain_view
        def get_balance(self, params):
            """Get balance for an address and token"""
            key = (params.owner, params.token_id)
            return self.data.balances.get(key, default=0)

# Test
@sp.add_test()
def test():
    admin = sp.test_account("admin")
    alice = sp.test_account("alice")
    bob = sp.test_account("bob")
    
    scenario = sp.test_scenario("KidLisp", [main])
    scenario.h1("KidLisp Contract")
    
    c = main.KidLisp(admin.address)
    scenario += c
    
    # Test minting
    scenario.h2("Mint KidLisp Token")
    
    c.mint_kidlisp_token(
        code_hash="abc123hash",
        creator=alice.address,
        metadata=sp.scenario_utils.bytes_of_string('{"name": "KidLisp Test Token"}'),
        _sender=admin
    )
    
    # Test that token exists
    scenario.verify(c.get_token_for_hash("abc123hash") == 0)
    
    # Test balance
    scenario.verify(c.get_balance(sp.record(owner=alice.address, token_id=0)) == 1000000)
    
    # Test that non-existent hash returns sentinel
    scenario.verify(c.get_token_for_hash("nonexistent") == 999999999)
