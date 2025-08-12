"""
KidLisp - Simplified FA2 Smart Contract
"""

import smartpy as sp

@sp.module  
def main():
    
    class KidLisp(sp.Contract):
        def __init__(self, admin):
            # FA2 Storage
            self.data.balances = sp.big_map()  # (address, token_id) -> nat
            self.data.token_metadata = sp.big_map()  # token_id -> metadata
            self.data.operators = sp.big_map()  # (owner, operator, token_id) -> unit
            self.data.metadata = sp.big_map()  # Contract metadata
            
            # KidLisp specific storage
            self.data.administrator = admin
            self.data.next_token_id = sp.nat(0)
            self.data.code_hashes = sp.big_map()  # token_id -> code_hash 
            self.data.hash_to_token = sp.big_map()  # code_hash -> token_id
            self.data.creators = sp.big_map()  # token_id -> creator_address
            self.data.royalty_percentage = sp.nat(500)  # 5%
            self.data.minting_fee = sp.mutez(100000)  # 0.1 tez
            
        @sp.entrypoint
        def mint_kidlisp_token(self, code_hash, creator, metadata):
            # Verify admin
            assert sp.sender == self.data.administrator, "Only admin"
            
            # Check if token exists
            if self.data.hash_to_token.contains(code_hash):
                # Token exists, mint more
                token_id = self.data.hash_to_token[code_hash]
                creator_addr = self.data.creators[token_id]
                key = (creator_addr, token_id)
                if self.data.balances.contains(key):
                    self.data.balances[key] += 1000000  # 1M tokens
                else:
                    self.data.balances[key] = 1000000
            else:
                # Create new token
                token_id = self.data.next_token_id
                self.data.next_token_id += 1
                
                # Store mappings
                self.data.code_hashes[token_id] = code_hash
                self.data.hash_to_token[code_hash] = token_id  
                self.data.creators[token_id] = creator
                self.data.token_metadata[token_id] = metadata
                
                # Mint initial supply
                key = (creator, token_id)
                self.data.balances[key] = 1000000  # 1M tokens
                
        @sp.entrypoint
        def transfer(self, transfers):
            for transfer in transfers:
                for tx in transfer.txs:
                    from_key = (transfer.from_, tx.token_id)
                    to_key = (tx.to_, tx.token_id)
                    
                    # Verify sender authorization
                    assert (sp.sender == transfer.from_) or \
                           self.data.operators.contains((transfer.from_, sp.sender, tx.token_id)), \
                           "FA2_NOT_OPERATOR"
                    
                    # Check balance
                    assert self.data.balances.get(from_key, default=0) >= tx.amount, "FA2_INSUFFICIENT_BALANCE"
                    
                    # Transfer
                    self.data.balances[from_key] = sp.as_nat(self.data.balances.get(from_key, default=0) - tx.amount)
                    self.data.balances[to_key] = self.data.balances.get(to_key, default=0) + tx.amount
                    
        @sp.entrypoint 
        def update_operators(self, updates):
            for update in updates:
                if update.variant == "add_operator":
                    assert sp.sender == update.value.owner, "FA2_NOT_OWNER"
                    self.data.operators[(update.value.owner, update.value.operator, update.value.token_id)] = True
                else:  # remove_operator
                    assert sp.sender == update.value.owner, "FA2_NOT_OWNER"
                    del self.data.operators[(update.value.owner, update.value.operator, update.value.token_id)]
                    
        @sp.onchain_view
        def balance_of(self, requests):
            # For now, return empty list - this would need proper FA2 implementation
            return []
            
        @sp.onchain_view
        def get_token_for_hash(self, code_hash):
            if self.data.hash_to_token.contains(code_hash):
                return sp.Some(self.data.hash_to_token[code_hash])
            else:
                return None

# Test
@sp.add_test()
def test():
    admin = sp.test_account("admin")
    alice = sp.test_account("alice")
    
    scenario = sp.test_scenario("KidLisp", [main])
    scenario.h1("KidLisp")
    
    c = main.KidLisp(admin.address)
    scenario += c
    
    # Test minting
    scenario.h2("Mint Token")
    c.mint_kidlisp_token(
        code_hash="abc123",
        creator=alice.address,
        metadata=sp.bytes("0x"),
        _sender=admin
    )
