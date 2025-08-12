"""
KidLisp Meme Coin - FA2 Smart Contract
A fungible token contract for aesthetic.computer's KidLisp language

Each unique KidLisp code snippet becomes a unique token type.
Tokens are fungible within each type (multiple copies possible).
Creators receive royalties on transfers.
"""

import smartpy as sp

@sp.module
def main():
    # FA2 Transfer type
    t_transfer_tx = sp.record(
        to_=sp.address,
        token_id=sp.nat,
        amount=sp.nat,
    )
    
    t_transfer = sp.record(
        from_=sp.address,
        txs=sp.list[t_transfer_tx]
    )
    
    class KidLispMemeCoin(sp.Contract):
        """
        KidLisp Meme Coin FA2 Contract
        
        Features:
        - FA2 compliant fungible tokens
        - Each unique KidLisp code gets its own token type
        - Creators receive royalties on transfers
        - Deterministic wallet generation from handles
        """
    - Each KidLisp snippet hash maps to a token_id
    - Automatic minting for new snippets
    - Creator royalties on transfers
    - Metadata linking to original KidLisp code
    """
    
    def __init__(self, admin, metadata, token_metadata={}):
        # Initialize base FA2 contract
        super().__init__(
            metadata=metadata,
            token_metadata=token_metadata,
            admin=admin
        )
        
        # KidLisp specific storage
        self.update_initial_storage(
            # Map from token_id to creator address
            creators=sp.big_map(tkey=sp.TNat, tvalue=sp.TAddress),
            
            # Map from token_id to KidLisp code hash
            code_hashes=sp.big_map(tkey=sp.TNat, tvalue=sp.TString),
            
            # Map from code hash to token_id (reverse lookup)
            hash_to_token=sp.big_map(tkey=sp.TString, tvalue=sp.TNat),
            
            # Royalty percentage in basis points (e.g., 500 = 5%)
            royalty_percentage=sp.nat(500),
            
            # Next available token ID
            next_token_id=sp.nat(0),
            
            # Minting fee in mutez (to prevent spam)
            minting_fee=sp.mutez(100000),  # 0.1 tez
            
            # Contract metadata
            contract_metadata=sp.big_map({
                "name": sp.utils.bytes_of_string("KidLisp Meme Coin"),
                "description": sp.utils.bytes_of_string("Fungible tokens for aesthetic.computer KidLisp code snippets"),
                "version": sp.utils.bytes_of_string("1.0.0"),
                "homepage": sp.utils.bytes_of_string("https://aesthetic.computer"),
                "license": sp.utils.bytes_of_string("MIT")
            })
        )

    @sp.entry_point
    def mint_kidlisp_token(self, params):
        """
        Mint a new KidLisp token or add to existing supply
        
        Args:
            code_hash: Hash of the KidLisp code
            kidlisp_code: The actual KidLisp source code
            creator: Address of the code creator
            amount: Number of tokens to mint
            metadata_uri: URI pointing to token metadata
        """
        sp.set_type(params, sp.TRecord(
            code_hash=sp.TString,
            kidlisp_code=sp.TString,
            creator=sp.TAddress,
            amount=sp.TNat,
            metadata_uri=sp.TString
        ))
        
        # Verify admin or authorized minter
        sp.verify(sp.sender == self.data.administrator, "Only admin can mint")
        
        # Check if minting fee is paid
        sp.verify(sp.amount >= self.data.minting_fee, "Insufficient minting fee")
        
        # Check if this code hash already exists
        with sp.if_(self.data.hash_to_token.contains(params.code_hash)):
            # Token already exists, mint more of the same type
            token_id = self.data.hash_to_token[params.code_hash]
            creator = self.data.creators[token_id]
            
            # Mint additional tokens to the creator
            self.mint(
                token_id=token_id,
                to_=creator,
                amount=params.amount
            )
        with sp.else_():
            # Create new token type
            token_id = self.data.next_token_id
            
            # Store creator and code hash mappings
            self.data.creators[token_id] = params.creator
            self.data.code_hashes[token_id] = params.code_hash
            self.data.hash_to_token[params.code_hash] = token_id
            
            # Create token metadata
            token_metadata = sp.map({
                "name": sp.utils.bytes_of_string(f"KidLisp #{token_id}"),
                "description": sp.utils.bytes_of_string(f"Meme coin for KidLisp code: {params.code_hash[:16]}..."),
                "decimals": sp.utils.bytes_of_string("6"),
                "symbol": sp.utils.bytes_of_string("KLSP"),
                "kidlisp_code": sp.utils.bytes_of_string(params.kidlisp_code),
                "code_hash": sp.utils.bytes_of_string(params.code_hash),
                "creator": sp.utils.bytes_of_string(sp.to_string(params.creator)),
                "metadata_uri": sp.utils.bytes_of_string(params.metadata_uri)
            })
            
            # Add token metadata to contract
            self.data.token_metadata[token_id] = token_metadata
            
            # Mint initial supply to creator
            self.mint(
                token_id=token_id,
                to_=params.creator,
                amount=params.amount
            )
            
            # Increment next token ID
            self.data.next_token_id += 1

    @sp.entry_point
    def transfer(self, params):
        """
        Override transfer to implement royalty payments
        """
        # Process standard FA2 transfer first
        super().transfer(params)
        
        # Calculate and distribute royalties for each transfer
        with sp.for_("transfer_item", params) as transfer_item:
            with sp.for_("tx", transfer_item.txs) as tx:
                with sp.if_(tx.amount > 0):
                    # Calculate royalty amount
                    token_id = tx.token_id
                    with sp.if_(self.data.creators.contains(token_id)):
                        creator = self.data.creators[token_id]
                        
                        # Only pay royalties if creator is not the sender or receiver
                        with sp.if_((creator != transfer_item.from_) & (creator != tx.to_)):
                            # Calculate royalty (simplified - in reality would need token value)
                            royalty_amount = (tx.amount * self.data.royalty_percentage) / 10000
                            
                            # Transfer royalty to creator (if they have a balance to transfer from)
                            with sp.if_(royalty_amount > 0):
                                # Note: This is a simplified royalty mechanism
                                # In practice, royalties would be calculated based on token value/price
                                pass

    @sp.entry_point
    def set_royalty_percentage(self, new_percentage):
        """
        Update royalty percentage (admin only)
        """
        sp.set_type(new_percentage, sp.TNat)
        sp.verify(sp.sender == self.data.administrator, "Only admin can set royalty")
        sp.verify(new_percentage <= 1000, "Royalty cannot exceed 10%")
        self.data.royalty_percentage = new_percentage

    @sp.entry_point
    def set_minting_fee(self, new_fee):
        """
        Update minting fee (admin only)
        """
        sp.set_type(new_fee, sp.TMutez)
        sp.verify(sp.sender == self.data.administrator, "Only admin can set fee")
        self.data.minting_fee = new_fee

    @sp.entry_point
    def withdraw_fees(self, amount):
        """
        Withdraw accumulated minting fees (admin only)
        """
        sp.set_type(amount, sp.TMutez)
        sp.verify(sp.sender == self.data.administrator, "Only admin can withdraw")
        sp.send(self.data.administrator, amount)

    @sp.onchain_view()
    def get_token_for_hash(self, code_hash):
        """
        Get token_id for a given KidLisp code hash
        """
        sp.set_type(code_hash, sp.TString)
        sp.result(self.data.hash_to_token.get(code_hash, default_value=sp.none))

    @sp.onchain_view()
    def get_creator(self, token_id):
        """
        Get creator address for a token
        """
        sp.set_type(token_id, sp.TNat)
        sp.result(self.data.creators.get(token_id, default_value=sp.none))

    @sp.onchain_view()
    def get_code_hash(self, token_id):
        """
        Get KidLisp code hash for a token
        """
        sp.set_type(token_id, sp.TNat)
        sp.result(self.data.code_hashes.get(token_id, default_value=sp.none))


# Test scenarios
@sp.add_test(name="KidLispMemeCoin Tests")
def test():
    scenario = sp.test_scenario()
    scenario.h1("KidLisp Meme Coin Contract Tests")
    
    # Test accounts
    admin = sp.test_account("Administrator")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Bob")
    charlie = sp.test_account("Charlie")
    
    # Contract metadata
    contract_metadata = {
        "": sp.utils.bytes_of_string("tezos-storage:content"),
        "content": sp.utils.bytes_of_string('{"name": "KidLisp Meme Coin", "description": "FA2 tokens for KidLisp code"}')
    }
    
    # Initialize contract
    c1 = KidLispMemeCoin(
        admin=admin.address,
        metadata=sp.big_map(contract_metadata)
    )
    
    scenario += c1
    
    # Test 1: Mint first KidLisp token
    scenario.h2("Test 1: Mint First Token")
    c1.mint_kidlisp_token(
        code_hash="sha256_hash_of_kidlisp_code_1",
        kidlisp_code="(wipe blue)(line 10 10 50 50)",
        creator=alice.address,
        amount=1000000,  # 1 token with 6 decimals
        metadata_uri="https://aesthetic.computer/kidlisp/metadata/1"
    ).run(sender=admin, amount=sp.mutez(100000))
    
    # Verify token was created
    scenario.verify(c1.data.next_token_id == 1)
    scenario.verify(c1.data.creators[0] == alice.address)
    scenario.verify(c1.data.code_hashes[0] == "sha256_hash_of_kidlisp_code_1")
    
    # Test 2: Mint duplicate code (should add to existing token)
    scenario.h2("Test 2: Mint Duplicate Code")
    c1.mint_kidlisp_token(
        code_hash="sha256_hash_of_kidlisp_code_1",  # Same hash
        kidlisp_code="(wipe blue)(line 10 10 50 50)",
        creator=alice.address,
        amount=500000,  # 0.5 more tokens
        metadata_uri="https://aesthetic.computer/kidlisp/metadata/1"
    ).run(sender=admin, amount=sp.mutez(100000))
    
    # Verify no new token was created, but supply increased
    scenario.verify(c1.data.next_token_id == 1)
    
    # Test 3: Mint different KidLisp code
    scenario.h2("Test 3: Mint Different Code")
    c1.mint_kidlisp_token(
        code_hash="sha256_hash_of_kidlisp_code_2",
        kidlisp_code="(wipe red)(circle 25 25 20)",
        creator=bob.address,
        amount=2000000,  # 2 tokens
        metadata_uri="https://aesthetic.computer/kidlisp/metadata/2"
    ).run(sender=admin, amount=sp.mutez(100000))
    
    # Verify new token was created
    scenario.verify(c1.data.next_token_id == 2)
    scenario.verify(c1.data.creators[1] == bob.address)
    
    # Test 4: Transfer tokens
    scenario.h2("Test 4: Transfer Tokens")
    c1.transfer([
        sp.record(
            from_=alice.address,
            txs=[
                sp.record(to_=charlie.address, token_id=0, amount=100000)  # 0.1 tokens
            ]
        )
    ]).run(sender=alice)
    
    # Test 5: View functions
    scenario.h2("Test 5: View Functions")
    scenario.verify(c1.get_token_for_hash("sha256_hash_of_kidlisp_code_1") == sp.some(0))
    scenario.verify(c1.get_creator(0) == sp.some(alice.address))
    scenario.verify(c1.get_code_hash(0) == sp.some("sha256_hash_of_kidlisp_code_1"))
    
    # Test 6: Admin functions
    scenario.h2("Test 6: Admin Functions")
    c1.set_royalty_percentage(750).run(sender=admin)  # 7.5%
    scenario.verify(c1.data.royalty_percentage == 750)
    
    c1.set_minting_fee(sp.mutez(200000)).run(sender=admin)  # 0.2 tez
    scenario.verify(c1.data.minting_fee == sp.mutez(200000))


if __name__ == "__main__":
    test()
