"""
Keeps FA2 v3 - Aesthetic Computer NFT Contract

This contract extends the SmartPy FA2 library with a custom `keep` entrypoint
that accepts pre-encoded bytes for all TZIP-21 metadata fields.

v3 Changes from v2:
- edit_metadata: Now allows token OWNER to edit (not just admin)
  This preserves artist attribution on objkt.com when updating metadata.
- Added token_creators bigmap to track original creator for each token

Key feature: Metadata bytes are stored directly WITHOUT sp.pack(),
ensuring compatibility with TzKT, objkt, and other Tezos indexers.
"""

import smartpy as sp
from smartpy.templates import fa2_lib as fa2

main = fa2.main


@sp.module
def keeps_module():
    import main

    # Order of inheritance: [Admin], [<policy>], <base class>, [<other mixins>].
    class KeepsFA2(
        main.Admin,
        main.Nft,
        main.MintNft,
        main.BurnNft,
        main.OnchainviewBalanceOf,
    ):
        """
        FA2 NFT contract for aesthetic.computer Keeps (v3).
        
        Extends the standard FA2 library with a custom `keep` entrypoint
        that accepts all TZIP-21 metadata fields as pre-encoded bytes.
        
        v3: Token owners can edit their own token metadata.
        """
        
        def __init__(self, admin_address, contract_metadata, ledger, token_metadata):
            # Initialize on-chain balance view
            main.OnchainviewBalanceOf.__init__(self)
            
            # Initialize the NFT-specific entrypoints
            main.BurnNft.__init__(self)
            main.MintNft.__init__(self)
            
            # Initialize the NFT base class
            main.Nft.__init__(self, contract_metadata, ledger, token_metadata)
            
            # Initialize administrative permissions
            main.Admin.__init__(self, admin_address)
            
            # Additional storage for metadata locking
            self.data.metadata_locked = sp.cast(
                sp.big_map(),
                sp.big_map[sp.nat, sp.bool]
            )
            
            # Track content hashes to prevent duplicate mints
            # Maps content_hash (bytes) -> token_id (nat)
            self.data.content_hashes = sp.cast(
                sp.big_map(),
                sp.big_map[sp.bytes, sp.nat]
            )
            
            # Track original creator for each token (v3)
            # Maps token_id -> creator address (the first minter)
            self.data.token_creators = sp.cast(
                sp.big_map(),
                sp.big_map[sp.nat, sp.address]
            )
            
            # Contract-level metadata lock flag
            self.data.contract_metadata_locked = False
            
            # Mint fee configuration (admin-adjustable)
            # Default: 0 tez (free minting via admin key)
            # Can be set to require payment when users mint via their wallets
            self.data.keep_fee = sp.tez(0)
        
        @sp.entrypoint
        def keep(self, params):
            """
            Mint a new Keep token with full TZIP-21 metadata.
            
            Two modes:
            1. Admin calling with owner param: mints to specified owner (for server-side minting)
            2. User calling directly: mints to sender, requires fee payment
               - This makes the USER the firstMinter for proper artist attribution
            
            All bytes parameters should be raw hex-encoded UTF-8 strings
            (NOT Michelson-packed with 0x05 prefix).
            """
            sp.cast(params, sp.record(
                name=sp.bytes,
                description=sp.bytes,
                artifactUri=sp.bytes,
                displayUri=sp.bytes,
                thumbnailUri=sp.bytes,
                decimals=sp.bytes,
                symbol=sp.bytes,
                isBooleanAmount=sp.bytes,
                shouldPreferSymbol=sp.bytes,
                formats=sp.bytes,
                tags=sp.bytes,
                attributes=sp.bytes,
                creators=sp.bytes,
                rights=sp.bytes,
                content_type=sp.bytes,
                content_hash=sp.bytes,
                metadata_uri=sp.bytes,
                owner=sp.address
            ))
            
            # Determine minting mode and owner
            is_admin = self.is_administrator_()
            
            # Non-admin callers must pay the fee and can only mint to themselves
            if not is_admin:
                assert sp.amount >= self.data.keep_fee, "INSUFFICIENT_FEE"
                # User must mint to themselves (ensures they are firstMinter)
                assert params.owner == sp.sender, "MUST_MINT_TO_SELF"
            
            # Check for duplicate content hash
            assert not self.data.content_hashes.contains(params.content_hash), "DUPLICATE_CONTENT_HASH"
            
            # Get next token ID from the library's counter
            token_id = self.data.next_token_id
            
            # Build token_info map with all TZIP-21 fields
            # Values are stored as raw bytes (no packing)
            token_info = sp.cast({
                "name": params.name,
                "description": params.description,
                "artifactUri": params.artifactUri,
                "displayUri": params.displayUri,
                "thumbnailUri": params.thumbnailUri,
                "decimals": params.decimals,
                "symbol": params.symbol,
                "isBooleanAmount": params.isBooleanAmount,
                "shouldPreferSymbol": params.shouldPreferSymbol,
                "formats": params.formats,
                "tags": params.tags,
                "attributes": params.attributes,
                "creators": params.creators,
                "rights": params.rights,
                "content_type": params.content_type,
                "content_hash": params.content_hash,
                "": params.metadata_uri  # Empty key for off-chain metadata URI
            }, sp.map[sp.string, sp.bytes])
            
            # Store token metadata using the library's mechanism
            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info
            )
            
            # Assign token to owner using the library's ledger format (NFT: token_id -> owner)
            self.data.ledger[token_id] = params.owner
            
            # Initialize as not locked
            self.data.metadata_locked[token_id] = False
            
            # Store content hash to prevent future duplicates
            self.data.content_hashes[params.content_hash] = token_id
            
            # Track the original creator (v3)
            # Use sender for user mints, owner param for admin mints
            self.data.token_creators[token_id] = params.owner
            
            # Increment token counter
            self.data.next_token_id = token_id + 1
        
        @sp.entrypoint
        def edit_metadata(self, params):
            """
            Update metadata for an existing token.
            
            v3: Can be called by:
            - Admin (contract administrator)
            - Token owner (current holder of the token)
            - Original creator (the address that first minted the token)
            
            This allows artists to update their work without admin intervention,
            preserving proper attribution on marketplaces like objkt.com.
            """
            sp.cast(params, sp.record(
                token_id=sp.nat,
                token_info=sp.map[sp.string, sp.bytes]
            ))
            
            assert self.data.token_metadata.contains(params.token_id), "FA2_TOKEN_UNDEFINED"
            
            # Check authorization: admin, owner, or original creator
            is_admin = self.is_administrator_()
            is_owner = self.data.ledger.get(params.token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")) == sp.sender
            is_creator = self.data.token_creators.get(params.token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")) == sp.sender
            
            assert is_admin or is_owner or is_creator, "NOT_AUTHORIZED"
            
            # Check if locked
            is_locked = self.data.metadata_locked.get(params.token_id, default=False)
            assert not is_locked, "METADATA_LOCKED"
            
            # Update metadata
            self.data.token_metadata[params.token_id] = sp.record(
                token_id=params.token_id,
                token_info=params.token_info
            )
        
        @sp.entrypoint
        def lock_metadata(self, token_id):
            """Permanently lock metadata for a token (admin or owner only)."""
            sp.cast(token_id, sp.nat)
            
            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
            
            # Check authorization: admin or owner
            is_admin = self.is_administrator_()
            is_owner = self.data.ledger.get(token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")) == sp.sender
            
            assert is_admin or is_owner, "NOT_AUTHORIZED"
            
            self.data.metadata_locked[token_id] = True

        @sp.entrypoint
        def set_contract_metadata(self, params):
            """Update contract-level metadata (admin only, if not locked). 
            Use this to set collection icon, banner, description etc.
            params: list of (key, value) pairs to set in the metadata big_map
            """
            sp.cast(params, sp.list[sp.record(key=sp.string, value=sp.bytes)])
            
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert not self.data.contract_metadata_locked, "CONTRACT_METADATA_LOCKED"
            
            for item in params:
                self.data.metadata[item.key] = item.value
        
        @sp.entrypoint
        def lock_contract_metadata(self):
            """Permanently lock contract-level metadata (admin only)."""
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.contract_metadata_locked = True

        @sp.entrypoint
        def set_keep_fee(self, new_fee):
            """
            Set the keep fee required for keeping new tokens.
            Admin only. Fee is in mutez (1 tez = 1,000,000 mutez).
            Set to 0 for free keeping.
            """
            sp.cast(new_fee, sp.mutez)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.keep_fee = new_fee

        @sp.entrypoint
        def withdraw_fees(self, destination):
            """
            Withdraw accumulated fees from the contract.
            Admin only. Sends the entire contract balance to the destination.
            """
            sp.cast(destination, sp.address)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            sp.send(destination, sp.balance)

        @sp.entrypoint
        def burn_keep(self, token_id):
            """
            Burn a token and remove its content_hash from the registry.
            This allows the same piece name to be minted again.
            Admin only.
            """
            sp.cast(token_id, sp.nat)
            
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
            
            # Get the content_hash before burning so we can remove it
            token_info = self.data.token_metadata[token_id].token_info
            content_hash = token_info.get("content_hash", default=sp.bytes("0x"))
            
            # Remove from content_hashes registry (allows re-minting this piece)
            if self.data.content_hashes.contains(content_hash):
                del self.data.content_hashes[content_hash]
            
            # Remove from ledger
            if self.data.ledger.contains(token_id):
                del self.data.ledger[token_id]
            
            # Remove token metadata
            del self.data.token_metadata[token_id]
            
            # Remove metadata lock entry
            if self.data.metadata_locked.contains(token_id):
                del self.data.metadata_locked[token_id]
            
            # Remove creator entry (v3)
            if self.data.token_creators.contains(token_id):
                del self.data.token_creators[token_id]

        @sp.onchain_view()
        def get_token_creator(self, token_id):
            """Get the original creator of a token."""
            sp.cast(token_id, sp.nat)
            return self.data.token_creators.get(token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX"))


def _get_balance(fa2_contract, args):
    """Utility function to call the contract's get_balance view."""
    return sp.View(fa2_contract, "get_balance")(args)


def _total_supply(fa2_contract, args):
    """Utility function to call the contract's total_supply view."""
    return sp.View(fa2_contract, "total_supply")(args)


@sp.add_test()
def test():
    """Simple test to compile the contract."""
    scenario = sp.test_scenario("KeepsFA2v3")
    scenario.h1("Keeps FA2 v3 - Compile Test")
    
    # Define test accounts
    admin = sp.test_account("Admin")
    
    # Create empty initial state
    ledger = {}
    token_metadata = []
    
    # Deploy contract
    contract = keeps_module.KeepsFA2(
        admin.address,
        sp.big_map(),
        ledger,
        token_metadata
    )
    
    scenario += contract
    
    # Just verify it compiles - actual tests done manually
    scenario.h2("Contract compiled successfully")
    scenario.p("v3 features: token owner/creator can edit metadata")

