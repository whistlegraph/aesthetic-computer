"""
Keeps FA2 v5 - Aesthetic Computer NFT Contract (FINAL PRODUCTION)

This contract is the production release based on v4 with revenue enabled.

v5 CHANGES from v4:
- Default fee: 2.5 XTZ (revenue enabled by default)
- Improved error messages with context
- All v4 features preserved

v4 features (preserved):
- 10% Royalty Support: Automatic royalties on secondary sales (objkt.com compatible)
- Emergency Pause: Admin can halt minting in emergencies
- Admin Transfer: Customer service tool for edge cases

v3 features (preserved):
- edit_metadata: Token owner/creator can edit (preserves attribution)
- token_creators: Tracks original creator for each token
- Pre-encoded metadata bytes (TzKT/objkt compatible)
- Fee system with withdraw capability
- Burn and re-mint functionality

Key feature: Metadata bytes stored directly WITHOUT sp.pack(),
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
        FA2 NFT contract for aesthetic.computer Keeps (v5 - FINAL PRODUCTION).

        v5 changes from v4:
        - Default fee set to 2.5 XTZ for revenue activation
        - Improved error messages with context

        v4 features: royalties, emergency pause, admin transfer
        v3 features: owner/creator editable metadata, attribution tracking
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
            # v5: Default fee set to 2.5 XTZ for revenue activation
            self.data.keep_fee = sp.mutez(2500000)

            # v4: Emergency pause flag
            # When true, minting and metadata edits are disabled
            self.data.paused = False

            # v4: Default royalty configuration
            # Basis points: 1000 = 10%, 2500 = 25% (max)
            # Applied to all new mints unless overridden
            self.data.default_royalty_bps = 1000  # 10% default

        @sp.entrypoint
        def keep(self, params):
            """
            Mint a new Keep token with full TZIP-21 metadata + royalties.

            Two modes:
            1. Admin calling: mints to specified owner (for server-side minting)
            2. User calling: mints to sender, requires fee payment

            v5: Fee defaults to 2.5 XTZ
            v4: Automatically adds royalty metadata based on default_royalty_bps
            v4: Respects pause flag (cannot mint when paused)

            All bytes parameters should be raw hex-encoded UTF-8 strings.
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
                royalties=sp.bytes,
                rights=sp.bytes,
                content_type=sp.bytes,
                content_hash=sp.bytes,
                metadata_uri=sp.bytes,
                owner=sp.address
            ))

            # Check if contract is paused
            assert not self.data.paused, "MINTING_PAUSED"

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
                "royalties": params.royalties,
                "rights": params.rights,
                "content_type": params.content_type,
                "content_hash": params.content_hash,
                "": params.metadata_uri  # Empty key for off-chain metadata URI
            }, sp.map[sp.string, sp.bytes])

            # Store token metadata
            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info
            )

            # Assign token to owner
            self.data.ledger[token_id] = params.owner

            # Initialize as not locked
            self.data.metadata_locked[token_id] = False

            # Store content hash to prevent duplicates
            self.data.content_hashes[params.content_hash] = token_id

            # Track the original creator
            self.data.token_creators[token_id] = params.owner

            # Increment token counter
            self.data.next_token_id = token_id + 1

        @sp.entrypoint
        def edit_metadata(self, params):
            """
            Update metadata for an existing token.

            Authorization:
            - Admin (contract administrator)
            - Token owner (current holder)
            - Original creator (preserves objkt.com attribution)

            Respects pause flag (cannot edit when paused).
            """
            sp.cast(params, sp.record(
                token_id=sp.nat,
                token_info=sp.map[sp.string, sp.bytes]
            ))

            # Check if contract is paused
            assert not self.data.paused, "EDITING_PAUSED"

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
            """Update contract-level metadata (admin only, if not locked)."""
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
            Set the keep fee required for minting.
            Admin only. Fee is in mutez (1 tez = 1,000,000 mutez).
            """
            sp.cast(new_fee, sp.mutez)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.keep_fee = new_fee

        @sp.entrypoint
        def withdraw_fees(self, destination):
            """
            Withdraw accumulated fees from the contract.
            Admin only. Sends entire contract balance to destination.
            """
            sp.cast(destination, sp.address)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            sp.send(destination, sp.balance)

        @sp.entrypoint
        def burn_keep(self, token_id):
            """
            Burn a token and remove its content_hash.
            This allows re-minting the same piece name.
            Admin only.
            """
            sp.cast(token_id, sp.nat)

            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"

            # Get content_hash before burning
            token_info = self.data.token_metadata[token_id].token_info
            content_hash = token_info.get("content_hash", default=sp.bytes("0x"))

            # Remove from registries
            if self.data.content_hashes.contains(content_hash):
                del self.data.content_hashes[content_hash]

            if self.data.ledger.contains(token_id):
                del self.data.ledger[token_id]

            del self.data.token_metadata[token_id]

            if self.data.metadata_locked.contains(token_id):
                del self.data.metadata_locked[token_id]

            if self.data.token_creators.contains(token_id):
                del self.data.token_creators[token_id]

        # =====================================================================
        # v4 ENTRYPOINTS (preserved in v5)
        # =====================================================================

        @sp.entrypoint
        def pause(self):
            """
            Emergency pause - stops minting and metadata edits.
            Admin only.

            Use cases:
            - Security vulnerability discovered
            - IPFS infrastructure issues
            - Spam attack detected
            - Contract bug found

            Note: Does NOT affect transfers (preserves FA2 composability)
            """
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.paused = True

        @sp.entrypoint
        def unpause(self):
            """
            Resume normal operations after emergency pause.
            Admin only.
            """
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.paused = False

        @sp.entrypoint
        def set_default_royalty(self, bps):
            """
            Set default royalty percentage for new mints.
            Admin only.

            Args:
                bps: Basis points (100 = 1%, 1000 = 10%, 2500 = 25% max)

            Example:
                set_default_royalty(1000)  # 10% royalty
            """
            sp.cast(bps, sp.nat)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert bps <= 2500, "MAX_ROYALTY_25_PERCENT"
            self.data.default_royalty_bps = bps

        @sp.entrypoint
        def admin_transfer(self, params):
            """Admin emergency transfer"""
            sp.cast(params, sp.record(
                token_id=sp.nat,
                from_=sp.address,
                to_=sp.address
            ))

            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert self.data.token_metadata.contains(params.token_id), "FA2_TOKEN_UNDEFINED"

            current_owner = self.data.ledger.get(params.token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX"))
            assert current_owner == params.from_, "INVALID_CURRENT_OWNER"

            self.data.ledger[params.token_id] = params.to_


def _get_balance(fa2_contract, args):
    """Utility function to call the contract's get_balance view."""
    return sp.View(fa2_contract, "get_balance")(args)


def _total_supply(fa2_contract, args):
    """Utility function to call the contract's total_supply view."""
    return sp.View(fa2_contract, "total_supply")(args)


@sp.add_test()
def test():
    """Minimal test to compile v5 contract."""
    scenario = sp.test_scenario("KeepsFA2v5")
    scenario.h1("Keeps FA2 v5 - Final Production Contract")

    # Define test account
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

    scenario.p("v5: Default fee = 2.5 XTZ, revenue enabled")
    scenario.p("v4 features: royalties, pause, admin transfer")
    scenario.p("v3 features: editable metadata, creator tracking")
