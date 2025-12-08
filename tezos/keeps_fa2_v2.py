"""
Keeps FA2 v2 - Aesthetic Computer NFT Contract

This contract extends the SmartPy FA2 library with a custom `keep` entrypoint
that accepts pre-encoded bytes for all TZIP-21 metadata fields.

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
        FA2 NFT contract for aesthetic.computer Keeps.
        
        Extends the standard FA2 library with a custom `keep` entrypoint
        that accepts all TZIP-21 metadata fields as pre-encoded bytes.
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
        
        @sp.entrypoint
        def keep(self, params):
            """
            Mint a new Keep token with full TZIP-21 metadata.
            
            All bytes parameters should be raw hex-encoded UTF-8 strings
            (NOT Michelson-packed with 0x05 prefix).
            
            This uses sp.scenario_utils.bytes_of_string() style encoding,
            which is just UTF-8 bytes encoded as hex, ensuring proper
            indexer recognition.
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
            
            # Only admin can mint
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            
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
            
            # Increment token counter
            self.data.next_token_id = token_id + 1
        
        @sp.entrypoint
        def edit_metadata(self, params):
            """Update metadata for an existing token (admin only, if not locked)."""
            sp.cast(params, sp.record(
                token_id=sp.nat,
                token_info=sp.map[sp.string, sp.bytes]
            ))
            
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert self.data.token_metadata.contains(params.token_id), "FA2_TOKEN_UNDEFINED"
            
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
            """Permanently lock metadata for a token (admin only)."""
            sp.cast(token_id, sp.nat)
            
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
            
            self.data.metadata_locked[token_id] = True


def _get_balance(fa2_contract, args):
    """Utility function to call the contract's get_balance view."""
    return sp.View(fa2_contract, "get_balance")(args)


def _total_supply(fa2_contract, args):
    """Utility function to call the contract's total_supply view."""
    return sp.View(fa2_contract, "total_supply")(args)


@sp.add_test()
def test():
    scenario = sp.test_scenario("KeepsFA2v2")
    scenario.h1("Keeps FA2 v2 Tests")
    
    # Define test accounts
    admin = sp.test_account("Admin")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Bob")
    
    # Create empty initial state
    ledger = {}
    token_metadata = []
    
    # Deploy contract
    contract = keeps_module.KeepsFA2(
        admin.address,
        sp.big_map(),  # contract_metadata - will be set below
        ledger,
        token_metadata
    )
    
    # Build contract metadata
    contract_metadata = sp.create_tzip16_metadata(
        name="Aesthetic.computer Keeps",
        description="FA2 NFT contract for aesthetic.computer keeps",
        version="2.0.0",
        license_name="MIT",
        interfaces=["TZIP-012", "TZIP-016", "TZIP-021"],
        authors=["aesthetic.computer"],
        homepage="https://aesthetic.computer",
        source_uri=None,
        offchain_views=contract.get_offchain_views(),
    )
    
    contract_metadata["permissions"] = {
        "operator": "owner-or-operator-transfer",
        "receiver": "owner-no-hook",
        "sender": "owner-no-hook",
    }
    
    # Placeholder metadata URI
    metadata_uri = "ipfs://example"
    contract.data.metadata = sp.scenario_utils.metadata_of_url(metadata_uri)
    
    scenario += contract
    
    if scenario.simulation_mode() is sp.SimulationMode.MOCKUP:
        scenario.p("mockups - skipping transfer tests")
        return
    
    scenario.h2("Mint token using keep entrypoint")
    
    # Helper to convert string to bytes (same as sp.scenario_utils.bytes_of_string)
    def str_to_bytes(s):
        return sp.scenario_utils.bytes_of_string(s)
    
    # Mint a token
    contract.keep(
        name=str_to_bytes("Test Token"),
        description=str_to_bytes("A test token for aesthetic.computer"),
        artifactUri=str_to_bytes("ipfs://QmXYZ"),
        displayUri=str_to_bytes("ipfs://QmXYZ"),
        thumbnailUri=str_to_bytes("ipfs://QmXYZ"),
        decimals=str_to_bytes("0"),
        symbol=str_to_bytes("KEEP"),
        isBooleanAmount=str_to_bytes("true"),
        shouldPreferSymbol=str_to_bytes("false"),
        formats=str_to_bytes("[]"),
        tags=str_to_bytes("[]"),
        attributes=str_to_bytes("[]"),
        creators=str_to_bytes('["aesthetic.computer"]'),
        rights=str_to_bytes("© All rights reserved."),
        content_type=str_to_bytes("kidlisp"),
        content_hash=str_to_bytes("QmXYZ"),
        metadata_uri=str_to_bytes("https://example.com/metadata.json"),
        owner=alice.address,
        _sender=admin,
    )
    
    # Verify token exists
    scenario.verify(contract.data.next_token_id == 1)
    scenario.verify(_get_balance(contract, sp.record(owner=alice.address, token_id=0)) == 1)
    scenario.verify(_total_supply(contract, sp.record(token_id=0)) == 1)
    
    scenario.h2("Transfer token")
    contract.transfer(
        [
            sp.record(
                from_=alice.address,
                txs=[sp.record(to_=bob.address, amount=1, token_id=0)],
            ),
        ],
        _sender=alice,
    )
    
    scenario.verify(_get_balance(contract, sp.record(owner=alice.address, token_id=0)) == 0)
    scenario.verify(_get_balance(contract, sp.record(owner=bob.address, token_id=0)) == 1)
    
    scenario.h2("Mint second token")
    contract.keep(
        name=str_to_bytes("Token Two"),
        description=str_to_bytes("Second test token"),
        artifactUri=str_to_bytes("ipfs://QmABC"),
        displayUri=str_to_bytes("ipfs://QmABC"),
        thumbnailUri=str_to_bytes("ipfs://QmABC"),
        decimals=str_to_bytes("0"),
        symbol=str_to_bytes("KEEP"),
        isBooleanAmount=str_to_bytes("true"),
        shouldPreferSymbol=str_to_bytes("false"),
        formats=str_to_bytes("[]"),
        tags=str_to_bytes("[]"),
        attributes=str_to_bytes("[]"),
        creators=str_to_bytes('["aesthetic.computer"]'),
        rights=str_to_bytes("©"),
        content_type=str_to_bytes("kidlisp"),
        content_hash=str_to_bytes("QmABC"),
        metadata_uri=str_to_bytes("https://example.com/metadata2.json"),
        owner=admin.address,
        _sender=admin,
    )
    
    scenario.verify(contract.data.next_token_id == 2)
    
    scenario.h2("Lock metadata")
    contract.lock_metadata(1, _sender=admin)
    scenario.verify(contract.data.metadata_locked[1] == True)
    
    scenario.h2("Try to edit locked metadata - should fail")
    contract.edit_metadata(
        token_id=1,
        token_info=sp.cast({
            "name": str_to_bytes("New Name")
        }, sp.map[sp.string, sp.bytes]),
        _sender=admin,
        _valid=False,
    )
    
    scenario.h2("Non-admin cannot mint")
    contract.keep(
        name=str_to_bytes("Unauthorized"),
        description=str_to_bytes("Should fail"),
        artifactUri=str_to_bytes("ipfs://Qm"),
        displayUri=str_to_bytes("ipfs://Qm"),
        thumbnailUri=str_to_bytes("ipfs://Qm"),
        decimals=str_to_bytes("0"),
        symbol=str_to_bytes("KEEP"),
        isBooleanAmount=str_to_bytes("true"),
        shouldPreferSymbol=str_to_bytes("false"),
        formats=str_to_bytes("[]"),
        tags=str_to_bytes("[]"),
        attributes=str_to_bytes("[]"),
        creators=str_to_bytes("[]"),
        rights=str_to_bytes(""),
        content_type=str_to_bytes("text"),
        content_hash=str_to_bytes(""),
        metadata_uri=str_to_bytes(""),
        owner=bob.address,
        _sender=bob,  # Bob is not admin
        _valid=False,
    )
