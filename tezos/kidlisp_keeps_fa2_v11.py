"""
KidLisp Keeps FA2 v11 - Aesthetic Computer NFT Contract

v11 CHANGES from v10:
- Admin minting path removed from keep — only user self-minting allowed.
  Users must submit the transaction themselves and pay the fee directly.
  The backend's role is permit issuance only, not transaction submission.
  This eliminates the risk of a compromised admin key minting for free.
- owner parameter removed from keep — always sp.sender.
  Permits now bind: contract + sender + content_hash + deadline.
- token_creators[id] = sp.sender (always the minter, never ambiguous).
- withdraw_fees docstring updated (no longer mentions admin-path mints).

Inherited from v10:
- admin_transfer removed — admin cannot move tokens on behalf of owners
- lock_metadata is owner-only — admin cannot freeze without owner consent
- withdraw_fees — fees accumulate in-contract, pulled to treasury
- keep requires backend-issued signature (keep permit)
- keep signature binds: contract + sender + content_hash + deadline
- owner-only burn_keep
- creator refresh metadata policy (v7)
- immutable fields: content_hash + royalties
- emergency pause/unpause (does NOT affect transfers)
- default fee: 2.5 XTZ
"""

import smartpy as sp
from smartpy.templates import fa2_lib as fa2

main = fa2.main


@sp.module
def keeps_module():
    import main

    KEEP_PERMIT_SIGNER = sp.key("edpktwf7pNMMfRcMoxHANoFtJLgGhJLwTsiSqaEMB2CnnSDTeLKoF6")

    class KidLispKeepsFA2v11(
        main.Admin,
        main.Nft,
        main.OnchainviewBalanceOf,
    ):
        """
        FA2 NFT contract for KidLisp Keeps (v11).

        v11 changes from v10:
        - Admin minting path removed: keep is user-only.
          All mints require the sender to pay the fee and mint to themselves.
          Backend issues permits; users submit transactions from their own wallets.
        - owner field removed from keep params — always sp.sender.
        - token_creators[id] = sp.sender (unambiguous).
        """

        def __init__(self, admin_address, treasury_address, contract_metadata, ledger, token_metadata):
            main.OnchainviewBalanceOf.__init__(self)
            main.Nft.__init__(self, contract_metadata, ledger, token_metadata)
            main.Admin.__init__(self, admin_address)

            # Fee destination — used by withdraw_fees, not sent at mint
            self.data.treasury_address = sp.cast(treasury_address, sp.address)

            # Metadata locking per token
            self.data.metadata_locked = sp.cast(
                sp.big_map(),
                sp.big_map[sp.nat, sp.bool]
            )

            # Content hash registry — prevents duplicate mints
            # Maps content_hash (bytes) -> token_id (nat)
            self.data.content_hashes = sp.cast(
                sp.big_map(),
                sp.big_map[sp.bytes, sp.nat]
            )

            # Original creator per token — used for creator refresh path
            # Maps token_id -> creator address (always the minting sender)
            self.data.token_creators = sp.cast(
                sp.big_map(),
                sp.big_map[sp.nat, sp.address]
            )

            # Contract-level metadata lock
            self.data.contract_metadata_locked = False

            # Mint fee — accumulates in-contract, pulled via withdraw_fees
            self.data.keep_fee = sp.mutez(2500000)  # 2.5 XTZ

            # Emergency pause — stops minting and metadata edits only
            # Transfers are always unaffected
            self.data.paused = False

            # Royalty split — informational, read by backend at mint time.
            # Not enforced on-chain; royalties are passed as bytes in keep params.
            # Backend builds objkt-standard royalties JSON:
            #   { "decimals": 4, "shares": { artist: 900, platform: 100 } }
            # Total: 10% (9% artist + 1% platform). Decimals: 4 => out of 10000.
            self.data.artist_royalty_bps = 900   # 9%
            self.data.platform_royalty_bps = 100  # 1% — goes to treasury_address

        @sp.entrypoint
        def keep(self, params):
            """
            Mint a new Keep token. User-only — no admin path.

            The caller must:
            - Pay at least keep_fee XTZ with the transaction.
            - Hold a valid backend-issued keep permit.

            The token is always minted to sp.sender.
            Permit binds: contract + sender + content_hash + deadline.
            """
            sp.cast(params, sp.record(
                name=sp.bytes,
                symbol=sp.bytes,
                description=sp.bytes,
                artifactUri=sp.bytes,
                displayUri=sp.bytes,
                thumbnailUri=sp.bytes,
                decimals=sp.bytes,
                creators=sp.bytes,
                royalties=sp.bytes,
                content_hash=sp.bytes,
                metadata_uri=sp.bytes,
                permit_deadline=sp.timestamp,
                keep_permit=sp.signature
            ))

            assert not self.data.paused, "MINTING_PAUSED"
            assert sp.amount >= self.data.keep_fee, "INSUFFICIENT_FEE"

            # Verify signed keep permit
            permit_payload = sp.record(
                contract=sp.self_address,
                owner=sp.sender,
                content_hash=params.content_hash,
                permit_deadline=params.permit_deadline,
            )
            sp.cast(
                permit_payload,
                sp.record(
                    contract=sp.address,
                    owner=sp.address,
                    content_hash=sp.bytes,
                    permit_deadline=sp.timestamp,
                ).layout(("contract", ("owner", ("content_hash", "permit_deadline"))))
            )
            assert sp.now <= params.permit_deadline, "PERMIT_EXPIRED"
            assert sp.check_signature(
                KEEP_PERMIT_SIGNER,
                params.keep_permit,
                sp.pack(permit_payload)
            ), "INVALID_KEEP_PERMIT"

            assert not self.data.content_hashes.contains(params.content_hash), "DUPLICATE_CONTENT_HASH"

            token_id = self.data.next_token_id

            token_info = sp.cast({
                "name": params.name,
                "symbol": params.symbol,
                "description": params.description,
                "artifactUri": params.artifactUri,
                "displayUri": params.displayUri,
                "thumbnailUri": params.thumbnailUri,
                "decimals": params.decimals,
                "creators": params.creators,
                "royalties": params.royalties,
                "content_hash": params.content_hash,
                "metadata_uri": params.metadata_uri,
                "": params.metadata_uri
            }, sp.map[sp.string, sp.bytes])

            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info
            )

            self.data.ledger[token_id] = sp.sender
            self.data.metadata_locked[token_id] = False
            self.data.content_hashes[params.content_hash] = token_id
            self.data.token_creators[token_id] = sp.sender
            self.data.next_token_id = token_id + 1
            # Fee accumulates in-contract — use withdraw_fees to pull to treasury

        @sp.entrypoint
        def mint(self, batch):
            """Disabled — use keep."""
            sp.cast(
                batch,
                sp.list[
                    sp.record(
                        to_=sp.address,
                        metadata=sp.map[sp.string, sp.bytes],
                    ).layout(("to_", "metadata"))
                ],
            )
            assert False, "MINT_DISABLED_USE_KEEP"

        @sp.entrypoint
        def burn(self, batch):
            """Disabled — use burn_keep."""
            sp.cast(
                batch,
                sp.list[
                    sp.record(
                        from_=sp.address,
                        token_id=sp.nat,
                        amount=sp.nat,
                    ).layout(("from_", ("token_id", "amount")))
                ],
            )
            assert False, "BURN_DISABLED_USE_BURN_KEEP"

        @sp.entrypoint
        def edit_metadata(self, params):
            """
            Update metadata for an existing token.

            Authorization:
            - Current token owner: full metadata edit
            - Original creator: refresh-only (URI/presentation fields only)

            content_hash and royalties are always preserved from original mint.
            """
            sp.cast(params, sp.record(
                token_id=sp.nat,
                token_info=sp.map[sp.string, sp.bytes]
            ))

            assert not self.data.paused, "EDITING_PAUSED"
            assert self.data.token_metadata.contains(params.token_id), "FA2_TOKEN_UNDEFINED"

            is_owner = self.data.ledger.get(params.token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")) == sp.sender
            is_creator = self.data.token_creators.get(params.token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")) == sp.sender

            assert is_owner or is_creator, "NOT_AUTHORIZED"

            is_locked = self.data.metadata_locked.get(params.token_id, default=False)
            assert not is_locked, "METADATA_LOCKED"

            existing_info = self.data.token_metadata[params.token_id].token_info
            original_hash = existing_info.get("content_hash", default=sp.bytes("0x"))
            original_royalties = existing_info.get("royalties", default=sp.bytes("0x"))

            if is_owner:
                self.data.token_metadata[params.token_id] = sp.record(
                    token_id=params.token_id,
                    token_info=params.token_info
                )
            else:
                # Creator refresh path: URI/presentation fields only
                refreshed_info = existing_info
                mutable_refresh_fields = [
                    "",
                    "metadata_uri",
                    "artifactUri",
                    "displayUri",
                    "thumbnailUri",
                    "formats",
                    "tags",
                    "attributes",
                    "rights",
                    "content_type",
                    "isBooleanAmount",
                    "shouldPreferSymbol",
                ]
                for field in mutable_refresh_fields:
                    if params.token_info.contains(field):
                        refreshed_info[field] = params.token_info[field]

                # Keep "" and metadata_uri aligned
                if params.token_info.contains("metadata_uri"):
                    refreshed_info[""] = params.token_info["metadata_uri"]
                if params.token_info.contains(""):
                    refreshed_info["metadata_uri"] = params.token_info[""]

                self.data.token_metadata[params.token_id] = sp.record(
                    token_id=params.token_id,
                    token_info=refreshed_info
                )

            # Always re-inject immutable fields
            self.data.token_metadata[params.token_id].token_info["content_hash"] = original_hash
            self.data.token_metadata[params.token_id].token_info["royalties"] = original_royalties

        @sp.entrypoint
        def lock_metadata(self, token_id):
            """
            Permanently lock token metadata. Owner only. Irreversible.
            Admin intentionally excluded — only the token owner can freeze their own metadata.
            """
            sp.cast(token_id, sp.nat)

            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"

            is_owner = self.data.ledger.get(token_id, default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")) == sp.sender
            assert is_owner, "NOT_TOKEN_OWNER"

            self.data.metadata_locked[token_id] = True

        @sp.entrypoint
        def burn_keep(self, token_id):
            """
            Burn a token and free its content_hash for re-minting.
            Owner only.
            """
            sp.cast(token_id, sp.nat)

            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
            current_owner = self.data.ledger.get(
                token_id,
                default=sp.address("tz1burnburnburnburnburnburnburjAYjjX")
            )
            assert current_owner == sp.sender, "NOT_TOKEN_OWNER"

            token_info = self.data.token_metadata[token_id].token_info
            content_hash = token_info.get("content_hash", default=sp.bytes("0x"))

            if self.data.content_hashes.contains(content_hash):
                del self.data.content_hashes[content_hash]

            if self.data.ledger.contains(token_id):
                del self.data.ledger[token_id]

            del self.data.token_metadata[token_id]

            if self.data.metadata_locked.contains(token_id):
                del self.data.metadata_locked[token_id]

            if self.data.token_creators.contains(token_id):
                del self.data.token_creators[token_id]

        @sp.entrypoint
        def set_contract_metadata(self, params):
            """Update contract-level metadata. Admin only, if not locked."""
            sp.cast(params, sp.list[sp.record(key=sp.string, value=sp.bytes)])

            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert not self.data.contract_metadata_locked, "CONTRACT_METADATA_LOCKED"

            for item in params:
                self.data.metadata[item.key] = item.value

        @sp.entrypoint
        def lock_contract_metadata(self):
            """Permanently lock contract-level metadata. Admin only."""
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.contract_metadata_locked = True

        @sp.entrypoint
        def withdraw_fees(self, destination):
            """
            Withdraw all accumulated keep fees to destination.
            Admin only.
            """
            sp.cast(destination, sp.address)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            sp.send(destination, sp.balance)

        @sp.entrypoint
        def set_keep_fee(self, new_fee):
            """Update the keep fee. Admin only. Fee in mutez."""
            sp.cast(new_fee, sp.mutez)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.keep_fee = new_fee

        @sp.entrypoint
        def set_treasury(self, new_treasury):
            """Update the fee treasury address. Admin only."""
            sp.cast(new_treasury, sp.address)
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.treasury_address = new_treasury

        @sp.entrypoint
        def pause(self):
            """
            Emergency pause — stops minting and metadata edits.
            Does NOT affect FA2 transfers.
            Admin only.
            """
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.paused = True

        @sp.entrypoint
        def unpause(self):
            """Resume normal operations. Admin only."""
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            self.data.paused = False

        @sp.entrypoint
        def set_royalty_split(self, params):
            """
            Update the artist/platform royalty split for new mints.
            Admin only. Total must not exceed 2500 bps (25%).
            Backend reads these values when building keep params.
            """
            sp.cast(params, sp.record(artist_bps=sp.nat, platform_bps=sp.nat))
            assert self.is_administrator_(), "FA2_NOT_ADMIN"
            assert params.artist_bps + params.platform_bps <= 2500, "MAX_ROYALTY_25_PERCENT"
            self.data.artist_royalty_bps = params.artist_bps
            self.data.platform_royalty_bps = params.platform_bps


@sp.add_test()
def test():
    scenario = sp.test_scenario("KeepsFA2v11")
    scenario.h1("KidLisp Keeps FA2 v11")

    admin = sp.test_account("Admin")
    treasury = sp.test_account("Treasury")

    ledger = {}
    token_metadata = []

    contract = keeps_module.KidLispKeepsFA2v11(
        admin.address,
        treasury.address,
        sp.big_map(),
        ledger,
        token_metadata
    )

    scenario += contract

    scenario.p("v11: admin minting path removed — user-only keep, backend issues permits only")
    scenario.p("v10: admin_transfer removed, lock_metadata owner-only, fees accumulate via withdraw_fees")
    scenario.p("v9: signed keep permits + owner self-mint enforcement")
    scenario.p("v7: owner full edit + creator refresh-only metadata updates")
    scenario.p("v6: owner-only burn_keep + royalties immutable after keep")
