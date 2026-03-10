"""
KidLisp Keeps FA2 v12 (draft) - trust-minimized design

Design goals:
- No admin role in-contract (no mutable privileged entrypoints).
- User-only minting via commit-reveal (no backend permit signer).
- Owner-only burn_keep.
- content_hash + royalties remain immutable after mint.
- Metadata refresh remains possible with per-token policy controls.
- Contract metadata upgrades/deprecation governed by token holders (no admin key).
- Optional trustless v11->v12 claim migration path.

Important notes:
- This is a draft for design review and testing.
- Royalty bytes are still client-supplied metadata, but become immutable post-mint.
- Commit-reveal is used to reduce mempool front-running risk for content_hash claims.
"""

import smartpy as sp
from smartpy.templates import fa2_lib as fa2

main = fa2.main


@sp.module
def keeps_module():
    import main

    t_commitment_key: type = sp.record(owner=sp.address, commitment=sp.bytes).layout(
        ("owner", "commitment")
    )
    t_contract_metadata_update: type = sp.record(key=sp.string, value=sp.bytes).layout(
        ("key", "value")
    )
    t_contract_upgrade_vote_key: type = sp.record(
        proposal_id=sp.nat, token_id=sp.nat
    ).layout(("proposal_id", "token_id"))
    t_fa2_balance_of_request: type = sp.record(owner=sp.address, token_id=sp.nat).layout(
        ("owner", "token_id")
    )
    t_fa2_balance_of_response: type = sp.record(
        request=t_fa2_balance_of_request,
        balance=sp.nat,
    ).layout(("request", "balance"))
    t_contract_upgrade_proposal: type = sp.record(
        proposer=sp.address,
        created_at=sp.timestamp,
        voting_deadline=sp.timestamp,
        metadata_updates=sp.list[t_contract_metadata_update],
        metadata_updates_hash=sp.bytes,
        successor=sp.option[sp.address],
        deprecate=sp.bool,
        yes_votes=sp.nat,
        no_votes=sp.nat,
        executed=sp.bool,
    ).layout(
        (
            "proposer",
            (
                "created_at",
                (
                    "voting_deadline",
                    (
                        "metadata_updates",
                        (
                            "metadata_updates_hash",
                            ("successor", ("deprecate", ("yes_votes", ("no_votes", "executed")))),
                        ),
                    ),
                ),
            ),
        )
    )

    # Metadata refresh policy constants
    POLICY_OWNER_ONLY = 0
    POLICY_CREATOR_ONLY = 1
    POLICY_OWNER_OR_CREATOR = 2
    POLICY_OWNER_AND_CREATOR = 3

    # Contract lifecycle constants
    CONTRACT_STATE_ACTIVE = 0
    CONTRACT_STATE_DEPRECATED = 1

    BURN_ADDRESS = sp.address("tz1burnburnburnburnburnburnburjAYjjX")

    class KidLispKeepsFA2v12(
        main.Nft,
        main.OnchainviewBalanceOf,
    ):
        """
        v12 draft:
        - No admin role.
        - No permit signer.
        - Commit-reveal keep authorization.
        - Fee forwarded immediately to treasury (no withdraw path).
        - Per-token metadata refresh policy.
        - Holder-governed contract metadata upgrades and deprecation.
        - Trustless v11 owner-claim migration path.
        """

        def __init__(
            self,
            treasury_address,
            migration_source_contract,
            contract_metadata,
            ledger,
            token_metadata,
            keep_fee,
            commitment_min_delay_seconds,
            artist_royalty_bps,
            platform_royalty_bps,
            governance_voting_period_seconds,
            governance_quorum_bps,
            governance_approval_bps,
        ):
            main.OnchainviewBalanceOf.__init__(self)
            main.Nft.__init__(self, contract_metadata, ledger, token_metadata)

            # Immutable-at-deploy policy fields (no admin setters in v12).
            self.data.treasury_address = sp.cast(treasury_address, sp.address)
            self.data.migration_source_contract = sp.cast(
                migration_source_contract, sp.address
            )
            self.data.keep_fee = sp.cast(keep_fee, sp.mutez)
            self.data.commitment_min_delay_seconds = sp.cast(
                commitment_min_delay_seconds, sp.nat
            )
            self.data.artist_royalty_bps = sp.cast(artist_royalty_bps, sp.nat)
            self.data.platform_royalty_bps = sp.cast(platform_royalty_bps, sp.nat)
            self.data.governance_voting_period_seconds = sp.cast(
                governance_voting_period_seconds, sp.nat
            )
            self.data.governance_quorum_bps = sp.cast(governance_quorum_bps, sp.nat)
            self.data.governance_approval_bps = sp.cast(governance_approval_bps, sp.nat)
            assert self.data.governance_voting_period_seconds > 0, "INVALID_VOTING_PERIOD"
            assert self.data.governance_quorum_bps <= 10000, "INVALID_QUORUM_BPS"
            assert self.data.governance_approval_bps <= 10000, "INVALID_APPROVAL_BPS"

            # Content hash registry (dedupe)
            self.data.content_hashes = sp.cast(
                sp.big_map(), sp.big_map[sp.bytes, sp.nat]
            )

            # Token creator address + key (key needed for creator co-sign refresh mode)
            self.data.token_creators = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, sp.address]
            )
            self.data.token_creator_keys = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, sp.key]
            )

            # Owner-controlled metadata lock
            self.data.metadata_locked = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, sp.bool]
            )

            # Per-token refresh policy + nonce for creator co-sign anti-replay
            self.data.refresh_policies = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, sp.nat]
            )
            self.data.refresh_nonces = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, sp.nat]
            )

            # Commit-reveal registry: (owner, commitment) -> registered_at
            self.data.keep_commitments = sp.cast(
                sp.big_map(), sp.big_map[t_commitment_key, sp.timestamp]
            )

            # Token count used for trustless governance denominator.
            self.data.active_token_count = sp.cast(len(ledger), sp.nat)

            # Contract lifecycle state.
            self.data.contract_state = sp.cast(CONTRACT_STATE_ACTIVE, sp.nat)
            self.data.deprecated_successor = sp.cast(None, sp.option[sp.address])
            self.data.deprecated_at = sp.cast(None, sp.option[sp.timestamp])

            # Holder-governed contract metadata/deprecation proposals.
            self.data.contract_upgrade_proposals = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, t_contract_upgrade_proposal]
            )
            self.data.contract_upgrade_votes = sp.cast(
                sp.big_map(), sp.big_map[t_contract_upgrade_vote_key, sp.bool]
            )
            self.data.next_contract_upgrade_proposal_id = sp.cast(0, sp.nat)

            # v11 -> v12 migration claims: old token id -> new token id
            self.data.migration_claims = sp.cast(
                sp.big_map(), sp.big_map[sp.nat, sp.nat]
            )

        @sp.entrypoint
        def register_keep_commitment(self, commitment):
            """
            Register a pre-commitment for a future keep.
            Users submit commitment = blake2b(pack(contract, owner, content_hash, salt)).
            """
            sp.cast(commitment, sp.bytes)
            assert self.data.contract_state == CONTRACT_STATE_ACTIVE, "CONTRACT_DEPRECATED"
            key = sp.record(owner=sp.sender, commitment=commitment)
            sp.cast(key, t_commitment_key)
            assert not self.data.keep_commitments.contains(key), "COMMITMENT_EXISTS"
            self.data.keep_commitments[key] = sp.now

        @sp.entrypoint
        def cancel_keep_commitment(self, commitment):
            """Remove a previously registered commitment for the sender."""
            sp.cast(commitment, sp.bytes)
            key = sp.record(owner=sp.sender, commitment=commitment)
            sp.cast(key, t_commitment_key)
            assert self.data.keep_commitments.contains(key), "COMMITMENT_NOT_FOUND"
            del self.data.keep_commitments[key]

        @sp.entrypoint
        def keep(self, params):
            """
            Mint a new keep.

            Requirements:
            - Exact keep fee payment.
            - Valid matured commitment for (sender, content_hash, salt).
            - Unique content_hash.
            - creator_pubkey must correspond to sender.
            """
            sp.cast(
                params,
                sp.record(
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
                    salt=sp.bytes,
                    creator_pubkey=sp.key,
                    initial_refresh_policy=sp.nat,
                ),
            )

            assert self.data.contract_state == CONTRACT_STATE_ACTIVE, "CONTRACT_DEPRECATED"
            assert sp.amount == self.data.keep_fee, "INVALID_FEE_AMOUNT"
            assert params.initial_refresh_policy <= POLICY_OWNER_AND_CREATOR, "INVALID_REFRESH_POLICY"

            derived_creator = sp.to_address(sp.implicit_account(sp.hash_key(params.creator_pubkey)))
            assert derived_creator == sp.sender, "CREATOR_KEY_SENDER_MISMATCH"

            commitment_payload = sp.record(
                contract=sp.self_address,
                owner=sp.sender,
                content_hash=params.content_hash,
                salt=params.salt,
            )
            sp.cast(
                commitment_payload,
                sp.record(
                    contract=sp.address,
                    owner=sp.address,
                    content_hash=sp.bytes,
                    salt=sp.bytes,
                ).layout(("contract", ("owner", ("content_hash", "salt")))),
            )
            commitment_key = sp.record(
                owner=sp.sender,
                commitment=sp.blake2b(sp.pack(commitment_payload)),
            )
            sp.cast(commitment_key, t_commitment_key)
            assert self.data.keep_commitments.contains(commitment_key), "COMMITMENT_NOT_FOUND"
            registered_at = self.data.keep_commitments[commitment_key]
            maturity = sp.add_seconds(
                registered_at, sp.to_int(self.data.commitment_min_delay_seconds)
            )
            assert sp.now >= maturity, "COMMITMENT_TOO_FRESH"
            del self.data.keep_commitments[commitment_key]

            assert not self.data.content_hashes.contains(params.content_hash), "DUPLICATE_CONTENT_HASH"

            token_id = self.data.next_token_id
            token_info = sp.cast(
                {
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
                    "": params.metadata_uri,
                },
                sp.map[sp.string, sp.bytes],
            )

            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info,
            )
            self.data.ledger[token_id] = sp.sender
            self.data.metadata_locked[token_id] = False
            self.data.content_hashes[params.content_hash] = token_id
            self.data.token_creators[token_id] = sp.sender
            self.data.token_creator_keys[token_id] = params.creator_pubkey
            self.data.refresh_policies[token_id] = params.initial_refresh_policy
            self.data.refresh_nonces[token_id] = 0
            self.data.next_token_id = token_id + 1
            self.data.active_token_count += 1

            # Non-custodial fee handling: forward fee immediately.
            if sp.amount > sp.mutez(0):
                sp.send(self.data.treasury_address, sp.amount)

        @sp.entrypoint
        def claim_from_v11(self, params):
            """
            Trustless one-time claim migration from v11.
            Verifies current ownership on the source contract via get_balance_of view.
            """
            sp.cast(
                params,
                sp.record(
                    old_token_id=sp.nat,
                    token_info=sp.map[sp.string, sp.bytes],
                    creator_pubkey=sp.key,
                ),
            )

            assert self.data.contract_state == CONTRACT_STATE_ACTIVE, "CONTRACT_DEPRECATED"
            assert sp.amount == sp.mutez(0), "MIGRATION_NO_FEE_REQUIRED"
            assert not self.data.migration_claims.contains(params.old_token_id), "OLD_TOKEN_ALREADY_CLAIMED"

            derived_creator = sp.to_address(
                sp.implicit_account(sp.hash_key(params.creator_pubkey))
            )
            assert derived_creator == sp.sender, "CREATOR_KEY_SENDER_MISMATCH"

            requests = [
                sp.record(
                    owner=sp.sender,
                    token_id=params.old_token_id,
                )
            ]
            sp.cast(requests, sp.list[t_fa2_balance_of_request])

            source_balances = sp.view(
                "get_balance_of",
                self.data.migration_source_contract,
                requests,
                sp.list[t_fa2_balance_of_response],
            ).unwrap_some(error="MIGRATION_VIEW_FAILED")

            assert sp.len(source_balances) == 1, "MIGRATION_BAD_VIEW_RESPONSE_COUNT"
            for response in source_balances:
                assert response.request.owner == sp.sender, "MIGRATION_BAD_VIEW_RESPONSE"
                assert response.request.token_id == params.old_token_id, "MIGRATION_BAD_VIEW_RESPONSE"
                assert response.balance == 1, "NOT_SOURCE_TOKEN_OWNER"

            content_hash = params.token_info.get("content_hash", default=sp.bytes("0x"))
            royalties = params.token_info.get("royalties", default=sp.bytes("0x"))
            assert content_hash != sp.bytes("0x"), "MISSING_CONTENT_HASH"
            assert royalties != sp.bytes("0x"), "MISSING_ROYALTIES"
            assert not self.data.content_hashes.contains(content_hash), "DUPLICATE_CONTENT_HASH"

            token_id = self.data.next_token_id
            token_info = params.token_info

            # Keep metadata URI aliases aligned.
            if token_info.contains("metadata_uri"):
                token_info[""] = token_info["metadata_uri"]
            if token_info.contains(""):
                token_info["metadata_uri"] = token_info[""]

            # Provenance tags for indexers/UIs.
            token_info["upgraded_from_contract"] = sp.pack(self.data.migration_source_contract)
            token_info["upgraded_from_token_id"] = sp.pack(params.old_token_id)
            token_info["migration_kind"] = sp.bytes("0x7631315f636c61696d")  # "v11_claim"

            # Force immutable fields to canonical values before mint.
            token_info["content_hash"] = content_hash
            token_info["royalties"] = royalties

            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info,
            )
            self.data.ledger[token_id] = sp.sender
            self.data.metadata_locked[token_id] = False
            self.data.content_hashes[content_hash] = token_id
            self.data.token_creators[token_id] = sp.sender
            self.data.token_creator_keys[token_id] = params.creator_pubkey
            self.data.refresh_policies[token_id] = POLICY_OWNER_ONLY
            self.data.refresh_nonces[token_id] = 0
            self.data.next_token_id = token_id + 1
            self.data.active_token_count += 1
            self.data.migration_claims[params.old_token_id] = token_id

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
        def propose_contract_upgrade(self, params):
            """
            Create a holder-governed proposal for contract-level metadata updates.
            Optional irreversible deprecation can be bundled in the same proposal.
            """
            sp.cast(
                params,
                sp.record(
                    metadata_updates=sp.list[t_contract_metadata_update],
                    metadata_updates_hash=sp.bytes,
                    successor=sp.option[sp.address],
                    deprecate=sp.bool,
                ),
            )

            assert self.data.active_token_count > 0, "NO_ACTIVE_TOKENS"
            assert sp.len(params.metadata_updates) > 0, "EMPTY_METADATA_UPDATES"
            assert (
                sp.blake2b(sp.pack(params.metadata_updates))
                == params.metadata_updates_hash
            ), "METADATA_HASH_MISMATCH"
            if params.deprecate:
                assert params.successor.is_some(), "SUCCESSOR_REQUIRED"
                assert self.data.contract_state == CONTRACT_STATE_ACTIVE, "ALREADY_DEPRECATED"

            proposal_id = self.data.next_contract_upgrade_proposal_id
            voting_deadline = sp.add_seconds(
                sp.now,
                sp.to_int(self.data.governance_voting_period_seconds)
            )
            proposal = sp.record(
                proposer=sp.sender,
                created_at=sp.now,
                voting_deadline=voting_deadline,
                metadata_updates=params.metadata_updates,
                metadata_updates_hash=params.metadata_updates_hash,
                successor=params.successor,
                deprecate=params.deprecate,
                yes_votes=0,
                no_votes=0,
                executed=False,
            )
            sp.cast(proposal, t_contract_upgrade_proposal)
            self.data.contract_upgrade_proposals[proposal_id] = proposal
            self.data.next_contract_upgrade_proposal_id = proposal_id + 1

        @sp.entrypoint
        def vote_contract_upgrade(self, params):
            """
            Vote a proposal with token IDs owned by sender.
            One vote per token_id per proposal.
            """
            sp.cast(
                params,
                sp.record(
                    proposal_id=sp.nat,
                    token_ids=sp.list[sp.nat],
                    support=sp.bool,
                ),
            )

            assert self.data.contract_upgrade_proposals.contains(
                params.proposal_id
            ), "PROPOSAL_NOT_FOUND"
            proposal = self.data.contract_upgrade_proposals[params.proposal_id]
            assert not proposal.executed, "PROPOSAL_ALREADY_EXECUTED"
            assert sp.now <= proposal.voting_deadline, "VOTING_CLOSED"
            assert sp.len(params.token_ids) > 0, "EMPTY_TOKEN_LIST"

            for token_id in params.token_ids:
                assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
                owner = self.data.ledger.get(token_id, default=BURN_ADDRESS)
                assert owner == sp.sender, "NOT_TOKEN_OWNER"

                vote_key = sp.record(proposal_id=params.proposal_id, token_id=token_id)
                sp.cast(vote_key, t_contract_upgrade_vote_key)
                assert not self.data.contract_upgrade_votes.contains(vote_key), "TOKEN_ALREADY_VOTED"
                self.data.contract_upgrade_votes[vote_key] = params.support

                if params.support:
                    proposal.yes_votes += 1
                else:
                    proposal.no_votes += 1
            self.data.contract_upgrade_proposals[params.proposal_id] = proposal

        @sp.entrypoint
        def execute_contract_upgrade(self, proposal_id):
            """
            Execute a passed proposal after voting closes.
            Trustless pass conditions:
            - quorum of active tokens participated
            - yes ratio meets approval threshold
            """
            sp.cast(proposal_id, sp.nat)
            assert self.data.contract_upgrade_proposals.contains(proposal_id), "PROPOSAL_NOT_FOUND"

            proposal = self.data.contract_upgrade_proposals[proposal_id]
            assert not proposal.executed, "PROPOSAL_ALREADY_EXECUTED"
            assert sp.now > proposal.voting_deadline, "VOTING_STILL_OPEN"
            assert self.data.active_token_count > 0, "NO_ACTIVE_TOKENS"

            cast_votes = proposal.yes_votes + proposal.no_votes
            assert cast_votes > 0, "NO_VOTES_CAST"
            assert (
                cast_votes * 10000
                >= self.data.active_token_count * self.data.governance_quorum_bps
            ), "QUORUM_NOT_MET"
            assert (
                proposal.yes_votes * 10000
                >= cast_votes * self.data.governance_approval_bps
            ), "APPROVAL_NOT_MET"

            for item in proposal.metadata_updates:
                self.data.metadata[item.key] = item.value

            if proposal.deprecate:
                assert self.data.contract_state == CONTRACT_STATE_ACTIVE, "ALREADY_DEPRECATED"
                assert proposal.successor.is_some(), "SUCCESSOR_REQUIRED"
                self.data.contract_state = CONTRACT_STATE_DEPRECATED
                self.data.deprecated_successor = proposal.successor
                self.data.deprecated_at = sp.Some(sp.now)

            proposal.executed = True
            self.data.contract_upgrade_proposals[proposal_id] = proposal

        @sp.offchain_view()
        def contract_upgrade_status(self):
            """Expose governance/deprecation status for UIs and tooling."""
            return sp.record(
                contract_state=self.data.contract_state,
                deprecated_successor=self.data.deprecated_successor,
                deprecated_at=self.data.deprecated_at,
                active_token_count=self.data.active_token_count,
                migration_source_contract=self.data.migration_source_contract,
                governance_voting_period_seconds=self.data.governance_voting_period_seconds,
                governance_quorum_bps=self.data.governance_quorum_bps,
                governance_approval_bps=self.data.governance_approval_bps,
                next_proposal_id=self.data.next_contract_upgrade_proposal_id,
            )

        @sp.entrypoint
        def set_refresh_policy(self, params):
            """
            Owner-controlled metadata refresh policy.
            """
            sp.cast(params, sp.record(token_id=sp.nat, policy=sp.nat))
            assert self.data.token_metadata.contains(params.token_id), "FA2_TOKEN_UNDEFINED"
            is_owner = self.data.ledger.get(
                params.token_id,
                default=BURN_ADDRESS,
            ) == sp.sender
            assert is_owner, "NOT_TOKEN_OWNER"
            is_locked = self.data.metadata_locked.get(params.token_id, default=False)
            assert not is_locked, "METADATA_LOCKED"
            assert params.policy <= POLICY_OWNER_AND_CREATOR, "INVALID_REFRESH_POLICY"
            self.data.refresh_policies[params.token_id] = params.policy

        @sp.entrypoint
        def edit_metadata(self, params):
            """
            Update metadata with per-token refresh policy authorization.

            Policy modes:
            - owner_only: owner full edit.
            - creator_only: creator refresh-only fields.
            - owner_or_creator: owner full edit OR creator refresh-only.
            - owner_and_creator: owner full edit + creator signature consent
              (unless owner == creator).

            In all cases, content_hash and royalties remain immutable.
            """
            sp.cast(
                params,
                sp.record(
                    token_id=sp.nat,
                    token_info=sp.map[sp.string, sp.bytes],
                    creator_sig=sp.option[sp.signature],
                    creator_sig_deadline=sp.option[sp.timestamp],
                ),
            )

            assert self.data.token_metadata.contains(params.token_id), "FA2_TOKEN_UNDEFINED"
            is_locked = self.data.metadata_locked.get(params.token_id, default=False)
            assert not is_locked, "METADATA_LOCKED"

            owner = self.data.ledger.get(
                params.token_id,
                default=BURN_ADDRESS,
            )
            creator = self.data.token_creators.get(
                params.token_id,
                default=BURN_ADDRESS,
            )
            is_owner = owner == sp.sender
            is_creator = creator == sp.sender

            existing_info = self.data.token_metadata[params.token_id].token_info
            original_hash = existing_info.get("content_hash", default=sp.bytes("0x"))
            original_royalties = existing_info.get("royalties", default=sp.bytes("0x"))

            policy = self.data.refresh_policies.get(
                params.token_id, default=POLICY_OWNER_OR_CREATOR
            )
            assert policy <= POLICY_OWNER_AND_CREATOR, "INVALID_REFRESH_POLICY"

            if policy == POLICY_OWNER_ONLY:
                assert is_owner, "OWNER_REQUIRED"
                self.data.token_metadata[params.token_id] = sp.record(
                    token_id=params.token_id,
                    token_info=params.token_info,
                )
            else:
                if policy == POLICY_CREATOR_ONLY:
                    assert is_creator, "CREATOR_REQUIRED"
                    refreshed_info = self.data.token_metadata[params.token_id].token_info
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
                    if params.token_info.contains("metadata_uri"):
                        refreshed_info[""] = params.token_info["metadata_uri"]
                    if params.token_info.contains(""):
                        refreshed_info["metadata_uri"] = params.token_info[""]
                    self.data.token_metadata[params.token_id] = sp.record(
                        token_id=params.token_id,
                        token_info=refreshed_info,
                    )
                else:
                    if policy == POLICY_OWNER_OR_CREATOR:
                        if is_owner:
                            self.data.token_metadata[params.token_id] = sp.record(
                                token_id=params.token_id,
                                token_info=params.token_info,
                            )
                        else:
                            assert is_creator, "NOT_AUTHORIZED"
                            refreshed_info = self.data.token_metadata[params.token_id].token_info
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
                            if params.token_info.contains("metadata_uri"):
                                refreshed_info[""] = params.token_info["metadata_uri"]
                            if params.token_info.contains(""):
                                refreshed_info["metadata_uri"] = params.token_info[""]
                            self.data.token_metadata[params.token_id] = sp.record(
                                token_id=params.token_id,
                                token_info=refreshed_info,
                            )
                    else:
                        if policy == POLICY_OWNER_AND_CREATOR:
                            if is_owner and is_creator:
                                # Same wallet is both roles.
                                self.data.token_metadata[params.token_id] = sp.record(
                                    token_id=params.token_id,
                                    token_info=params.token_info,
                                )
                            else:
                                # Owner submits edit + creator co-signs exact payload.
                                assert is_owner, "OWNER_REQUIRED"
                                assert self.data.token_creator_keys.contains(params.token_id), "MISSING_CREATOR_KEY"
                                assert params.creator_sig.is_some(), "CREATOR_SIG_REQUIRED"
                                assert params.creator_sig_deadline.is_some(), "CREATOR_SIG_DEADLINE_REQUIRED"

                                deadline = params.creator_sig_deadline.unwrap_some()
                                assert sp.now <= deadline, "CREATOR_SIG_EXPIRED"

                                nonce = self.data.refresh_nonces.get(params.token_id, default=0)
                                consent_payload = sp.record(
                                    contract=sp.self_address,
                                    token_id=params.token_id,
                                    token_info_hash=sp.blake2b(sp.pack(params.token_info)),
                                    nonce=nonce,
                                    deadline=deadline,
                                )
                                sp.cast(
                                    consent_payload,
                                    sp.record(
                                        contract=sp.address,
                                        token_id=sp.nat,
                                        token_info_hash=sp.bytes,
                                        nonce=sp.nat,
                                        deadline=sp.timestamp,
                                    ).layout(("contract", ("token_id", ("token_info_hash", ("nonce", "deadline"))))),
                                )
                                creator_key = self.data.token_creator_keys[params.token_id]
                                assert sp.check_signature(
                                    creator_key,
                                    params.creator_sig.unwrap_some(),
                                    sp.pack(consent_payload),
                                ), "INVALID_CREATOR_SIG"

                                self.data.refresh_nonces[params.token_id] = nonce + 1
                                self.data.token_metadata[params.token_id] = sp.record(
                                    token_id=params.token_id,
                                    token_info=params.token_info,
                                )
                        else:
                            assert False, "INVALID_REFRESH_POLICY"

            self.data.token_metadata[params.token_id].token_info["content_hash"] = original_hash
            self.data.token_metadata[params.token_id].token_info["royalties"] = original_royalties

            # Keep metadata URI aliases aligned post-update.
            updated_info = self.data.token_metadata[params.token_id].token_info
            if updated_info.contains("metadata_uri"):
                updated_info[""] = updated_info["metadata_uri"]
            if updated_info.contains(""):
                updated_info["metadata_uri"] = updated_info[""]
            self.data.token_metadata[params.token_id].token_info = updated_info

        @sp.entrypoint
        def lock_metadata(self, token_id):
            """Owner-only irreversible metadata lock."""
            sp.cast(token_id, sp.nat)
            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
            is_owner = self.data.ledger.get(
                token_id, default=BURN_ADDRESS
            ) == sp.sender
            assert is_owner, "NOT_TOKEN_OWNER"
            self.data.metadata_locked[token_id] = True

        @sp.entrypoint
        def burn_keep(self, token_id):
            """Owner-only burn; frees content_hash for re-keep."""
            sp.cast(token_id, sp.nat)

            assert self.data.token_metadata.contains(token_id), "FA2_TOKEN_UNDEFINED"
            current_owner = self.data.ledger.get(
                token_id, default=BURN_ADDRESS
            )
            assert current_owner == sp.sender, "NOT_TOKEN_OWNER"

            token_info = self.data.token_metadata[token_id].token_info
            content_hash = token_info.get("content_hash", default=sp.bytes("0x"))

            if self.data.content_hashes.contains(content_hash):
                del self.data.content_hashes[content_hash]
            if self.data.ledger.contains(token_id):
                del self.data.ledger[token_id]
            if self.data.metadata_locked.contains(token_id):
                del self.data.metadata_locked[token_id]
            if self.data.token_creators.contains(token_id):
                del self.data.token_creators[token_id]
            if self.data.token_creator_keys.contains(token_id):
                del self.data.token_creator_keys[token_id]
            if self.data.refresh_policies.contains(token_id):
                del self.data.refresh_policies[token_id]
            if self.data.refresh_nonces.contains(token_id):
                del self.data.refresh_nonces[token_id]

            del self.data.token_metadata[token_id]
            self.data.active_token_count = sp.as_nat(self.data.active_token_count - 1)


@sp.add_test()
def test():
    scenario = sp.test_scenario("KeepsFA2v12 draft")
    scenario.h1("KidLisp Keeps FA2 v12 (draft)")

    alice = sp.test_account("Alice")
    bob = sp.test_account("Bob")
    treasury = sp.test_account("Treasury")

    token0_metadata = sp.cast(
        {
            "name": sp.bytes("0x2464656d6f"),  # "$demo"
            "symbol": sp.bytes("0x64656d6f"),  # "demo"
            "metadata_uri": sp.bytes("0x697066733a2f2f6b69646c6973702d7631312d6d657461"),
            "": sp.bytes("0x697066733a2f2f6b69646c6973702d7631312d6d657461"),
            "content_hash": sp.bytes("0x746573742d68617368"),
            "royalties": sp.bytes("0x7b7d"),
        },
        sp.map[sp.string, sp.bytes],
    )

    contract = keeps_module.KidLispKeepsFA2v12(
        treasury.address,
        treasury.address,
        sp.big_map(),
        {0: alice.address},
        [token0_metadata],
        sp.mutez(0),
        0,
        900,
        100,
        1,
        2000,
        6667,
    )
    scenario += contract

    metadata_updates = [
        sp.record(
            key="",
            value=sp.bytes("0x697066733a2f2f6b69646c6973702d7631322d636f6c6c656374696f6e2d6d657461"),
        ),
        sp.record(
            key="content",
            value=sp.bytes("0x697066733a2f2f6b69646c6973702d7631322d75706772616465"),
        ),
    ]
    metadata_updates_hash = sp.blake2b(sp.pack(metadata_updates))

    contract.propose_contract_upgrade(
        metadata_updates=metadata_updates,
        metadata_updates_hash=metadata_updates_hash,
        successor=sp.Some(bob.address),
        deprecate=True,
        _sender=bob,
        _now=sp.timestamp(5),
    )

    contract.vote_contract_upgrade(
        proposal_id=0,
        token_ids=[0],
        support=True,
        _sender=alice,
        _now=sp.timestamp(5),
    )

    contract.execute_contract_upgrade(
        0,
        _sender=bob,
        _now=sp.timestamp(10),
    )

    scenario.verify(contract.data.contract_state == keeps_module.CONTRACT_STATE_DEPRECATED)
    scenario.verify(contract.data.deprecated_successor.unwrap_some() == bob.address)
    scenario.verify(
        contract.data.metadata[""]
        == sp.bytes("0x697066733a2f2f6b69646c6973702d7631322d636f6c6c656374696f6e2d6d657461")
    )

    contract.register_keep_commitment(
        sp.bytes("0x00"),
        _sender=alice,
        _now=sp.timestamp(11),
        _valid=False,
        _exception="CONTRACT_DEPRECATED",
    )

    scenario.p(
        "v12 draft: no admin, commit-reveal keep, owner burn, refresh policies, holder-governed upgrades/deprecation"
    )
