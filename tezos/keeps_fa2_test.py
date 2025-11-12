#!/usr/bin/env python3
import smartpy as sp

@sp.module
def main():
    class KeepsFA2(sp.Contract):
        def __init__(self, administrator):
            self.data.administrator = administrator
            self.data.ledger = sp.cast(sp.big_map(), sp.big_map[sp.nat, sp.address])
            self.data.metadata = sp.cast(sp.big_map({
                "": sp.bytes("0x74657a6f732d73746f726167653a636f6e74656e74"),  # "tezos-storage:content"
                "content": sp.pack({
                    "name": "Aesthetic Computer Keeps",
                    "description": "FA2 NFT contract for aesthetic.computer",
                    "version": "1.0.0",
                    "interfaces": ["TZIP-012", "TZIP-016"]
                })
            }), sp.big_map[sp.string, sp.bytes])
            self.data.operators = sp.cast(sp.big_map(), sp.big_map[
                sp.record(owner=sp.address, operator=sp.address, token_id=sp.nat), 
                sp.unit
            ])
            self.data.token_metadata = sp.cast(sp.big_map(), sp.big_map[
                sp.nat, 
                sp.record(token_id=sp.nat, token_info=sp.map[sp.string, sp.bytes])
            ])
            self.data.next_token_id = sp.nat(0)
        
        @sp.entrypoint
        def keep(self, params):
            sp.cast(params, sp.record(
                ac_url=sp.string,
                content_hash=sp.string,
                content_type=sp.string,
                metadata_uri=sp.string,
                owner=sp.address
            ))
            
            token_id = self.data.next_token_id
            
            # Build TZIP-21 metadata
            token_info = {
                "": sp.pack({
                    "name": "Keep",
                    "artifactUri": params.ac_url,
                    "displayUri": params.ac_url,
                    "thumbnailUri": params.ac_url,
                    "content_hash": params.content_hash,
                    "content_type": params.content_type,
                    "metadata_uri": params.metadata_uri
                })
            }
            
            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info
            )
            self.data.ledger[token_id] = params.owner
            self.data.next_token_id += 1
        
        @sp.entrypoint
        def transfer(self, params):
            sp.cast(params, sp.list[sp.record(
                from_=sp.address,
                txs=sp.list[sp.record(
                    to_=sp.address,
                    token_id=sp.nat,
                    amount=sp.nat
                )]
            )])
            
            for transfer in params:
                for tx in transfer.txs:
                    assert tx.amount == 1, "FA2_INSUFFICIENT_BALANCE"
                    assert (transfer.from_ == sp.sender) or self.data.operators.contains(
                        sp.record(owner=transfer.from_, operator=sp.sender, token_id=tx.token_id)
                    ), "FA2_NOT_OWNER_OR_OPERATOR"
                    assert self.data.ledger[tx.token_id] == transfer.from_, "FA2_INSUFFICIENT_BALANCE"
                    self.data.ledger[tx.token_id] = tx.to_
        
        @sp.entrypoint
        def balance_of(self, params):
            sp.cast(params, sp.record(
                requests=sp.list[sp.record(owner=sp.address, token_id=sp.nat)],
                callback=sp.contract[sp.list[sp.record(
                    request=sp.record(owner=sp.address, token_id=sp.nat),
                    balance=sp.nat
                )]]
            ))
            
            responses = []
            for req in params.requests:
                balance = sp.nat(1) if self.data.ledger.get(req.token_id, sp.address("tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU")) == req.owner else sp.nat(0)
                responses.append(sp.record(request=req, balance=balance))
            
            sp.transfer(responses, sp.mutez(0), params.callback)
        
        @sp.entrypoint
        def update_operators(self, params):
            sp.cast(params, sp.list[sp.variant(
                add_operator=sp.record(owner=sp.address, operator=sp.address, token_id=sp.nat),
                remove_operator=sp.record(owner=sp.address, operator=sp.address, token_id=sp.nat)
            )])
            
            for update in params:
                if update.is_variant("add_operator"):
                    op = update.unwrap_some()
                    assert op.owner == sp.sender, "FA2_NOT_OWNER"
                    self.data.operators[sp.record(owner=op.owner, operator=op.operator, token_id=op.token_id)] = sp.unit
                else:  # remove_operator
                    op = update.unwrap_some()
                    assert op.owner == sp.sender, "FA2_NOT_OWNER"
                    del self.data.operators[sp.record(owner=op.owner, operator=op.operator, token_id=op.token_id)]

@sp.add_test()
def test():
    admin = sp.address("tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC")
    scenario = sp.test_scenario("keeps_fa2_enhanced", main)
    scenario.h1("Keeps FA2 NFT Contract")
    
    contract = main.KeepsFA2(admin)
    scenario += contract
