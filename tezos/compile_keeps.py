#!/usr/bin/env python3
# Direct compile script
import smartpy as sp
import sys

@sp.module
def main():
    class KeepsFA2(sp.Contract):
        def __init__(self, administrator):
            self.data.administrator = administrator
            self.data.ledger = sp.cast(sp.big_map(), sp.big_map[sp.pair[sp.address, sp.nat], sp.nat])
            self.data.metadata = sp.cast(sp.big_map(), sp.big_map[sp.string, sp.bytes])
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
            
            token_info = sp.cast({
                "name": sp.pack("Aesthetic Computer Keep"),
                "symbol": sp.pack("KEEP"),
                "decimals": sp.pack(sp.nat(0)),
                "artifactUri": sp.pack(params.ac_url),
                "displayUri": sp.pack(params.ac_url),
                "thumbnailUri": sp.pack(params.ac_url),
                "description": sp.pack("An aesthetic.computer piece preserved on Tezos"),
                "content_hash": sp.pack(params.content_hash),
                "content_type": sp.pack(params.content_type),
                "metadata_uri": sp.pack(params.metadata_uri)
            }, sp.map[sp.string, sp.bytes])
            
            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=token_info
            )
            self.data.ledger[(params.owner, token_id)] = sp.nat(1)
            self.data.next_token_id += 1

# Compile and output Michelson
admin = sp.address("tz1Lc2DzTjDPyWFj1iuAVGGZWNjK67Wun2dC")
sp.add_compilation_target("KeepsFA2", main.KeepsFA2(admin))
