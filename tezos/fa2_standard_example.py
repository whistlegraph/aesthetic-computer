import smartpy as sp

# Standard FA2 NFT template from SmartPy v0.23.1
@sp.module
def main():
    class FA2(sp.Contract):
        def __init__(self, admin, metadata):
            self.data.ledger = sp.big_map()
            self.data.token_metadata = sp.big_map()
            self.data.operators = sp.big_map()
            self.data.metadata = metadata
            self.data.admin = admin
            self.data.next_token_id = 0

        @sp.entrypoint
        def transfer(self, params):
            for transfer in params:
                for tx in transfer.txs:
                    from_user = transfer.from_
                    to_user = tx.to_
                    token_id = tx.token_id
                    amount = tx.amount
                    
                    # Check permissions
                    assert (sp.sender == from_user) | \
                           self.data.operators.contains((from_user, (sp.sender, token_id))), \
                           "FA2_NOT_OPERATOR"
                    
                    # Get current balances
                    from_balance = self.data.ledger.get((from_user, token_id), default=0)
                    to_balance = self.data.ledger.get((to_user, token_id), default=0)
                    
                    # Check sufficient balance
                    assert from_balance >= amount, "FA2_INSUFFICIENT_BALANCE"
                    
                    # Update balances
                    self.data.ledger[(from_user, token_id)] = sp.as_nat(from_balance - amount)
                    self.data.ledger[(to_user, token_id)] = to_balance + amount

        @sp.entrypoint
        def balance_of(self, params):
            responses = []
            for request in params.requests:
                balance = self.data.ledger.get(
                    (request.owner, request.token_id),
                    default=0
                )
                responses.append(
                    sp.record(request=request, balance=balance)
                )
            sp.transfer(responses, sp.mutez(0), params.callback)

        @sp.entrypoint
        def update_operators(self, params):
            for update in params:
                if update.is_variant("add_operator"):
                    operator = update.unwrap_some()
                    assert sp.sender == operator.owner, "FA2_NOT_OWNER"
                    self.data.operators[(operator.owner, (operator.operator, operator.token_id))] = ()
                else:
                    operator = update.unwrap_some()
                    assert sp.sender == operator.owner, "FA2_NOT_OWNER"
                    del self.data.operators[(operator.owner, (operator.operator, operator.token_id))]

        @sp.entrypoint
        def mint(self, params):
            assert sp.sender == self.data.admin, "FA2_NOT_ADMIN"
            token_id = self.data.next_token_id
            self.data.token_metadata[token_id] = sp.record(
                token_id=token_id,
                token_info=params.metadata
            )
            self.data.ledger[(params.to_, token_id)] = params.amount
            self.data.next_token_id += 1

@sp.add_test()
def test():
    scenario = sp.test_scenario("FA2", main)
    scenario.h1("FA2 NFT Standard Example")
    
    admin = sp.test_account("Admin")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Bob")
    
    # Create contract
    metadata = sp.big_map({
        "": sp.bytes("0x74657a6f732d73746f726167653a64617461"),
        "data": sp.bytes('{"name": "FA2 NFT", "version": "1.0.0"}')
    })
    
    c1 = main.FA2(admin.address, metadata)
    scenario += c1
    
    # Mint token
    scenario.h2("Mint")
    token_metadata = sp.map({
        "name": sp.bytes("0x546f6b656e2030"),
        "decimals": sp.bytes("0x30"),
        "symbol": sp.bytes("0x544b4e")
    })
    c1.mint(to_=alice.address, amount=1, metadata=token_metadata).run(sender=admin)
    
    # Transfer
    scenario.h2("Transfer")
    c1.transfer([
        sp.record(
            from_=alice.address,
            txs=[sp.record(to_=bob.address, token_id=0, amount=1)]
        )
    ]).run(sender=alice)
