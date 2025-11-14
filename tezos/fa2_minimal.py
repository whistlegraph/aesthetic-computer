import smartpy as sp

@sp.module
def main():
    class FA2(sp.Contract):
        def __init__(self, admin):
            self.data.ledger = sp.big_map()
            self.data.token_metadata = sp.big_map()
            self.data.operators = sp.big_map()
            self.data.all_tokens = sp.set()
            self.data.admin = admin

        @sp.entrypoint
        def transfer(self, batch):
            for transfer in batch:
                for tx in transfer.txs:
                    sp.cast(tx.token_id, sp.nat)
                    from_ = transfer.from_
                    to_ = tx.to_
                    
                    assert self.data.all_tokens.contains(tx.token_id), "FA2_TOKEN_UNDEFINED"
                    assert (sp.sender == from_) | \
                           self.data.operators.contains(sp.record(owner=from_, operator=sp.sender, token_id=tx.token_id)), \
                           "FA2_NOT_OPERATOR"
                    
                    if from_ != to_:
                        current_from = self.data.ledger.get((from_, tx.token_id), default=0)
                        assert current_from >= tx.amount, "FA2_INSUFFICIENT_BALANCE"
                        self.data.ledger[(from_, tx.token_id)] = sp.as_nat(current_from - tx.amount)
                        
                        current_to = self.data.ledger.get((to_, tx.token_id), default=0)
                        self.data.ledger[(to_, tx.token_id)] = current_to + tx.amount

        @sp.entrypoint
        def balance_of(self, params):
            result = []
            for req in params.requests:
                assert self.data.all_tokens.contains(req.token_id), "FA2_TOKEN_UNDEFINED"
                balance = self.data.ledger.get((req.owner, req.token_id), default=0)
                result.append(sp.record(request=req, balance=balance))
            sp.transfer(result, sp.tez(0), params.callback)

        @sp.entrypoint
        def update_operators(self, actions):
            for action in actions:
                if action.is_variant("add_operator"):
                    params = action.unwrap_some()
                    assert params.owner == sp.sender, "FA2_NOT_OWNER"
                    self.data.operators[sp.record(owner=params.owner, operator=params.operator, token_id=params.token_id)] = ()
                else:
                    params = action.unwrap_some()
                    assert params.owner == sp.sender, "FA2_NOT_OWNER"
                    del self.data.operators[sp.record(owner=params.owner, operator=params.operator, token_id=params.token_id)]

        @sp.entrypoint
        def mint(self, params):
            assert sp.sender == self.data.admin, "NOT_ADMIN"
            self.data.all_tokens.add(params.token_id)
            self.data.token_metadata[params.token_id] = sp.record(
                token_id=params.token_id,
                token_info=params.metadata
            )
            self.data.ledger[(params.address, params.token_id)] = params.amount

if "templates" not in __name__:
    @sp.add_test()
    def test():
        admin = sp.test_account("Admin")
        alice = sp.test_account("Alice")
        
        scenario = sp.test_scenario("FA2", main)
        c1 = main.FA2(admin.address)
        scenario += c1
        
        c1.mint(
            token_id=0,
            address=alice.address,
            amount=1,
            metadata=sp.map({"name": sp.utils.bytes_of_string("My Token")})
        ).run(sender=admin)
