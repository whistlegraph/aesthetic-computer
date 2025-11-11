"""
Tests for FA2 standard entrypoints: transfer, balance_of, update_operators
"""

import pytest


class TestTransfer:
    """Tests for FA2 transfer entrypoint"""
    
    def test_transfer_validates_structure(self):
        """Transfer should accept list of (from, [(to, (token_id, amount))])"""
        from lib.entrypoints.transfer import entrypoint
        ep = entrypoint()
        
        assert ep["name"] == "transfer"
        assert "list" in ep["parameter_type"].lower()
        assert "from_" in ep["parameter_type"]
        assert "to_" in ep["parameter_type"]
    
    def test_transfer_checks_sender_authorization(self):
        """Transfer should verify sender is from or operator"""
        from lib.entrypoints.transfer import entrypoint
        ep = entrypoint()
        
        assert "FA2_NOT_OPERATOR" in ep["code"]
        assert "SENDER" in ep["code"]
    
    def test_transfer_validates_amount(self):
        """Transfer should check amount is 1 for NFTs"""
        from lib.entrypoints.transfer import entrypoint
        ep = entrypoint()
        
        assert "FA2_INSUFFICIENT_BALANCE" in ep["code"]
        assert "PUSH nat 1" in ep["code"]
    
    def test_transfer_updates_ledger(self):
        """Transfer should remove from sender and add to recipient"""
        from lib.entrypoints.transfer import entrypoint
        ep = entrypoint()
        
        # Should update ledger by removing old entry
        assert "NONE unit" in ep["code"]
        # Should add new entry
        assert "SOME" in ep["code"]
        assert "UPDATE" in ep["code"]
    
    def test_transfer_supports_batch(self):
        """Transfer should process multiple transfers in one call"""
        from lib.entrypoints.transfer import entrypoint
        ep = entrypoint()
        
        assert "ITER" in ep["code"]
    
    def test_transfer_checks_balance_exists(self):
        """Transfer should fail if sender doesn't own token"""
        from lib.entrypoints.transfer import entrypoint
        ep = entrypoint()
        
        assert "IF_NONE" in ep["code"]
        assert "FA2_INSUFFICIENT_BALANCE" in ep["code"]


class TestBalanceOf:
    """Tests for FA2 balance_of view entrypoint"""
    
    def test_balance_of_validates_structure(self):
        """Balance of should accept requests and callback"""
        from lib.entrypoints.balance_of import entrypoint
        ep = entrypoint()
        
        assert ep["name"] == "balance_of"
        assert "requests" in ep["parameter_type"]
        assert "callback" in ep["parameter_type"]
    
    def test_balance_of_maps_requests(self):
        """Balance of should process all requests"""
        from lib.entrypoints.balance_of import entrypoint
        ep = entrypoint()
        
        assert "MAP" in ep["code"]
    
    def test_balance_of_returns_0_or_1(self):
        """Balance of should return 0 or 1 for NFTs"""
        from lib.entrypoints.balance_of import entrypoint
        ep = entrypoint()
        
        assert "PUSH nat 0" in ep["code"]
        assert "PUSH nat 1" in ep["code"]
        assert "IF_SOME" in ep["code"]
    
    def test_balance_of_calls_callback(self):
        """Balance of should send results to callback contract"""
        from lib.entrypoints.balance_of import entrypoint
        ep = entrypoint()
        
        assert "TRANSFER_TOKENS" in ep["code"]
        assert "PUSH mutez 0" in ep["code"]
    
    def test_balance_of_returns_operation(self):
        """Balance of should return operation for callback"""
        from lib.entrypoints.balance_of import entrypoint
        ep = entrypoint()
        
        assert "NIL operation" in ep["code"]
        assert "CONS" in ep["code"]


class TestUpdateOperators:
    """Tests for FA2 update_operators entrypoint"""
    
    def test_update_operators_validates_structure(self):
        """Update operators should accept add/remove variants"""
        from lib.entrypoints.update_operators import entrypoint
        ep = entrypoint()
        
        assert ep["name"] == "update_operators"
        assert "add_operator" in ep["parameter_type"]
        assert "remove_operator" in ep["parameter_type"]
        assert "or" in ep["parameter_type"].lower()
    
    def test_update_operators_checks_ownership(self):
        """Update operators should verify sender is owner"""
        from lib.entrypoints.update_operators import entrypoint
        ep = entrypoint()
        
        assert "FA2_NOT_OWNER" in ep["code"]
        assert "SENDER" in ep["code"]
    
    def test_update_operators_handles_add(self):
        """Update operators should add operator permissions"""
        from lib.entrypoints.update_operators import entrypoint
        ep = entrypoint()
        
        assert "IF_LEFT" in ep["code"]
        assert "PUSH unit Unit" in ep["code"]
        assert "SOME" in ep["code"]
    
    def test_update_operators_handles_remove(self):
        """Update operators should remove operator permissions"""
        from lib.entrypoints.update_operators import entrypoint
        ep = entrypoint()
        
        assert "NONE unit" in ep["code"]
        assert "UPDATE" in ep["code"]
    
    def test_update_operators_iterates_batch(self):
        """Update operators should process multiple updates"""
        from lib.entrypoints.update_operators import entrypoint
        ep = entrypoint()
        
        assert "ITER" in ep["code"]
    
    def test_update_operators_updates_storage(self):
        """Update operators should modify operators big_map"""
        from lib.entrypoints.update_operators import entrypoint
        ep = entrypoint()
        
        assert "GET 8" in ep["code"]  # Get operators from storage
        assert "UPDATE" in ep["code"]


class TestFA2Integration:
    """Integration tests for FA2 entrypoints working together"""
    
    def test_all_entrypoints_exist(self):
        """All FA2 entrypoints should be importable"""
        from lib.entrypoints.transfer import entrypoint as transfer
        from lib.entrypoints.balance_of import entrypoint as balance_of
        from lib.entrypoints.update_operators import entrypoint as update_operators
        
        assert transfer()["name"] == "transfer"
        assert balance_of()["name"] == "balance_of"
        assert update_operators()["name"] == "update_operators"
    
    def test_entrypoints_have_descriptions(self):
        """All FA2 entrypoints should have documentation"""
        from lib.entrypoints.transfer import entrypoint as transfer
        from lib.entrypoints.balance_of import entrypoint as balance_of
        from lib.entrypoints.update_operators import entrypoint as update_operators
        
        assert transfer()["description"]
        assert balance_of()["description"]
        assert update_operators()["description"]
    
    def test_operator_workflow(self):
        """Test operator approval -> transfer workflow"""
        from lib.entrypoints.update_operators import entrypoint as update_ops
        from lib.entrypoints.transfer import entrypoint as transfer
        
        # update_operators should set permissions
        ops_code = update_ops()["code"]
        assert "SOME" in ops_code  # Add operator
        
        # transfer should check operators
        transfer_code = transfer()["code"]
        assert "FA2_NOT_OPERATOR" in transfer_code
        assert "MEM" in transfer_code  # Check operator membership
