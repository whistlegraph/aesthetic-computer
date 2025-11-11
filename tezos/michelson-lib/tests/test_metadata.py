"""
Tests for metadata management entrypoints
"""

import pytest
from lib.entrypoints.update_metadata import UpdateMetadataEntrypoint
from lib.entrypoints.freeze_metadata import FreezeMetadataEntrypoint


class TestUpdateMetadataEntrypoint:
    """Test suite for update_metadata entrypoint"""
    
    def test_name(self):
        """Entrypoint has correct name"""
        ep = UpdateMetadataEntrypoint()
        assert ep.name == "update_metadata"
    
    def test_param_type_defined(self):
        """Parameter type is defined"""
        ep = UpdateMetadataEntrypoint()
        assert ep.param_type is not None
        assert "token_id" in ep.param_type
        assert "metadata" in ep.param_type
    
    def test_michelson_code_exists(self):
        """Michelson code is generated"""
        ep = UpdateMetadataEntrypoint()
        code = ep.michelson()
        assert code is not None
        assert len(code) > 0
    
    def test_michelson_has_admin_check(self):
        """Michelson includes admin authorization check"""
        ep = UpdateMetadataEntrypoint()
        code = ep.michelson()
        assert "FA2_NOT_ADMIN" in code
        assert "SENDER" in code
        assert "administrator" in code
    
    def test_michelson_validates_token_exists(self):
        """Michelson validates token exists"""
        ep = UpdateMetadataEntrypoint()
        code = ep.michelson()
        assert "FA2_TOKEN_UNDEFINED" in code
        assert "token_metadata" in code
        assert "MEM" in code
    
    def test_michelson_updates_map(self):
        """Michelson updates token_info map"""
        ep = UpdateMetadataEntrypoint()
        code = ep.michelson()
        assert "ITER" in code  # Iterates over new metadata entries
        assert "UPDATE" in code  # Updates the map
    
    def test_factory_function(self):
        """Factory function creates instance"""
        from lib.entrypoints.update_metadata import entrypoint
        ep = entrypoint()
        assert isinstance(ep, UpdateMetadataEntrypoint)


class TestFreezeMetadataEntrypoint:
    """Test suite for freeze_metadata entrypoint"""
    
    def test_name(self):
        """Entrypoint has correct name"""
        ep = FreezeMetadataEntrypoint()
        assert ep.name == "freeze_metadata"
    
    def test_param_type_defined(self):
        """Parameter type is defined"""
        ep = FreezeMetadataEntrypoint()
        assert ep.param_type is not None
        assert ep.param_type == "nat"  # Just token_id
    
    def test_michelson_code_exists(self):
        """Michelson code is generated"""
        ep = FreezeMetadataEntrypoint()
        code = ep.michelson()
        assert code is not None
        assert len(code) > 0
    
    def test_michelson_has_admin_check(self):
        """Michelson includes admin authorization check"""
        ep = FreezeMetadataEntrypoint()
        code = ep.michelson()
        assert "FA2_NOT_ADMIN" in code
        assert "SENDER" in code
        assert "administrator" in code
    
    def test_michelson_validates_token_exists(self):
        """Michelson validates token exists"""
        ep = FreezeMetadataEntrypoint()
        code = ep.michelson()
        assert "FA2_TOKEN_UNDEFINED" in code
        assert "token_metadata" in code
    
    def test_michelson_adds_frozen_flag(self):
        """Michelson adds __frozen flag to metadata"""
        ep = FreezeMetadataEntrypoint()
        code = ep.michelson()
        assert "__frozen" in code
        assert "0x01" in code  # True in bytes
    
    def test_michelson_updates_metadata(self):
        """Michelson updates token_metadata big_map"""
        ep = FreezeMetadataEntrypoint()
        code = ep.michelson()
        assert "UPDATE" in code
        assert "token_info" in code
    
    def test_factory_function(self):
        """Factory function creates instance"""
        from lib.entrypoints.freeze_metadata import entrypoint
        ep = entrypoint()
        assert isinstance(ep, FreezeMetadataEntrypoint)


class TestMetadataIntegration:
    """Integration tests for metadata management"""
    
    def test_update_then_freeze_workflow(self):
        """Can create both entrypoints for update->freeze workflow"""
        from lib.entrypoints.update_metadata import entrypoint as update_ep
        from lib.entrypoints.freeze_metadata import entrypoint as freeze_ep
        
        update = update_ep()
        freeze = freeze_ep()
        
        assert update.name == "update_metadata"
        assert freeze.name == "freeze_metadata"
        
    def test_both_have_admin_protection(self):
        """Both entrypoints require admin"""
        from lib.entrypoints.update_metadata import entrypoint as update_ep
        from lib.entrypoints.freeze_metadata import entrypoint as freeze_ep
        
        update_code = update_ep().michelson()
        freeze_code = freeze_ep().michelson()
        
        assert "FA2_NOT_ADMIN" in update_code
        assert "FA2_NOT_ADMIN" in freeze_code
