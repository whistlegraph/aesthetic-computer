"""
Tests for Keep entrypoint
"""

import pytest
from lib.entrypoints.keep import KeepEntrypoint


class TestKeepEntrypoint:
    """Test suite for Keep entrypoint"""
    
    def test_name(self):
        """Entrypoint has correct name"""
        ep = KeepEntrypoint()
        assert ep.name == "keep"
    
    def test_param_type_defined(self):
        """Parameter type is defined"""
        ep = KeepEntrypoint()
        assert ep.param_type is not None
        assert "ac_url" in ep.param_type
        assert "content_hash" in ep.param_type
        assert "content_type" in ep.param_type
    
    def test_michelson_code_exists(self):
        """Michelson code is generated"""
        ep = KeepEntrypoint()
        code = ep.michelson()
        assert code is not None
        assert len(code) > 0
    
    def test_michelson_has_validation(self):
        """Michelson includes content_type validation"""
        ep = KeepEntrypoint()
        code = ep.michelson()
        assert "INVALID_CONTENT_TYPE" in code
        assert "kidlisp" in code
        assert "tape" in code
        assert "painting" in code
    
    def test_michelson_has_auth_check(self):
        """Michelson includes authorization check"""
        ep = KeepEntrypoint()
        code = ep.michelson()
        assert "FA2_NOT_ADMIN" in code
        assert "SENDER" in code
    
    def test_michelson_builds_metadata(self):
        """Michelson builds token metadata"""
        ep = KeepEntrypoint()
        code = ep.michelson()
        assert "token_info" in code or "EMPTY_MAP" in code
        assert "artifactUri" in code
        assert "displayUri" in code
        assert "thumbnailUri" in code
    
    def test_validate_content_type_kidlisp(self):
        """Validates kidlisp content type"""
        assert KeepEntrypoint.validate_content_type("kidlisp") == True
    
    def test_validate_content_type_tape(self):
        """Validates tape content type"""
        assert KeepEntrypoint.validate_content_type("tape") == True
    
    def test_validate_content_type_painting(self):
        """Validates painting content type"""
        assert KeepEntrypoint.validate_content_type("painting") == True
    
    def test_validate_content_type_invalid(self):
        """Rejects invalid content type"""
        assert KeepEntrypoint.validate_content_type("invalid") == False
        assert KeepEntrypoint.validate_content_type("") == False
        assert KeepEntrypoint.validate_content_type("mp4") == False
