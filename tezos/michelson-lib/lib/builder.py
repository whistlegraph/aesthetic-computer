"""
Contract builder - Assembles complete Michelson contracts from modular entrypoints
"""

from typing import List, Dict, Optional
import re
from .types import STORAGE_TYPE


def compact_michelson_code(code_str: str) -> str:
    """
    Compact Michelson code to single line for Octez compatibility.
    
    The strict Octez parser requires code to be on a single line or have
    very specific formatting. Compacting to one line is the most reliable.
    """
    lines = code_str.strip().split('\n')
    code_parts = []
    
    for line in lines:
        stripped = line.strip()
        # Skip empty lines
        if not stripped:
            continue
        # Remove comments
        if '#' in stripped:
            code_part = stripped.split('#')[0].strip()
            if code_part:
                code_parts.append(code_part)
        else:
            code_parts.append(stripped)
    
    # Join all parts with spaces
    result = ' '.join(code_parts)
    
    # Normalize spacing
    result = result.replace(' ;', ';')
    result = result.replace('  ', ' ')  # Remove double spaces
    
    # Ensure proper spacing around braces
    result = result.replace('{ ', '{')
    result = result.replace(' }', '}')
    result = result.replace('}{', '} {')  # Space between consecutive braces
    
    return '{' + result[1:-1] + '}'  # Preserve outer braces


def compact_michelson_type(type_str: str) -> str:
    """
    Compact Michelson type to single line with proper annotation placement.
    
    In Michelson's compact syntax:
    - Annotations come AFTER type constructors: (pair %name ...)  
    - Remove unnecessary spacing around parentheses
    - Keep tokens space-separated
    """
    # Join all lines, preserving spaces between tokens
    lines = [line.strip() for line in type_str.strip().split('\n')]
    compacted = ' '.join(lines)
    
    # Normalize spacing: single space between tokens
    compacted = re.sub(r'\s+', ' ', compacted)
    
    # Remove spaces around parentheses
    compacted = re.sub(r'\(\s+', '(', compacted)
    compacted = re.sub(r'\s+\)', ')', compacted)
    
    return compacted.strip()


class Entrypoint:
    """Wrapper for entrypoint configuration"""
    def __init__(self, config: dict):
        self.name = config["name"]
        self.param_type = config["parameter_type"]
        self.code = config["code"]
        self.description = config.get("description", "")
    
    def michelson(self) -> str:
        return self.code


class ContractBuilder:
    """
    Build complete Michelson contracts from modular entrypoints
    
    Example:
        builder = ContractBuilder()
        builder.add_entrypoint(keep.entrypoint())
        builder.add_entrypoint(transfer.entrypoint())
        michelson = builder.build()
    """
    
    def __init__(self):
        self.entrypoints: List = []
        self.storage_type = STORAGE_TYPE
        
    def add_entrypoint(self, entrypoint_config) -> 'ContractBuilder':
        """Add an entrypoint to the contract"""
        # Convert dict to Entrypoint object if needed
        if isinstance(entrypoint_config, dict):
            entrypoint_config = Entrypoint(entrypoint_config)
        self.entrypoints.append(entrypoint_config)
        return self
    
    def build_parameter_type(self) -> str:
        """Build the parameter type from all entrypoints"""
        if not self.entrypoints:
            return "unit"
        
        if len(self.entrypoints) == 1:
            ep = self.entrypoints[0]
            compacted = compact_michelson_type(ep.param_type)
            # Single entrypoint: just return the type (no wrapping, no annotation)
            return compacted
        
        # Build nested or-tree for multiple entrypoints
        # Note: Entrypoint names are NOT included in the parameter type.
        # They are derived from the or-tree structure and used via --entrypoint when calling.
        param_parts = []
        for ep in self.entrypoints:
            compacted = compact_michelson_type(ep.param_type)
            param_parts.append(compacted)
        
        # Build left-biased or-tree
        # TODO: Balance the tree for efficiency
        result = param_parts[0]
        for part in param_parts[1:]:
            result = f"(or {result} {part})"
        
        return result
    
    def build_code(self) -> str:
        """Build the code section with entrypoint dispatch"""
        if not self.entrypoints:
            return "{ DROP; NIL operation; PAIR }"
        
        if len(self.entrypoints) == 1:
            ep = self.entrypoints[0]
            ep_code = ep.michelson().strip()
            return f"""{{
  UNPAIR;
  {ep_code}
}}"""
        
        # Build dispatch tree for multiple entrypoints
        # Pattern matches left-associative OR structure:
        # (or (or (or ep1 ep2) ep3) ep4)
        # Dispatch: IF_LEFT { IF_LEFT { IF_LEFT { ep1 } { ep2 } } { ep3 } } { ep4 }
        
        def build_dispatch_tree(eps: list) -> list:
            """
            Recursively build IF_LEFT dispatch tree matching left-associative OR.
            
            For parameter type: (or (or ep1 ep2) ep3)
            Generates dispatch: IF_LEFT { IF_LEFT { ep1 } { ep2 } } { ep3 }
            """
            if len(eps) == 1:
                # Base case: single entrypoint
                ep_code = eps[0].michelson().strip()
                lines = []
                for line in ep_code.split('\n'):
                    lines.append(f"    {line}")
                return lines
            
            # Recursive case for left-associative structure
            # (or (or ... (or (or ep[0] ep[1]) ep[2]) ...) ep[n-1])
            # Last entrypoint goes in right branch (unwrapped)
            # All others recursively in left branch (still OR-wrapped)
            
            *left_eps, right_ep = eps  # Split: all-but-last, last
            
            lines = ["  IF_LEFT"]
            lines.append("    {")
            # Left branch: recursive for all-but-last entrypoints
            left_lines = build_dispatch_tree(left_eps)
            lines.extend([f"  {line}" for line in left_lines])
            lines.append("    }")
            lines.append("    {")
            # Right branch: last entrypoint (unwrapped)
            right_code = right_ep.michelson().strip()
            for line in right_code.split('\n'):
                lines.append(f"      {line}")
            lines.append("    }")
            
            return lines
        
        code_lines = ["{ UNPAIR;"]
        code_lines.extend(build_dispatch_tree(self.entrypoints))
        code_lines.append("}")
        
        return '\n'.join(code_lines)
    
    def build(self, compact_code: bool = True) -> str:
        """
        Build complete Michelson contract
        
        Args:
            compact_code: If True, compact code section for Octez compatibility
        """
        param_type = self.build_parameter_type()
        storage_type = self.storage_type
        code = self.build_code()
        
        # Compact storage type for strict Octez parser
        storage_type = compact_michelson_type(storage_type)
        
        # Optionally compact code for strict Octez parser
        if compact_code:
            code = compact_michelson_code(code)
        
        contract = f"""parameter {param_type};
storage {storage_type};
code {code};
"""
        return contract
    
    def save(self, filename: str):
        """Save contract to file"""
        contract = self.build()
        with open(filename, 'w') as f:
            f.write(contract)
        return filename
