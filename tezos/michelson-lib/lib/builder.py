"""
Contract builder - Assembles complete Michelson contracts from modular entrypoints
"""

from typing import List, Dict, Optional
import re
from .types import STORAGE_TYPE


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
        # Pattern: IF_LEFT { ep1 } { IF_LEFT { ep2 } { ep3 } }
        
        def build_dispatch_tree(eps: list) -> list:
            """Recursively build IF_LEFT dispatch tree"""
            if len(eps) == 1:
                # Base case: single entrypoint
                ep_code = eps[0].michelson().strip()
                lines = []
                for line in ep_code.split('\n'):
                    lines.append(f"    {line}")
                return lines
            
            # Recursive case: IF_LEFT with left branch and right subtree
            left_ep = eps[0]
            right_eps = eps[1:]
            
            lines = ["  IF_LEFT"]
            lines.append("    {")
            # Left branch
            left_code = left_ep.michelson().strip()
            for line in left_code.split('\n'):
                lines.append(f"      {line}")
            lines.append("    }")
            lines.append("    {")
            # Right branch (recursive)
            right_lines = build_dispatch_tree(right_eps)
            lines.extend([f"  {line}" for line in right_lines])
            lines.append("    }")
            
            return lines
        
        code_lines = ["{ UNPAIR;"]
        code_lines.extend(build_dispatch_tree(self.entrypoints))
        code_lines.append("}")
        
        return '\n'.join(code_lines)
    
    def build(self) -> str:
        """Build complete Michelson contract"""
        param_type = self.build_parameter_type()
        storage_type = self.storage_type
        code = self.build_code()
        
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
