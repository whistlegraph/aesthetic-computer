#!/usr/bin/env python3
"""
Compile KidLisp Contract
"""

import sys
import os
import json
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

import smartpy as sp

# Import our contract
exec(open('contracts/kidlisp_working.py').read())

def compile_contract():
    """Compile the KidLisp contract"""
    
    print("🔨 Compiling KidLisp contract...")
    
    # Create a test scenario to compile the contract
    @sp.add_test(name="compile_test")
    def test():
        # Initialize contract with admin
        admin = sp.test_account("admin")
        contract = KidLisp(admin.address)
        
        scenario = sp.test_scenario()
        scenario.h1("KidLisp")
        scenario += contract
        
        # Export the contract
        scenario.export_contract(contract, 'output/kidlisp')
    
    # Compile the test
    sp.test_scenario_runner.run_test("compile_test")
    
    print("✅ Contract compiled successfully!")
    print("📁 Output files generated in output/ directory")

if __name__ == "__main__":
    compile_contract()
