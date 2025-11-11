# Michelson Library - Modular Contract Components

Test-driven Michelson library for building FA2 NFT contracts with composable entrypoints.

## Architecture

```
michelson-lib/
├── lib/
│   ├── __init__.py
│   ├── types.py          # Type definitions
│   ├── storage.py        # Storage builders
│   ├── entrypoints/
│   │   ├── __init__.py
│   │   ├── keep.py       # Custom keep/mint entrypoint
│   │   ├── transfer.py   # FA2 transfer
│   │   ├── balance_of.py # FA2 balance queries
│   │   ├── operators.py  # FA2 operator management
│   │   ├── burn.py       # Burn tokens
│   │   ├── update_metadata.py  # Update token metadata
│   │   └── freeze_metadata.py  # Freeze metadata
│   └── builder.py        # Contract assembly
├── tests/
│   ├── __init__.py
│   ├── test_keep.py
│   ├── test_transfer.py
│   ├── test_metadata.py
│   └── test_integration.py
└── examples/
    ├── keeps_basic.py    # Basic keeps contract
    └── keeps_full.py     # Full featured contract
```

## Design Principles

1. **Each entrypoint is a separate module** with its own tests
2. **Pure Michelson** - no SmartPy dependencies
3. **Composable** - mix and match entrypoints
4. **Type-safe** - explicit type definitions
5. **Test-first** - every feature has tests before implementation

## Usage

```python
from michelson_lib.builder import ContractBuilder
from michelson_lib.entrypoints import keep, transfer, burn

# Build a contract
contract = ContractBuilder()
contract.add_entrypoint(keep.entrypoint())
contract.add_entrypoint(transfer.entrypoint())
contract.add_entrypoint(burn.entrypoint())

# Generate Michelson
michelson = contract.build()
```

## Testing

```bash
cd /workspaces/aesthetic-computer/tezos/michelson-lib
pytest -v
```

**Test Coverage:**
- 63 passing tests (unit + Docker integration)
- FA2 standard compliance validation
- Docker-based Octez integration tests
- Real contract parsing and validation

See [TESTING.md](TESTING.md) for detailed test documentation.

## Current Status

- [x] Project structure
- [ ] Type definitions
- [ ] Storage builder
- [ ] Keep entrypoint
- [ ] Transfer entrypoint
- [ ] Metadata operations
- [ ] Full integration tests
