# Tezos Data Persistence

This directory contains persistent Tezos blockchain data that survives container rebuilds.

## Directories

- `tezos-data/` - Maps to `/home/me/.tezos-client` in container
  - Contains client configuration, keys, and aliases
  - Persists wallet configurations and imported keys
  
- `tezos-node/` - Maps to `/home/me/.tezos-node` in container  
  - Contains blockchain data and node state
  - Persists synced blockchain data so you don't need to resync after rebuilds

## Security Note

The `tezos-data/` directory may contain private keys and sensitive information.
Make sure this directory is properly secured and not committed to version control.

## Usage

After the container starts, Tezos tools will automatically use these directories:

```fish
# Check client status
octez-client --version

# List configured networks
octez-client list known addresses

# Connect to Ghostnet (testnet)
octez-client --endpoint https://ghostnet.ecadinfra.com config update

# Check node status (if running a local node)
octez-node --version
```
