# From here: https://smartpy.io/templates/fa2_lib_nft.py
import smartpy as sp
from smartpy.templates import fa2_lib as fa2

# Main template for FA2 contracts
main = fa2.main


@sp.module
def my_module():
    import main

    # Order of inheritance: [Admin], [<policy>], <base class>, [<other mixins>].
    class MyNFTContract(
        main.Admin,
        main.Nft,
        main.MintNft,
        main.BurnNft,
        main.OnchainviewBalanceOf,
    ):
        def __init__(self, admin_address, contract_metadata, ledger, token_metadata):
            """Initializes the contract with administrative permissions and NFT functionalities.
            The base class is required; all mixins are optional.
            The initialization must follow this order:

            - Other mixins such as OnchainviewBalanceOf, MintNFT, and BurnNFT
            - Base class: NFT
            - Transfer policy
            - Admin
            """

            # Initialize on-chain balance view
            main.OnchainviewBalanceOf.__init__(self)

            # Initialize the NFT-specific entrypoints
            main.BurnNft.__init__(self)
            main.MintNft.__init__(self)

            # Initialize the NFT base class
            main.Nft.__init__(self, contract_metadata, ledger, token_metadata)

            # Initialize administrative permissions
            main.Admin.__init__(self, admin_address)


def _get_balance(fa2_contract, args):
    """Utility function to call the contract's get_balance view to get an account's token balance."""
    return sp.View(fa2_contract, "get_balance")(args)


def _total_supply(fa2_contract, args):
    """Utility function to call the contract's total_supply view to get the total amount of tokens."""
    return sp.View(fa2_contract, "total_supply")(args)


@sp.add_test()
def test():
    # Create and configure the test scenario
    # Import the types from the FA2 library, the library itself, and the contract module, in that order.
    scenario = sp.test_scenario("fa2_lib_nft")
    scenario.h1("FA2 NFT contract test")

    # Define test accounts
    admin = sp.test_account("Admin")
    alice = sp.test_account("Alice")
    bob = sp.test_account("Bob")

    # Define initial token metadata and ownership
    tok0_md = fa2.make_metadata(name="Token Zero", decimals=1, symbol="Tok0")
    tok1_md = fa2.make_metadata(name="Token One", decimals=1, symbol="Tok1")
    tok2_md = fa2.make_metadata(name="Token Two", decimals=1, symbol="Tok2")
    token_metadata = [tok0_md, tok1_md, tok2_md]
    ledger = {0: alice.address, 1: alice.address, 2: bob.address}

    # Instantiate the FA2 NFT contract
    contract = my_module.MyNFTContract(
        admin.address, sp.big_map(), ledger, token_metadata
    )

    # Build contract metadata content
    contract_metadata = sp.create_tzip16_metadata(
        name="My FA2 NFT contract",
        description="This is an FA2 NFT contract using SmartPy.",
        version="1.0.0",
        license_name="CC-BY-SA",
        license_details="Creative Commons Attribution Share Alike license 4.0 https://creativecommons.org/licenses/by/4.0/",
        interfaces=["TZIP-012", "TZIP-016"],
        authors=["SmartPy <https://smartpy.io/#contact>"],
        homepage="https://smartpy.io/ide?template=fa2_lib_nft.py",
        # Optionally, upload the source code to IPFS and add the URI here
        source_uri=None,
        offchain_views=contract.get_offchain_views(),
    )

    # Add the info specific to FA2 permissions
    contract_metadata["permissions"] = {
        # The operator policy chosen:
        # owner-or-operator-transfer is the default.
        "operator": "owner-or-operator-transfer",
        # Those two options should always have these values.
        # It means that the contract doesn't use the hook mechanism.
        "receiver": "owner-no-hook",
        "sender": "owner-no-hook",
    }

    # You must upload the contract metadata to IPFS and get its URI.
    # You can write the contract_metadata object to a JSON file with json.dumps() and upload it manually.
    # You can also use sp.pin_on_ipfs() to upload the object via pinata.cloud and get the IPFS URI:
    # metadata_uri = sp.pin_on_ipfs(contract_metadata, api_key=None, secret_key=None, name = "Metadata for my FA2 contract")

    # This is a placeholder value. In production, replace it with your metadata URI.
    metadata_uri = "ipfs://example"

    # Create the metadata big map based on the IPFS URI
    contract_metadata = sp.scenario_utils.metadata_of_url(metadata_uri)

    # Update the scenario instance with the new metadata
    contract.data.metadata = contract_metadata

    # Originate the contract in the test scenario
    scenario += contract

    if scenario.simulation_mode() is sp.SimulationMode.MOCKUP:
        scenario.p("mockups - fix transfer based testing")
        return

    # Run tests

    scenario.h2("Verify the initial owners of the tokens")
    scenario.verify(
        _get_balance(contract, sp.record(owner=alice.address, token_id=0)) == 1
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=bob.address, token_id=0)) == 0
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=alice.address, token_id=1)) == 1
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=bob.address, token_id=1)) == 0
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=alice.address, token_id=2)) == 0
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=bob.address, token_id=2)) == 1
    )

    # Verify the token supply
    scenario.verify(_total_supply(contract, sp.record(token_id=0)) == 1)
    scenario.verify(_total_supply(contract, sp.record(token_id=1)) == 1)
    scenario.verify(_total_supply(contract, sp.record(token_id=2)) == 1)

    scenario.h2("Transfer a token")
    contract.transfer(
        [
            sp.record(
                from_=alice.address,
                txs=[sp.record(to_=bob.address, amount=1, token_id=0)],
            ),
        ],
        _sender=alice,
    )
    # Verify the result
    scenario.verify(
        _get_balance(contract, sp.record(owner=alice.address, token_id=0)) == 0
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=bob.address, token_id=0)) == 1
    )
    # Transfer it back
    contract.transfer(
        [
            sp.record(
                from_=bob.address,
                txs=[sp.record(to_=alice.address, amount=1, token_id=0)],
            ),
        ],
        _sender=bob,
    )

    scenario.h2("Mint a token")
    nft3_md = fa2.make_metadata(name="Token Three", decimals=1, symbol="Tok3")
    # Verify that only the admin can mint a token
    contract.mint(
        [
            sp.record(metadata=nft3_md, to_=bob.address),
        ],
        _sender=bob,
        _valid=False,
    )
    # Mint a token
    contract.mint(
        [
            sp.record(metadata=nft3_md, to_=bob.address),
        ],
        _sender=admin,
    )
    # Verify the result
    scenario.verify(_total_supply(contract, sp.record(token_id=3)) == 1)
    scenario.verify(
        _get_balance(contract, sp.record(owner=alice.address, token_id=3)) == 0
    )
    scenario.verify(
        _get_balance(contract, sp.record(owner=bob.address, token_id=3)) == 1
    )

    scenario.h2("Burn a token")
    # Verify that you can't burn someone else's token
    contract.burn(
        [sp.record(token_id=3, from_=bob.address, amount=1)],
        _sender=alice,
        _valid=False,
    )

    # Verify that you can burn your own token
    contract.burn([sp.record(token_id=3, from_=bob.address, amount=1)], _sender=bob)