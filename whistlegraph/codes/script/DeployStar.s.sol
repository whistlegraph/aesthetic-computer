// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Script.sol";
import "../src/OrgStar.sol";

// Deploys OrgStar — the open-edition patronage star.
//
//   Rehearsal:  forge script script/DeployStar.s.sol \
//                 --rpc-url https://sepolia.base.org --broadcast
//   Mainnet:    forge script script/DeployStar.s.sol \
//                 --rpc-url https://mainnet.base.org --broadcast
//
// PRIVATE_KEY comes from the environment (vault wallet export) and is never
// written to this repository. The deployer becomes owner and royalty receiver,
// and should be whistlegraph.eth (0x238c9c645c6EE83d4323A2449C706940321a0cBf).
//
// STAR_PRICE overrides the default in wei; the RFC recommends 0.01 ether.
//
// After deploying, put the address and the deploy block into the CHAINS block
// of system/public/whistlegraph.org/stars.html — the page reads the sky from
// mint logs starting at that block, and renders an empty sky until it is set.
contract DeployStar is Script {
    function run() external {
        uint256 pk = vm.envUint("PRIVATE_KEY");
        uint256 price = vm.envOr("STAR_PRICE", uint256(0.01 ether));

        vm.startBroadcast(pk);
        OrgStar star = new OrgStar(price);
        vm.stopBroadcast();

        console.log("OrgStar deployed:", address(star));
        console.log("price (wei):", price);
        console.log("deploy block:", block.number);
        console.log("-> set contract + deployBlock in stars.html");
    }
}
