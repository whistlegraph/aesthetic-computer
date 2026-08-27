// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Script.sol";
import "../src/WhistlegraphCodes.sol";

// Deploys WhistlegraphCodes with the production metadata base.
//
//   forge script script/Deploy.s.sol --rpc-url $RPC --broadcast
//
// PRIVATE_KEY comes from the environment (vault wallet export) and is
// never written to this repository.
contract Deploy is Script {
    function run() external {
        uint256 pk = vm.envUint("PRIVATE_KEY");
        vm.startBroadcast(pk);
        WhistlegraphCodes wg = new WhistlegraphCodes("https://whistlegraph.org/api/codes/");
        vm.stopBroadcast();
        console.log("WhistlegraphCodes deployed:", address(wg));
    }
}
