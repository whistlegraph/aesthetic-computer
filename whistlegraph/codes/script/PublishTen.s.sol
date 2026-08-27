// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Script.sol";
import "../src/WhistlegraphCodes.sol";

// Publishes the first ten codes — the Ten Whistlegraphs set — at PRICE_WEI.
//
//   CONTRACT=0x... PRICE_WEI=20000000000000000 \
//   forge script script/PublishTen.s.sol --rpc-url $RPC --broadcast
contract PublishTen is Script {
    function run() external {
        WhistlegraphCodes wg = WhistlegraphCodes(vm.envAddress("CONTRACT"));
        uint256 price = vm.envUint("PRICE_WEI");
        string[10] memory codes = ["imab", "l8ly", "grow", "idni", "ppl", "wiyh", "lonr", "sdog", "w0w", "puzz"];

        vm.startBroadcast(vm.envUint("PRIVATE_KEY"));
        for (uint256 i = 0; i < codes.length; i++) {
            wg.publish(codes[i], price);
        }
        vm.stopBroadcast();
    }
}
