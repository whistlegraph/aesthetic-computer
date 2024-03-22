// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";

/// @custom:security-contact mail+contracts@aesthetic.computer
contract Paintings is ERC721 {
    constructor() ERC721("Paintings", "ACP") {}

    function _baseURI() internal pure override returns (string memory) {
        return "https://paintings.aesthetic.computer/";
    }
}