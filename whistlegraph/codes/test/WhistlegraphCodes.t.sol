// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Test.sol";
import "../src/WhistlegraphCodes.sol";

contract Receiver is IERC721Receiver {
    function onERC721Received(address, address, uint256, bytes calldata) external pure returns (bytes4) {
        return IERC721Receiver.onERC721Received.selector;
    }
}

contract Rejector {}

contract WhistlegraphCodesTest is Test {
    WhistlegraphCodes wg;
    address practice = address(0xA11CE);
    address collector = address(0xB0B);

    function setUp() public {
        vm.prank(practice);
        wg = new WhistlegraphCodes("https://whistlegraph.org/api/codes/");
        vm.deal(collector, 10 ether);
    }

    // ---- code <-> tokenId bijection ----

    function test_roundTrip() public view {
        string[6] memory codes = ["imab", "grow", "ppl", "w0w", "a", "abcdefghijklmnopqrstuvwxyz0123"];
        for (uint256 i = 0; i < codes.length; i++) {
            assertEq(wg.codeOf(wg.tokenIdOf(codes[i])), codes[i]);
        }
    }

    function test_distinctCodesDistinctIds() public view {
        assertTrue(wg.tokenIdOf("imab") != wg.tokenIdOf("imaa"));
        assertTrue(wg.tokenIdOf("a") != wg.tokenIdOf("aa"));
    }

    function test_badCodes() public {
        vm.expectRevert(WhistlegraphCodes.BadCode.selector);
        wg.tokenIdOf("");
        vm.expectRevert(WhistlegraphCodes.BadCode.selector);
        wg.tokenIdOf("IMAB"); // uppercase
        vm.expectRevert(WhistlegraphCodes.BadCode.selector);
        wg.tokenIdOf("$imab"); // sigils belong to KidLisp
        vm.expectRevert(WhistlegraphCodes.BadCode.selector);
        wg.tokenIdOf("im ab"); // whitespace
        vm.expectRevert(WhistlegraphCodes.BadCode.selector);
        wg.tokenIdOf("abcdefghijklmnopqrstuvwxyz01234"); // 31 bytes
    }

    // ---- publish + mint ----

    function test_publishAndMint() public {
        vm.prank(practice);
        wg.publish("imab", 0.05 ether);

        vm.prank(collector);
        wg.mint{value: 0.05 ether}("imab");

        assertEq(wg.ownerOfCode("imab"), collector);
        assertEq(wg.balanceOf(collector), 1);
        assertEq(address(wg).balance, 0.05 ether);
    }

    function test_mintUnpublishedReverts() public {
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.NotPublished.selector);
        wg.mint{value: 1 ether}("imab");
    }

    function test_wrongPaymentReverts() public {
        vm.prank(practice);
        wg.publish("grow", 0.1 ether);
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.WrongPayment.selector);
        wg.mint{value: 0.09 ether}("grow");
    }

    function test_doubleMintReverts() public {
        vm.prank(practice);
        wg.publish("ppl", 0);
        vm.prank(collector);
        wg.mint("ppl");
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.AlreadyMinted.selector);
        wg.mint("ppl");
    }

    function test_publishMintedReverts() public {
        vm.prank(practice);
        wg.publish("w0w", 0);
        vm.prank(collector);
        wg.mint("w0w");
        vm.prank(practice);
        vm.expectRevert(WhistlegraphCodes.AlreadyMinted.selector);
        wg.publish("w0w", 1 ether);
    }

    function test_publishOnlyOwner() public {
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.NotOwner.selector);
        wg.publish("imab", 0);
    }

    function test_repriceBeforeMint() public {
        vm.startPrank(practice);
        wg.publish("lonr", 1 ether);
        wg.publish("lonr", 0.2 ether);
        vm.stopPrank();
        vm.prank(collector);
        wg.mint{value: 0.2 ether}("lonr");
        assertEq(wg.ownerOfCode("lonr"), collector);
    }

    function test_mintTo() public {
        vm.prank(practice);
        wg.mintTo("wiyh", collector); // unpublished is fine for the owner
        assertEq(wg.ownerOfCode("wiyh"), collector);
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.NotOwner.selector);
        wg.mintTo("sdog", collector);
    }

    function test_publishMany() public {
        string[] memory codes = new string[](3);
        codes[0] = "imab"; codes[1] = "tri"; codes[2] = "nbff";
        uint256[] memory prices = new uint256[](3);
        prices[0] = 0.25 ether; prices[1] = 0.1 ether; prices[2] = 0.1 ether;

        vm.prank(practice);
        wg.publishMany(codes, prices);
        assertEq(wg.priceOf(wg.tokenIdOf("tri")), 0.1 ether);
        vm.prank(collector);
        wg.mint{value: 0.25 ether}("imab");
        assertEq(wg.ownerOfCode("imab"), collector);

        // length mismatch and non-owner both revert
        uint256[] memory short_ = new uint256[](2);
        vm.prank(practice);
        vm.expectRevert(WhistlegraphCodes.BadCode.selector);
        wg.publishMany(codes, short_);
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.NotOwner.selector);
        wg.publishMany(codes, prices);
    }

    // ---- money ----

    function test_withdraw() public {
        vm.prank(practice);
        wg.publish("imab", 1 ether);
        vm.prank(collector);
        wg.mint{value: 1 ether}("imab");

        uint256 before = practice.balance;
        vm.prank(practice);
        wg.withdraw(payable(practice));
        assertEq(practice.balance - before, 1 ether);

        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.NotOwner.selector);
        wg.withdraw(payable(collector));
    }

    function test_royaltyInfo() public {
        (address r, uint256 amt) = wg.royaltyInfo(0, 1 ether);
        assertEq(r, practice);
        assertEq(amt, 0.1 ether); // 10%

        vm.prank(practice);
        wg.setRoyalty(address(0xFEE), 500);
        (r, amt) = wg.royaltyInfo(0, 2 ether);
        assertEq(r, address(0xFEE));
        assertEq(amt, 0.1 ether); // 5% of 2

        vm.prank(practice);
        vm.expectRevert(WhistlegraphCodes.RoyaltyTooHigh.selector);
        wg.setRoyalty(address(0xFEE), 1001);
    }

    // ---- transfers ----

    function test_transferAndOwnerOfCode() public {
        vm.prank(practice);
        wg.mintTo("imab", collector);
        uint256 id = wg.tokenIdOf("imab");

        vm.prank(collector);
        wg.transferFrom(collector, address(0xCAFE), id);
        assertEq(wg.ownerOfCode("imab"), address(0xCAFE));
        assertEq(wg.balanceOf(collector), 0);
        assertEq(wg.balanceOf(address(0xCAFE)), 1);
    }

    function test_transferUnauthorizedReverts() public {
        vm.prank(practice);
        wg.mintTo("imab", collector);
        uint256 id = wg.tokenIdOf("imab");
        vm.prank(address(0xBAD));
        vm.expectRevert(WhistlegraphCodes.NotAuthorized.selector);
        wg.transferFrom(collector, address(0xBAD), id);
    }

    function test_approveThenTransfer() public {
        vm.prank(practice);
        wg.mintTo("imab", collector);
        uint256 id = wg.tokenIdOf("imab");

        vm.prank(collector);
        wg.approve(address(0xD00D), id);
        vm.prank(address(0xD00D));
        wg.transferFrom(collector, address(0xD00D), id);
        assertEq(wg.ownerOf(id), address(0xD00D));
        assertEq(wg.getApproved(id), address(0)); // cleared
    }

    function test_operatorApproval() public {
        vm.prank(practice);
        wg.mintTo("imab", collector);
        uint256 id = wg.tokenIdOf("imab");

        vm.prank(collector);
        wg.setApprovalForAll(address(0x09), true);
        vm.prank(address(0x09));
        wg.transferFrom(collector, address(0xCAFE), id);
        assertEq(wg.ownerOfCode("imab"), address(0xCAFE));
    }

    function test_safeTransferReceivers() public {
        vm.prank(practice);
        wg.mintTo("imab", collector);
        uint256 id = wg.tokenIdOf("imab");

        Receiver good = new Receiver();
        vm.prank(collector);
        wg.safeTransferFrom(collector, address(good), id);
        assertEq(wg.ownerOf(id), address(good));

        vm.prank(practice);
        wg.mintTo("grow", collector);
        Rejector bad = new Rejector();
        uint256 gid = wg.tokenIdOf("grow");
        vm.prank(collector);
        vm.expectRevert(WhistlegraphCodes.UnsafeReceiver.selector);
        wg.safeTransferFrom(collector, address(bad), gid);
    }

    // ---- metadata ----

    function test_tokenURI() public {
        vm.prank(practice);
        wg.mintTo("imab", collector);
        assertEq(wg.tokenURI(wg.tokenIdOf("imab")), "https://whistlegraph.org/api/codes/imab");

        uint256 gid = wg.tokenIdOf("grow");
        vm.expectRevert(WhistlegraphCodes.NoToken.selector);
        wg.tokenURI(gid);
    }

    function test_supportsInterface() public view {
        assertTrue(wg.supportsInterface(0x01ffc9a7)); // 165
        assertTrue(wg.supportsInterface(0x80ac58cd)); // 721
        assertTrue(wg.supportsInterface(0x5b5e139f)); // 721 metadata
        assertTrue(wg.supportsInterface(0x2a55205a)); // 2981
        assertFalse(wg.supportsInterface(0xdeadbeef));
    }

    function test_ownerHandoff() public {
        vm.prank(practice);
        wg.setOwner(address(0x111));
        vm.prank(practice);
        vm.expectRevert(WhistlegraphCodes.NotOwner.selector);
        wg.publish("imab", 0);
        vm.prank(address(0x111));
        wg.publish("imab", 0);
    }
}
