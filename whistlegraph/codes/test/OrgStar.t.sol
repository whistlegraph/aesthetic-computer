// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

import "forge-std/Test.sol";
import "../src/OrgStar.sol";

contract Receiver {
    bool public reject;
    function set(bool r) external { reject = r; }
    function onERC721Received(address, address, uint256, bytes calldata) external view returns (bytes4) {
        return reject ? bytes4(0xdeadbeef) : this.onERC721Received.selector;
    }
}

contract OrgStarTest is Test {
    OrgStar star;
    address alice = address(0xA11CE);
    address bob = address(0xB0B);
    uint256 constant PRICE = 0.01 ether;

    function setUp() public {
        star = new OrgStar(PRICE);
        vm.deal(alice, 10 ether);
        vm.deal(bob, 10 ether);
    }

    // ---- minting ----

    function test_MintAtPrice() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        assertEq(star.totalSupply(), 1);
        assertEq(star.ownerOf(1), alice);
        assertEq(star.balanceOf(alice), 1);
        assertEq(address(star).balance, PRICE);
    }

    function test_MintRejectsWrongPayment() public {
        vm.prank(alice);
        vm.expectRevert(OrgStar.WrongPayment.selector);
        star.mint{value: PRICE - 1}();

        vm.prank(alice);
        vm.expectRevert(OrgStar.WrongPayment.selector);
        star.mint{value: PRICE + 1}();
    }

    function test_OpenEdition_ManyStarsOneWallet() public {
        for (uint256 i = 0; i < 7; i++) {
            vm.prank(alice);
            star.mint{value: PRICE}();
        }
        assertEq(star.totalSupply(), 7);
        assertEq(star.balanceOf(alice), 7);
        assertEq(star.ownerOf(7), alice);
    }

    function test_MintToIsOwnerOnly() public {
        vm.prank(alice);
        vm.expectRevert(OrgStar.NotOwner.selector);
        star.mintTo(alice);

        star.mintTo(bob); // owner is this test contract
        assertEq(star.ownerOf(1), bob);
    }

    // ---- the star keeps its face after it trades ----

    function test_MinterFixedAtBirth() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        assertEq(star.minterOf(1), alice);

        vm.prank(alice);
        star.transferFrom(alice, bob, 1);

        assertEq(star.ownerOf(1), bob);
        assertEq(star.minterOf(1), alice, "the star keeps the face it was born with");
    }

    function test_TokenURI_UnchangedByTransfer() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        string memory before = star.tokenURI(1);
        vm.prank(alice);
        star.transferFrom(alice, bob, 1);
        assertEq(keccak256(bytes(star.tokenURI(1))), keccak256(bytes(before)));
    }

    // ---- the page/chain contract ----
    //
    // These are the values stars.html computes in JS for the same addresses
    // (FNV-1a over the lowercase "0x…" string). If this test ever fails, the
    // sky on whistlegraph.org and the art on-chain have drifted apart.

    function test_StarHashMatchesThePage() public view {
        assertEq(star.starHash(0x238c9c645c6EE83d4323A2449C706940321a0cBf), 1692378563);
        assertEq(star.starHash(0x5e6758C96A4cB5E2A1FE2E2772020dc8ad753b08), 3076528610);
        assertEq(star.starHash(0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045), 3251228244);
    }

    function test_HueAndPointsDerivedFromMinter() public view {
        // whistlegraph.eth -> hue 203, 7 points (page agrees)
        uint32 h = star.starHash(0x238c9c645c6EE83d4323A2449C706940321a0cBf);
        assertEq(h % 360, 203);
        assertEq(4 + (h % 4), 7);
    }

    // ---- on-chain art ----

    function test_TokenURIIsSelfContained() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        string memory uri = star.tokenURI(1);
        assertTrue(_startsWith(uri, "data:application/json;base64,"), "must be a data URI");
        // Nothing may point off-chain: no http, no ipfs.
        assertFalse(_contains(uri, "http"), "no external host");
        assertFalse(_contains(uri, "ipfs"), "no ipfs dependency");
    }

    function test_TokenURIRevertsForNoToken() public {
        vm.expectRevert(OrgStar.NoToken.selector);
        star.tokenURI(1);
    }

    // Each calendar year is an epoch, so the boundary is the part that matters:
    // the last second of a year and the first second of the next must differ.
    function test_ConstellationYear() public {
        uint256[5] memory when = [
            uint256(1767225600),  // 2026-01-01T00:00:00Z
            1788296150,           // 2026-09-01
            1798761599,           // 2026-12-31T23:59:59Z — last second
            1798761600,           // 2027-01-01T00:00:00Z — rollover
            4116700800            // 2100-06-15 — a non-leap century
        ];
        uint16[5] memory expected = [uint16(2026), 2026, 2026, 2027, 2100];

        for (uint256 i = 0; i < when.length; i++) {
            vm.warp(when[i]);
            vm.prank(alice);
            star.mint{value: PRICE}();
            assertEq(star.yearOf(i + 1), expected[i]);
        }
    }

    // ---- transfers and approvals ----

    function test_ApproveThenTransfer() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        vm.prank(alice);
        star.approve(bob, 1);
        vm.prank(bob);
        star.transferFrom(alice, bob, 1);
        assertEq(star.ownerOf(1), bob);
        assertEq(star.getApproved(1), address(0), "approval clears on transfer");
    }

    function test_ApprovalForAll() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        vm.prank(alice);
        star.setApprovalForAll(bob, true);
        vm.prank(bob);
        star.transferFrom(alice, bob, 1);
        assertEq(star.ownerOf(1), bob);
    }

    function test_TransferRejectsStranger() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        vm.prank(bob);
        vm.expectRevert(OrgStar.NotAuthorized.selector);
        star.transferFrom(alice, bob, 1);
    }

    function test_TransferRejectsWrongFrom() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        vm.prank(alice);
        vm.expectRevert(OrgStar.WrongFrom.selector);
        star.transferFrom(bob, alice, 1);
    }

    function test_TransferRejectsZero() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        vm.prank(alice);
        vm.expectRevert(OrgStar.ZeroAddress.selector);
        star.transferFrom(alice, address(0), 1);
    }

    function test_SafeTransferChecksReceiver() public {
        Receiver r = new Receiver();
        vm.prank(alice);
        star.mint{value: PRICE}();

        r.set(true);
        vm.prank(alice);
        vm.expectRevert(OrgStar.UnsafeReceiver.selector);
        star.safeTransferFrom(alice, address(r), 1);

        r.set(false);
        vm.prank(alice);
        star.safeTransferFrom(alice, address(r), 1);
        assertEq(star.ownerOf(1), address(r));
    }

    // ---- money ----

    function test_Withdraw() public {
        vm.prank(alice);
        star.mint{value: PRICE}();
        address payable dest = payable(address(0xDEAD));
        star.withdraw(dest);
        assertEq(dest.balance, PRICE);
        assertEq(address(star).balance, 0);
    }

    function test_WithdrawIsOwnerOnly() public {
        vm.prank(alice);
        vm.expectRevert(OrgStar.NotOwner.selector);
        star.withdraw(payable(alice));
    }

    function test_Royalty() public view {
        (address recv, uint256 amt) = star.royaltyInfo(1, 1 ether);
        assertEq(recv, address(this));
        assertEq(amt, 0.1 ether); // 10%
    }

    function test_RoyaltyCapped() public {
        vm.expectRevert(OrgStar.RoyaltyTooHigh.selector);
        star.setRoyalty(alice, 2001);
        star.setRoyalty(alice, 2000);
        assertEq(star.royaltyBps(), 2000);
    }

    function test_SetPrice() public {
        star.setPrice(0.02 ether);
        assertEq(star.price(), 0.02 ether);
        vm.prank(alice);
        star.mint{value: 0.02 ether}();
        assertEq(star.totalSupply(), 1);
    }

    function test_OwnerHandoff() public {
        star.setOwner(alice);
        assertEq(star.owner(), alice);
        vm.expectRevert(OrgStar.NotOwner.selector);
        star.setPrice(1);
    }

    function test_Interfaces() public view {
        assertTrue(star.supportsInterface(0x01ffc9a7)); // ERC-165
        assertTrue(star.supportsInterface(0x80ac58cd)); // ERC-721
        assertTrue(star.supportsInterface(0x5b5e139f)); // Metadata
        assertTrue(star.supportsInterface(0x2a55205a)); // EIP-2981
        assertFalse(star.supportsInterface(0xffffffff));
    }

    // ---- helpers ----

    function _startsWith(string memory s, string memory p) internal pure returns (bool) {
        bytes memory b = bytes(s);
        bytes memory q = bytes(p);
        if (b.length < q.length) return false;
        for (uint256 i = 0; i < q.length; i++) if (b[i] != q[i]) return false;
        return true;
    }

    function _contains(string memory s, string memory needle) internal pure returns (bool) {
        bytes memory b = bytes(s);
        bytes memory n = bytes(needle);
        if (n.length > b.length) return false;
        for (uint256 i = 0; i <= b.length - n.length; i++) {
            bool hit = true;
            for (uint256 j = 0; j < n.length; j++) if (b[i + j] != n[j]) { hit = false; break; }
            if (hit) return true;
        }
        return false;
    }
}
