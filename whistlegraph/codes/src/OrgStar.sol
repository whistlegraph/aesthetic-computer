// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

// OrgStar — star the org.
//
// An open edition at a flat price. A star does nothing but shine: no supply
// cap, no tiers, no roadmap, no utility. Patronage, never investment — if a
// line in here starts to read like a security, delete it.
//
// The art is fully on-chain. tokenURI returns an SVG generated from the
// MINTING wallet, so nothing is hosted, nothing is pinned, and nothing can
// rot the way rdp.whistlegraph.com did. The star's birth wallet is recorded
// at mint and never changes, so a star keeps its face after it trades.
//
// whistlegraph.org/stars draws the same sky from mint logs using the same
// FNV-1a hash of the same lowercase "0x…" address string. The two must agree:
// change starHash here and you change the page.
//
// Self-contained on purpose: no external libraries, every line reviewable.
// RFC: whistlegraph/codes/ORGSTARS-RFC.md

contract OrgStar {
    string public constant name = "Whistlegraph Stars";
    string public constant symbol = "STAR";

    address public owner;
    address public royaltyReceiver;
    uint96 public royaltyBps; // out of 10_000
    uint256 public price;     // wei, flat, open edition
    uint256 public totalSupply;

    mapping(uint256 => address) private holder;
    mapping(address => uint256) public balanceOf;
    mapping(uint256 => address) public getApproved;
    mapping(address => mapping(address => bool)) public isApprovedForAll;

    // The star's face, fixed at birth: who minted it, and which constellation.
    mapping(uint256 => address) public minterOf;
    mapping(uint256 => uint16) public yearOf;

    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);
    event Approval(address indexed owner, address indexed approved, uint256 indexed tokenId);
    event ApprovalForAll(address indexed owner, address indexed operator, bool approved);
    event Minted(uint256 indexed tokenId, address indexed to, uint256 paid, uint16 year);

    error NotOwner();
    error WrongPayment();
    error NotAuthorized();
    error WrongFrom();
    error ZeroAddress();
    error NoToken();
    error UnsafeReceiver();
    error RoyaltyTooHigh();
    error SendFailed();

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner();
        _;
    }

    constructor(uint256 _price) {
        owner = msg.sender;
        royaltyReceiver = msg.sender;
        royaltyBps = 1000; // 10%
        price = _price;
    }

    // ---- minting ----

    function mint() external payable {
        if (msg.value != price) revert WrongPayment();
        _mint(msg.sender);
    }

    // Stars given rather than sold — the ones handed out on stage.
    function mintTo(address to) external onlyOwner {
        _mint(to);
    }

    function _mint(address to) internal {
        if (to == address(0)) revert ZeroAddress();
        uint256 tokenId = ++totalSupply;
        holder[tokenId] = to;
        minterOf[tokenId] = to;
        uint16 y = _year(block.timestamp);
        yearOf[tokenId] = y;
        unchecked { balanceOf[to] += 1; }
        emit Transfer(address(0), to, tokenId);
        emit Minted(tokenId, to, msg.value, y);
    }

    function withdraw(address payable to) external onlyOwner {
        if (to == address(0)) revert ZeroAddress();
        (bool ok, ) = to.call{value: address(this).balance}("");
        if (!ok) revert SendFailed();
    }

    // ---- the star, derived from its minter ----

    // FNV-1a (32-bit) over the lowercase "0x…" address string — byte for byte
    // what stars.html hashes, so page and chain draw the same star.
    function starHash(address a) public pure returns (uint32) {
        bytes memory s = _hexString(a);
        uint256 h = 0x811c9dc5;
        for (uint256 i = 0; i < s.length; i++) {
            h ^= uint8(s[i]);
            unchecked { h = (h * 0x01000193) & 0xffffffff; }
        }
        return uint32(h);
    }

    function _hexString(address a) internal pure returns (bytes memory) {
        bytes memory hexchars = "0123456789abcdef";
        bytes memory out = new bytes(42);
        out[0] = "0";
        out[1] = "x";
        uint160 v = uint160(a);
        for (uint256 i = 0; i < 20; i++) {
            uint8 b = uint8(v >> (8 * (19 - i)));
            out[2 + i * 2] = hexchars[b >> 4];
            out[3 + i * 2] = hexchars[b & 0x0f];
        }
        return out;
    }

    // Civil year from a unix timestamp (Howard Hinnant's days->civil, trimmed).
    function _year(uint256 ts) internal pure returns (uint16) {
        uint256 z = ts / 86400 + 719468;
        uint256 era = z / 146097;
        uint256 doe = z - era * 146097;
        uint256 yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
        uint256 y = yoe + era * 400;
        uint256 doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
        uint256 mp = (5 * doy + 2) / 153;
        return uint16(mp >= 10 ? y + 1 : y);
    }

    function tokenURI(uint256 tokenId) external view returns (string memory) {
        if (holder[tokenId] == address(0)) revert NoToken();
        uint32 h = starHash(minterOf[tokenId]);
        string memory json = _json(tokenId, h % 360, 4 + (h % 4), _toString(yearOf[tokenId]));
        return string(abi.encodePacked("data:application/json;base64,", _b64(bytes(json))));
    }

    // Split from tokenURI, and each half kept short: the whole thing in one
    // encodePacked runs the stack out of slots at solc 0.8.24 without via-ir,
    // and via-ir would change how the sibling contract compiles too.
    function _svg(uint256 hue, uint256 points) internal pure returns (string memory) {
        string memory hs = _toString(hue);
        string memory head = string(abi.encodePacked(
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 400 400">',
            '<rect width="400" height="400" fill="#16122a"/>',
            '<defs><radialGradient id="g">',
            '<stop offset="0" stop-color="hsl(', hs, ',80%,78%)" stop-opacity=".55"/>',
            '<stop offset="1" stop-color="hsl(', hs, ',80%,78%)" stop-opacity="0"/>',
            '</radialGradient></defs>'
        ));
        return string(abi.encodePacked(
            head,
            '<circle cx="200" cy="190" r="150" fill="url(#g)"/>',
            '<polygon points="', _points(points), '" fill="hsl(', hs, ',92%,80%)"/>'
        ));
    }

    function _json(uint256 tokenId, uint256 hue, uint256 points, string memory year)
        internal pure returns (string memory)
    {
        string memory svg = string(abi.encodePacked(
            _svg(hue, points),
            '<text x="200" y="360" text-anchor="middle" font-family="monospace" font-size="17" fill="#fffdf6" opacity=".72">Constellation of ',
            year, '</text></svg>'
        ));
        string memory head = string(abi.encodePacked(
            '{"name":"Star #', _toString(tokenId),
            '","description":"A star on whistlegraph.org. It does nothing but shine.",',
            '"attributes":[{"trait_type":"Constellation","value":"', year, '"},'
        ));
        return string(abi.encodePacked(
            head,
            '{"trait_type":"Points","value":', _toString(points),
            '},{"trait_type":"Hue","value":', _toString(hue),
            '}],"image":"data:image/svg+xml;base64,', _b64(bytes(svg)), '"}'
        ));
    }

    // A star polygon of `n` points on a 400x400 field, centred at (200,190).
    // Vertex 0 sits at 90° — straight up — and the rest walk round in half
    // steps, even indices reaching out to the tips, odd ones tucking in.
    function _points(uint256 n) internal pure returns (string memory) {
        bytes memory out;
        for (uint256 i = 0; i < n * 2; i++) {
            uint256 r = i % 2 == 0 ? 110 : 42;
            (int256 c, int256 s) = _unit((90 + (i * 180) / n) % 360);
            int256 x = 200 + (c * int256(r)) / 10000;
            int256 y = 190 - (s * int256(r)) / 10000; // SVG y grows downward
            out = abi.encodePacked(out, i == 0 ? "" : " ", _toString(uint256(x)), ",", _toString(uint256(y)));
        }
        return string(out);
    }

    // cos/sin * 10000 for a degree angle in the ordinary maths convention.
    // A 16-entry quarter table, mirrored into the other three — plenty of
    // resolution for a shape this size, and no trig library to import.
    function _unit(uint256 deg) internal pure returns (int256 c, int256 s) {
        uint16[17] memory T = [uint16(0), 1045, 2079, 3090, 4067, 5000, 5878, 6691,
            7431, 8090, 8660, 9135, 9511, 9781, 9945, 9998, 10000];
        uint256 q = deg / 90;
        uint256 idx = ((deg % 90) * 16) / 90;
        int256 lo = int256(uint256(T[idx]));       // sin across the first quarter
        int256 hi = int256(uint256(T[16 - idx]));  // cos across the first quarter
        if (q == 0) return (hi, lo);
        if (q == 1) return (-lo, hi);
        if (q == 2) return (-hi, -lo);
        return (lo, -hi);
    }

    // ---- ERC-721 ----

    function ownerOf(uint256 tokenId) public view returns (address) {
        address h = holder[tokenId];
        if (h == address(0)) revert NoToken();
        return h;
    }

    function approve(address to, uint256 tokenId) external {
        address h = ownerOf(tokenId);
        if (msg.sender != h && !isApprovedForAll[h][msg.sender]) revert NotAuthorized();
        getApproved[tokenId] = to;
        emit Approval(h, to, tokenId);
    }

    function setApprovalForAll(address operator, bool approved) external {
        isApprovedForAll[msg.sender][operator] = approved;
        emit ApprovalForAll(msg.sender, operator, approved);
    }

    function transferFrom(address from, address to, uint256 tokenId) public {
        if (ownerOf(tokenId) != from) revert WrongFrom();
        if (to == address(0)) revert ZeroAddress();
        if (msg.sender != from && msg.sender != getApproved[tokenId]
            && !isApprovedForAll[from][msg.sender]) revert NotAuthorized();
        delete getApproved[tokenId];
        unchecked { balanceOf[from] -= 1; balanceOf[to] += 1; }
        holder[tokenId] = to;
        emit Transfer(from, to, tokenId);
    }

    function safeTransferFrom(address from, address to, uint256 tokenId) external {
        safeTransferFrom(from, to, tokenId, "");
    }

    function safeTransferFrom(address from, address to, uint256 tokenId, bytes memory data) public {
        transferFrom(from, to, tokenId);
        if (to.code.length != 0) {
            bytes4 got = IERC721Receiver(to).onERC721Received(msg.sender, from, tokenId, data);
            if (got != IERC721Receiver.onERC721Received.selector) revert UnsafeReceiver();
        }
    }

    function royaltyInfo(uint256, uint256 salePrice) external view returns (address, uint256) {
        return (royaltyReceiver, (salePrice * royaltyBps) / 10000);
    }

    function supportsInterface(bytes4 id) external pure returns (bool) {
        return id == 0x01ffc9a7   // ERC-165
            || id == 0x80ac58cd   // ERC-721
            || id == 0x5b5e139f   // ERC-721Metadata
            || id == 0x2a55205a;  // EIP-2981
    }

    // ---- owner ----

    function setPrice(uint256 next) external onlyOwner { price = next; }

    function setRoyalty(address receiver, uint96 bps) external onlyOwner {
        if (bps > 2000) revert RoyaltyTooHigh();
        royaltyReceiver = receiver;
        royaltyBps = bps;
    }

    function setOwner(address next) external onlyOwner {
        if (next == address(0)) revert ZeroAddress();
        owner = next;
    }

    // ---- small helpers ----

    function _toString(uint256 v) internal pure returns (string memory) {
        if (v == 0) return "0";
        uint256 n = v;
        uint256 len;
        while (n != 0) { len++; n /= 10; }
        bytes memory b = new bytes(len);
        while (v != 0) { b[--len] = bytes1(uint8(48 + v % 10)); v /= 10; }
        return string(b);
    }

    function _b64(bytes memory data) internal pure returns (string memory) {
        if (data.length == 0) return "";
        bytes memory tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        uint256 outLen = 4 * ((data.length + 2) / 3);
        bytes memory out = new bytes(outLen);
        uint256 j;
        for (uint256 i = 0; i < data.length; i += 3) {
            uint256 a = uint8(data[i]);
            uint256 b = i + 1 < data.length ? uint8(data[i + 1]) : 0;
            uint256 c = i + 2 < data.length ? uint8(data[i + 2]) : 0;
            uint256 t = (a << 16) | (b << 8) | c;
            out[j++] = tbl[(t >> 18) & 63];
            out[j++] = tbl[(t >> 12) & 63];
            out[j++] = i + 1 < data.length ? tbl[(t >> 6) & 63] : bytes1("=");
            out[j++] = i + 2 < data.length ? tbl[t & 63] : bytes1("=");
        }
        return string(out);
    }
}

interface IERC721Receiver {
    function onERC721Received(address, address, uint256, bytes calldata) external returns (bytes4);
}
