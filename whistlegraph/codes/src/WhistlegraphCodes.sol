// SPDX-License-Identifier: MIT
pragma solidity ^0.8.24;

// WhistlegraphCodes — one ERC-721 per whistlegraph code.
//
// The code IS the token: a short lowercase string ("imab", "grow", "ppl")
// packs left-aligned into bytes32, and that word is the tokenId. The same
// string is the URL slug (whistlegraph.org/imab) and the ENS subname
// (imab.whistlegraph.eth) — one string across web, token, and name.
//
// Self-contained on purpose: no external libraries, every line reviewable.
// Owner (whistlegraph.eth) publishes codes at a price; collectors mint.
// EIP-2981 royalties route secondary sales back to the practice.

contract WhistlegraphCodes {
    string public constant name = "Whistlegraph Codes";
    string public constant symbol = "WG";

    address public owner;
    address public royaltyReceiver;
    uint96 public royaltyBps; // out of 10_000
    string public baseURI;

    // code word (tokenId) => holder
    mapping(uint256 => address) private holder;
    mapping(address => uint256) public balanceOf;
    mapping(uint256 => address) public getApproved;
    mapping(address => mapping(address => bool)) public isApprovedForAll;

    // publishing: price is set by the owner before a code can be minted
    mapping(uint256 => uint256) public priceOf; // in wei
    mapping(uint256 => bool) public published;

    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);
    event Approval(address indexed owner, address indexed approved, uint256 indexed tokenId);
    event ApprovalForAll(address indexed owner, address indexed operator, bool approved);
    event Published(string code, uint256 indexed tokenId, uint256 price);
    event Minted(string code, uint256 indexed tokenId, address indexed to, uint256 paid);

    error NotOwner();
    error BadCode();
    error NotPublished();
    error AlreadyMinted();
    error WrongPayment();
    error NotAuthorized();
    error WrongFrom();
    error ZeroAddress();
    error NoToken();
    error UnsafeReceiver();
    error RoyaltyTooHigh();

    modifier onlyOwner() {
        if (msg.sender != owner) revert NotOwner();
        _;
    }

    constructor(string memory _baseURI) {
        owner = msg.sender;
        royaltyReceiver = msg.sender;
        royaltyBps = 1000; // 10%
        baseURI = _baseURI;
    }

    // ---- the code <-> tokenId bijection ----

    // Valid codes are 1..30 bytes of [a-z0-9], the same alphabet the site
    // and ENS labels use. The bytes pack left-aligned into a bytes32.
    function tokenIdOf(string memory code) public pure returns (uint256) {
        bytes memory b = bytes(code);
        if (b.length == 0 || b.length > 30) revert BadCode();
        bytes32 word;
        for (uint256 i = 0; i < b.length; i++) {
            bytes1 c = b[i];
            bool ok = (c >= 0x61 && c <= 0x7a) || (c >= 0x30 && c <= 0x39);
            if (!ok) revert BadCode();
            word |= bytes32(c) >> (i * 8);
        }
        return uint256(word);
    }

    function codeOf(uint256 tokenId) public pure returns (string memory) {
        bytes32 word = bytes32(tokenId);
        uint256 len = 0;
        while (len < 32 && word[len] != 0) len++;
        bytes memory b = new bytes(len);
        for (uint256 i = 0; i < len; i++) b[i] = word[i];
        return string(b);
    }

    // ---- publishing and minting ----

    function publish(string calldata code, uint256 price) external onlyOwner {
        uint256 id = tokenIdOf(code);
        if (holder[id] != address(0)) revert AlreadyMinted();
        published[id] = true;
        priceOf[id] = price;
        emit Published(code, id, price);
    }

    // One transaction publishes a whole drop.
    function publishMany(string[] calldata codes, uint256[] calldata prices) external onlyOwner {
        if (codes.length != prices.length) revert BadCode();
        for (uint256 i = 0; i < codes.length; i++) {
            uint256 id = tokenIdOf(codes[i]);
            if (holder[id] != address(0)) revert AlreadyMinted();
            published[id] = true;
            priceOf[id] = prices[i];
            emit Published(codes[i], id, prices[i]);
        }
    }

    function mint(string calldata code) external payable {
        uint256 id = tokenIdOf(code);
        if (!published[id]) revert NotPublished();
        if (holder[id] != address(0)) revert AlreadyMinted();
        if (msg.value != priceOf[id]) revert WrongPayment();
        _mint(msg.sender, id);
        emit Minted(code, id, msg.sender, msg.value);
    }

    // First-edition claims, gifts, and the reserve set: the owner may place
    // any unminted code directly, published or not.
    function mintTo(string calldata code, address to) external onlyOwner {
        uint256 id = tokenIdOf(code);
        if (holder[id] != address(0)) revert AlreadyMinted();
        _mint(to, id);
        emit Minted(code, id, to, 0);
    }

    function withdraw(address payable to) external onlyOwner {
        (bool ok,) = to.call{value: address(this).balance}("");
        require(ok);
    }

    // ---- views ----

    function ownerOf(uint256 tokenId) public view returns (address) {
        address h = holder[tokenId];
        if (h == address(0)) revert NoToken();
        return h;
    }

    function ownerOfCode(string calldata code) external view returns (address) {
        return ownerOf(tokenIdOf(code));
    }

    function tokenURI(uint256 tokenId) external view returns (string memory) {
        if (holder[tokenId] == address(0)) revert NoToken();
        return string(abi.encodePacked(baseURI, codeOf(tokenId)));
    }

    function royaltyInfo(uint256, uint256 salePrice) external view returns (address, uint256) {
        return (royaltyReceiver, (salePrice * royaltyBps) / 10_000);
    }

    function supportsInterface(bytes4 id) external pure returns (bool) {
        return id == 0x01ffc9a7 // ERC-165
            || id == 0x80ac58cd // ERC-721
            || id == 0x5b5e139f // ERC-721 Metadata
            || id == 0x2a55205a; // ERC-2981
    }

    // ---- admin ----

    function setBaseURI(string calldata uri) external onlyOwner {
        baseURI = uri;
    }

    function setRoyalty(address receiver, uint96 bps) external onlyOwner {
        if (bps > 1000) revert RoyaltyTooHigh();
        if (receiver == address(0)) revert ZeroAddress();
        royaltyReceiver = receiver;
        royaltyBps = bps;
    }

    function setOwner(address next) external onlyOwner {
        if (next == address(0)) revert ZeroAddress();
        owner = next;
    }

    // ---- ERC-721 transfer machinery ----

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
        address h = ownerOf(tokenId);
        if (h != from) revert WrongFrom();
        if (to == address(0)) revert ZeroAddress();
        if (msg.sender != h && msg.sender != getApproved[tokenId] && !isApprovedForAll[h][msg.sender]) {
            revert NotAuthorized();
        }
        delete getApproved[tokenId];
        unchecked {
            balanceOf[from]--;
            balanceOf[to]++;
        }
        holder[tokenId] = to;
        emit Transfer(from, to, tokenId);
    }

    function safeTransferFrom(address from, address to, uint256 tokenId) external {
        safeTransferFrom(from, to, tokenId, "");
    }

    function safeTransferFrom(address from, address to, uint256 tokenId, bytes memory data) public {
        transferFrom(from, to, tokenId);
        _checkReceiver(from, to, tokenId, data);
    }

    function _mint(address to, uint256 tokenId) internal {
        if (to == address(0)) revert ZeroAddress();
        unchecked {
            balanceOf[to]++;
        }
        holder[tokenId] = to;
        emit Transfer(address(0), to, tokenId);
        _checkReceiver(address(0), to, tokenId, "");
    }

    function _checkReceiver(address from, address to, uint256 tokenId, bytes memory data) internal {
        if (to.code.length == 0) return;
        try IERC721Receiver(to).onERC721Received(msg.sender, from, tokenId, data) returns (bytes4 ret) {
            if (ret != IERC721Receiver.onERC721Received.selector) revert UnsafeReceiver();
        } catch {
            revert UnsafeReceiver();
        }
    }
}

interface IERC721Receiver {
    function onERC721Received(address operator, address from, uint256 tokenId, bytes calldata data)
        external
        returns (bytes4);
}
