# Cryptocat Revival for AC DMs: Research Report

**Date:** January 31, 2026  
**Author:** Research for @jeffrey  
**Subject:** Feasibility of reviving Cryptocat-style encryption for Aesthetic Computer direct messages

---

## Executive Summary

**Yes, it's absolutely possible!** The cryptographic protocols that powered Cryptocat are now industry standards with mature, well-maintained implementations available for web browsers. Reviving Cryptocat's spirit for AC DMs would give users cryptographically secure, forward-secret messaging without the complexity of running external infrastructure.

---

## 1. Cryptocat History & Legacy

### What Made Cryptocat Special

Cryptocat (2011-2019) was created by **Nadim Kobeissi** (now a senior cryptography auditor at Cure53) and became famous for:

- **Accessible encryption** — "the encryption you can actually use in your daily life"
- Used by Glenn Greenwald to meet Edward Snowden in Hong Kong
- Scored **7/7 on EFF's Secure Messaging Scorecard**
- Banned in Iran shortly after launch
- GPL-3.0 licensed, fully open source

### Why It Shut Down

- Discontinued February 2019
- Last commit was 2017 (version 3.2.08)
- Domain sold (now redirects to Wire messenger)
- Nadim moved on to formal cryptography research and auditing

### The Good News

The **protocol designs are public domain** — Signal's Double Ratchet and X3DH specifications were explicitly released for anyone to implement. Cryptocat's final architecture (OMEMO-based) is an XMPP standard still in active development.

---

## 2. The Cryptographic Stack

### Core Protocols (All Public Domain)

| Protocol | Purpose | Status |
|----------|---------|--------|
| **X3DH** (Extended Triple Diffie-Hellman) | Key agreement / session establishment | Signal spec, widely implemented |
| **Double Ratchet** | Message encryption with forward secrecy | Signal spec, industry standard |
| **OMEMO** (XEP-0384) | Multi-device E2E encryption over XMPP | Active XMPP standard (v0.9.0, April 2025) |

### Cryptographic Primitives

Cryptocat's final version used:

- **AES-256-GCM** — Authenticated encryption (available in Web Crypto API)
- **Curve25519** — ECDH key agreement 
- **Ed25519** — Digital signatures
- **HMAC-SHA256** — Key derivation
- **HKDF** — Key derivation function (available in Web Crypto API)

### Web Crypto API Support

The browser's native `SubtleCrypto` API now supports most of what we need:

| Primitive | Web Crypto Support | Notes |
|-----------|-------------------|-------|
| AES-GCM | ✅ Full | Native |
| AES-CBC | ✅ Full | Native |
| ECDH (P-256) | ✅ Full | Native, but not Curve25519 |
| ECDSA | ✅ Full | Native |
| HKDF | ✅ Full | Native |
| HMAC-SHA256 | ✅ Full | Native |
| Curve25519/X25519 | ⚠️ Partial | Not in all browsers; use libsodium.js |
| Ed25519 | ⚠️ Partial | Not in all browsers; use libsodium.js |

**Key Insight:** For Curve25519/Ed25519 (Signal's preferred curves), we'd use **libsodium.js** (WebAssembly) which is fast, audited, and works everywhere.

---

## 3. Available JavaScript/TypeScript Libraries

### Signal Protocol Implementations

| Package | Weekly Downloads | Notes |
|---------|-----------------|-------|
| `@privacyresearch/libsignal-protocol-typescript` | ~7,600 | TypeScript, based on original libsignal-js |
| `@privacyresearch/libsignal-protocol-protobuf-ts` | ~7,800 | Protobuf types for Signal messages |
| `@getmaapp/signal-wasm` | New (Jan 2026) | Signal Protocol compiled to WASM |
| `signall-protocol-js` | Newer | Pure JS, X3DH + Double Ratchet |
| `murmur-chat` | New | E2E chat CLI using Signal Protocol |

### Cryptographic Primitives

| Package | Purpose |
|---------|---------|
| `libsodium-wrappers` | Curve25519, Ed25519, ChaCha20-Poly1305 |
| `tweetnacl` | Lightweight crypto primitives |
| `@noble/curves` | Modern, audited elliptic curves |
| `@noble/hashes` | SHA-256, HMAC, HKDF |

### Recommended Stack for AC

```
@noble/curves          — Curve25519, Ed25519 (audited, no WASM needed)
@noble/hashes          — SHA-256, HMAC, HKDF  
Web Crypto API         — AES-GCM, random bytes
@privacyresearch/libsignal-protocol-typescript — Full Signal Protocol
```

---

## 4. Architecture Proposal for AC DMs

### Session Server Integration

AC already has a real-time session server (Jamsocket). The encryption layer would work alongside it:

```
┌─────────────────────────────────────────────────────────────────┐
│                        AC DM Architecture                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────┐      Encrypted Messages      ┌──────────┐         │
│  │ Client A │ ◄──────────────────────────► │ Client B │         │
│  │          │                              │          │         │
│  │ ┌──────┐ │                              │ ┌──────┐ │         │
│  │ │Crypto│ │                              │ │Crypto│ │         │
│  │ │Store │ │                              │ │Store │ │         │
│  │ └──────┘ │                              │ └──────┘ │         │
│  └────┬─────┘                              └────┬─────┘         │
│       │                                         │               │
│       │         ┌─────────────────┐            │               │
│       └────────►│  Session Server │◄───────────┘               │
│                 │   (Jamsocket)   │                             │
│                 │                 │                             │
│                 │ • Relay only    │                             │
│                 │ • No plaintext  │                             │
│                 │ • Key bundles   │                             │
│                 └─────────────────┘                             │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Key Components

1. **Identity Key Pair** — Long-term Ed25519 keys per user
2. **Pre-Key Bundles** — Published to server, allows async session setup
3. **Double Ratchet Sessions** — Per-conversation, forward-secret
4. **Local Encrypted Storage** — IndexedDB with encrypted keys

### Security Properties (Same as Cryptocat/Signal)

- ✅ **End-to-end encryption** — Server sees only ciphertext
- ✅ **Forward secrecy** — Compromise doesn't reveal past messages
- ✅ **Future secrecy** (Break-in recovery) — Session heals after compromise
- ✅ **Deniability** — Can't cryptographically prove who sent what
- ✅ **Offline messages** — Works with async delivery
- ✅ **Multi-device** — OMEMO-style device management

---

## 5. Implementation Complexity

### Effort Estimate

| Component | Complexity | Notes |
|-----------|------------|-------|
| Crypto primitives | Low | Use existing libraries |
| X3DH key exchange | Medium | Well-documented, libraries exist |
| Double Ratchet | Medium-High | State management is tricky |
| Key storage (IndexedDB) | Medium | Need secure key derivation from password |
| UI for key verification | Low | QR codes, fingerprint display |
| Session server changes | Low | Just relay encrypted blobs |
| Multi-device sync | High | Optional, adds complexity |

### Minimum Viable Implementation

A basic 1:1 encrypted DM could be built with:

1. **~500 lines** of crypto wrapper code
2. **Existing libraries** for heavy lifting
3. **1-2 weeks** of focused development
4. **No server-side encryption logic** — server is just a relay

---

## 6. Trust Model Considerations

### Key Verification Options

1. **Trust on First Use (TOFU)** — Simple, what most people use
2. **QR Code Scanning** — In-person verification
3. **Safety Numbers** — Like Signal, compare fingerprints
4. **Blockchain/NFT Anchored** — Could tie identity keys to AC accounts

### What Cryptocat Got Right

> "With OMEMO you no longer trust user identities but device identities."

Each device has its own keys. Users verify devices, not just accounts. This prevents a compromised password from revealing past messages.

---

## 7. Why This Matters for AC

### Alignment with AC Values

- **Privacy** — Users own their conversations
- **Decentralization** — No central authority reads messages
- **Openness** — Can publish implementation for others
- **Creative freedom** — Private collaboration space

### Potential Features

- **Encrypted piece sharing** — Share unreleased work privately
- **Encrypted collaboration** — Real-time co-editing with E2E
- **Ephemeral rooms** — Cryptocat-style temporary chat spaces
- **Encrypted media** — Images, audio secured end-to-end

---

## 8. Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| Implementation bugs | Use audited libraries, consider audit |
| Key management UX | Keep it simple, TOFU by default |
| Browser storage security | Encrypt keys at rest, clear on logout |
| Metadata leakage | Server sees who talks to whom (unavoidable without Tor) |
| Regulatory concerns | Open source, standard protocols, no backdoors |

---

## 9. Recommended Next Steps

### Phase 1: Proof of Concept (1-2 weeks)
1. Set up `@privacyresearch/libsignal-protocol-typescript`
2. Implement basic X3DH handshake between two browsers
3. Send/receive encrypted messages via session server
4. Store keys in IndexedDB

### Phase 2: Production DMs (2-4 weeks)
1. Integrate with AC user accounts
2. Add key verification UI
3. Handle offline message delivery
4. Implement proper key rotation

### Phase 3: Advanced Features (Optional)
1. Multi-device support
2. Group encrypted chats
3. Encrypted file sharing
4. Message expiration

---

## 10. Resources

### Specifications
- [Signal Double Ratchet](https://signal.org/docs/specifications/doubleratchet/)
- [Signal X3DH](https://signal.org/docs/specifications/x3dh/)
- [OMEMO XEP-0384](https://xmpp.org/extensions/xep-0384.html)

### Libraries
- [@privacyresearch/libsignal-protocol-typescript](https://www.npmjs.com/package/@privacyresearch/libsignal-protocol-typescript)
- [@noble/curves](https://github.com/paulmillr/noble-curves)
- [libsodium.js](https://github.com/nickvdyck/libsodium.js)

### Historical Reference
- [Cryptocat GitHub (archived)](https://github.com/cryptocat/cryptocat)
- [Cryptocat Wikipedia](https://en.wikipedia.org/wiki/Cryptocat)

### Nadim Kobeissi (Original Creator)
- Now at Cure53 doing cryptography audits
- Teaching applied cryptography at American University of Beirut
- [nadim.computer](https://nadim.computer/)

---

## Conclusion

**Reviving Cryptocat's spirit for AC DMs is not only possible but practical.** The hard cryptographic work has been done — Signal's protocols are battle-tested and freely available. Modern JavaScript libraries make implementation straightforward. The main work is integration with AC's existing infrastructure and thoughtful UX design.

The result would be something Cryptocat always aimed to be: encryption so easy you forget you're using it, but strong enough that Glenn Greenwald could trust it to meet a whistleblower.

🔐✨
