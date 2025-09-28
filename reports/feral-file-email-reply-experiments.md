# Feral File Integration: Tezos FA2 Contract for KidLisp

*Date: September 1, 2025*  
*To: Sean Moss-Pultz <sean@feralfile.com>*  
*From: Jeffrey Alan Scudder <me@jas.life>*

Hi Sean,

Thanks for the update. I want to focus on one thing: **Tezos FA2 integration for KidLisp**.

## What's Already Working

KidLisp pieces automatically generate shareable $codes when users create them:
- Write code → Gets cached as `$abc123` → Appears in live feed → Others can embed it
- Every piece attributed to creator's @handle
- DP-1 playlists already supported
- 100% compatible with iframe + `window.postMessage`
- Perfect for kids: simple, visual, immediate feedback

## The Addition: Optional 1/1 Token Minting

When someone creates a KidLisp piece they love, they can choose to mint it as a unique Tezos FA2 token:
- User decides: "This one's special" → Mints 1/1 token for chosen price
- Creator gets 92.5% of mint price
- Fee split: 2.5% Feral File (device), 2.5% Aesthetic Computer (development), 2.5% Objkt (marketplace)
- Secondary sales: 10% creator royalty + platform fees
- Feral File curates the best pieces in your gallery interface

**Contract Details**: [Full FA2 Specification](https://github.com/whistlegraph/aesthetic-computer/blob/main/reports/kidlisp-fa2-contract-spec.md)

## For FF-X1 Kids & Families

Interactive creative games that kids can play together:
- Draw with code instead of just consuming content
- Share creations instantly via $codes  
- Parents can mint their kid's masterpieces as keepsakes
- Builds digital literacy through play

## Technical Implementation

The integration is already built and tested:
- [Tezos FA2 contract](https://github.com/whistlegraph/aesthetic-computer/blob/main/tezos/src/integration.js)
- [Auto-minting on cache](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/netlify/functions/store-kidlisp.mjs#L845-L870)
- [DP-1 playlist support](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/netlify/functions/playlist.mjs)

## For FF-X1

Kids create interactive art on the device. Gallery curates the best pieces. Parents can mint favorites as 1/1 keepsakes. More families want devices.

That's the experiment: **Can creative coding games + optional tokenization drive FF-X1 family adoption?**

Let me know if you want to move forward.

Jeffrey

---

**Key Files:**
- [Tezos Integration](https://github.com/whistlegraph/aesthetic-computer/blob/main/tezos/src/integration.js)
- [KidLisp System](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/public/aesthetic.computer/lib/kidlisp.mjs)  
- [Auto-minting Backend](https://github.com/whistlegraph/aesthetic-computer/blob/main/system/netlify/functions/store-kidlisp.mjs)
