# 🛒 Shopify Integration Engineering Plan

## Current State

### Existing Integration Points:
1. **`shop.mjs`** - Static array of product codes used for routing
2. **`products.mjs`** - Rich product display system (books, records) in prompt.mjs
3. **`prompt.mjs`** - Routes shop codes via `/shop~code` to Shopify
4. **`netlify.toml`** - Redirect rules mapping codes to Shopify product URLs
5. **`ac-shop` CLI** - New admin tool for managing products

### Data Flow:
```
User types code → prompt.mjs checks shop.mjs → redirects to shop.aesthetic.computer
```

---

## 🎯 Integration Goals

### Phase 1: Live Product Data (Replace Static Arrays)
**Goal:** Fetch product data from Shopify instead of hardcoding

```javascript
// Instead of static shop.mjs:
const signed = ["25.4.8.21.19", ...];

// Dynamic API endpoint:
GET /api/shop/products → Returns live product list from Shopify
```

**Implementation:**
- [ ] Create Netlify function: `functions/shop-products.mjs`
- [ ] Cache Shopify responses (5-15 min TTL)
- [ ] Update `shop.mjs` to export from API or fallback to static
- [ ] Auto-generate redirect rules from Shopify handles

### Phase 2: Rich Product Display in Prompt
**Goal:** Show product images/info when hovering codes in prompt

```javascript
// products.mjs enhancement:
- Fetch product metadata (title, image, price) from Shopify
- Display product cards in corner carousel
- Support new product types: sketchbooks, bikes, tools
```

**New Product Types:**
- [ ] `sketchbook` - Similar to book but with @creator attribution
- [ ] `bike` - Large product with specs
- [ ] `tool` - Simple sketchbook variant

### Phase 3: Homepage Reel / Product Showcase
**Goal:** Visual product browser on homepage or dedicated piece

```javascript
// New piece: `shop.mjs` or integrate into `prompt.mjs`
- Horizontal scrolling product reel
- Sixel thumbnails in terminal mode
- Category filters (books, music, pictures, tools, bikes)
- Search by code or title
```

**Design Options:**
1. **Carousel Mode** - Swipeable cards in prompt corner (current)
2. **Gallery Mode** - Full-screen grid with filtering
3. **Reel Mode** - TikTok-style vertical scroll
4. **Terminal Mode** - `ac-shop list -i` with sixel images

### Phase 4: Inventory & Order Sync
**Goal:** Real-time inventory display and order notifications

```javascript
// Webhooks from Shopify:
- Product created/updated → Update client cache
- Order placed → Notification to ac-event-daemon
- Inventory changed → Update availability display
```

**Implementation:**
- [ ] Shopify webhook endpoint: `functions/shopify-webhook.mjs`
- [ ] WebSocket broadcast for real-time updates
- [ ] "SOLD" overlay when inventory hits 0

### Phase 5: Creator Attribution System
**Goal:** Link products to @handles, show creator pages

```javascript
// Product metadata:
{
  code: "25.12.4.11.21",
  title: "I welcome new experiences",
  creator: "@fifi",
  type: "sketchbook",
  location: "Los Angeles (Fia's House)"
}

// Creator page: aesthetic.computer/@fifi/shop
// Shows all products by that creator
```

---

## 📐 Architecture

### API Endpoints (Netlify Functions)

```
/api/shop/products          - List all products (cached)
/api/shop/products/:code    - Get single product by code
/api/shop/categories        - List product categories
/api/shop/creator/:handle   - Products by creator
/api/shop/webhook           - Shopify webhook receiver
```

### Client-Side Modules

```
lib/shop.mjs          - Product codes & routing (upgrade to dynamic)
lib/shop-api.mjs      - NEW: Fetch product data from API
disks/common/products.mjs - Product display classes (extend types)
disks/shop.mjs        - NEW: Full shop browser piece
```

### Data Model

```typescript
interface Product {
  // Identifiers
  id: string;                    // Shopify product ID
  code: string;                  // Our code format (25.x.x.x.x)
  handle: string;                // Shopify URL handle
  
  // Display
  title: string;
  description: string;
  imageUrl: string;
  thumbnailUrl: string;          // Smaller version for lists
  
  // Metadata
  type: 'book' | 'record' | 'picture' | 'sketchbook' | 'bike' | 'tool';
  creator: string;               // @handle
  price: number;
  currency: string;
  
  // Inventory
  available: boolean;
  quantity: number;
  location: string;
  
  // Media (optional)
  audioUrl?: string;             // For records
  videoUrl?: string;             // For demos
}
```

---

## 🛠️ Implementation Priority

### Quick Wins (This Week)
1. ✅ `ac-shop` CLI with sixel images
2. [ ] Add `ac-shop activate` command to set products active
3. [ ] Add `ac-shop qr <code>` to generate QR codes
4. [ ] Commit vault changes and update shop.mjs codes

### Medium Term (Next 2 Weeks)  
5. [ ] Create `/api/shop/products` Netlify function
6. [ ] Add sketchbook/tool/bike types to products.mjs
7. [ ] Update prompt carousel to show new product types

### Longer Term (Month)
8. [ ] Shopify webhooks for real-time sync
9. [ ] Creator shop pages (@handle/shop)
10. [ ] Full shop browser piece with filtering
11. [ ] Order notifications via ac-event-daemon

---

## 🔗 File Locations

```
ac-shop/
├── shopify.mjs           # Main CLI tool
├── create-tools.mjs      # One-off product creation scripts
├── update-tools.mjs      # Product update scripts
└── package.json

system/
├── netlify.toml          # Redirect rules
├── netlify/functions/
│   ├── shop-products.mjs # NEW: Product API
│   └── shopify-webhook.mjs # NEW: Webhook handler
└── public/aesthetic.computer/
    ├── lib/shop.mjs      # Product codes
    ├── lib/shop-api.mjs  # NEW: API client
    └── disks/
        ├── prompt.mjs    # Uses products.mjs
        ├── shop.mjs      # NEW: Shop browser
        └── common/products.mjs # Product display

aesthetic-computer-vault/
└── shop/.env             # Shopify credentials
```

---

## 💡 Ideas for Future

1. **Print-on-Demand Integration** - Auto-create products from paintings
2. **NFT Bridge** - Link Tezos NFTs to physical products
3. **Subscription Products** - Recurring access to content
4. **Gift Cards** - Digital codes redeemable in shop
5. **Auction Mode** - Time-limited bidding on unique items
6. **Bundle Builder** - Create custom product bundles
7. **AR Preview** - See products in space before buying
