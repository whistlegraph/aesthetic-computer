# ac-shop - Aesthetic Computer Shopify Tools

CLI tools for managing the Aesthetic Computer Shopify store.

## Quick Reference

### Theme Editing

The shop uses the **Spotlight** theme (ID: `141869547701`).

```bash
# Pull the theme locally
npx shopify theme pull --theme 141869547701 --store 3pc8se-sj.myshopify.com --path ./theme

# Push changes (with --allow-live to skip confirmation)
npx shopify theme push --theme 141869547701 --store 3pc8se-sj.myshopify.com --path ./theme --allow-live

# Push only specific files
npx shopify theme push --theme 141869547701 --store 3pc8se-sj.myshopify.com --path ./theme --only snippets/buy-buttons.liquid --allow-live

# List available themes
npx shopify theme list --store 3pc8se-sj.myshopify.com
```

### Key Theme Files

- `snippets/buy-buttons.liquid` - Buy button text (customized per product type)
- `sections/header.liquid` - Header navigation
- `templates/index.json` - Homepage content

### Product Types & Button Text

The buy button text is customized based on `product.type` in `snippets/buy-buttons.liquid`:

| Product Type | Button Text |
|-------------|-------------|
| Sketchbook | "Buy this sketchbook" |
| Book | "Buy this book" |
| Bike | "Buy this bike" |
| (record URL) | "Buy this record" |
| (@jeffrey URL) | "Book @jeffrey now" |
| Default | "Buy this artwork" |

### Product Management CLI

```bash
# List all products
node shopify.mjs list

# List with filter
node shopify.mjs list bikes

# Show product details
node shopify.mjs show 25.12.4.10.09

# Test API connection
node shopify.mjs test

# Sync product codes
node shopify.mjs sync
```

### Update Sketchbook Descriptions

```bash
# Updates all 5 @fifi sketchbooks with current description template
node update-tools.mjs
```

## Environment Setup

Requires credentials in `../aesthetic-computer-vault/shop/.env`:
- `SHOPIFY_STORE_DOMAIN` - Store domain (3pc8se-sj.myshopify.com)
- `SHOPIFY_ADMIN_ACCESS_TOKEN` - Admin API access token

### API Permissions

Manage app permissions at:
https://admin.shopify.com/store/3pc8se-sj/settings/apps/development

Required scopes:
- `read_products`, `write_products`
- `read_themes`, `write_themes`
- `read_locations` (for inventory management)
