import qrcode
from PIL import Image, ImageDraw, ImageFont
import random
import os
import requests
from datetime import datetime
from io import BytesIO

# Paths
BASE_DIR = "/workspaces/aesthetic-computer"
INPUT_IMAGE_PATH = os.path.join(BASE_DIR, "system/public/assets/pruttipal/ads/wire-2025.png")
OUTPUT_IMAGE_PATH = os.path.join(BASE_DIR, "system/public/assets/pruttipal/ads/wire-2025-ac.png")
FONT_PATH = os.path.join(BASE_DIR, "system/public/assets/type/MatrixChunky8.bdf")

# API
CHAT_API = "https://aesthetic.computer/api/chat/messages"

# Colors - Holiday palette (red, green, gold, white)
COLORS = [
    (220, 20, 60),    # Crimson red
    (34, 139, 34),    # Forest green
    (255, 215, 0),    # Gold
    (255, 255, 255),  # White
    (139, 0, 0),      # Dark red
    (0, 100, 0),      # Dark green
    (218, 165, 32),   # Goldenrod
]

def get_random_color_with_opacity():
    """Get a random holiday color with INSANE random opacity between 30-100%"""
    color = random.choice(COLORS)
    opacity = random.randint(76, 255)  # 30% to 100% opacity (WILD!)
    return color + (opacity,)

def draw_rotated_text(img, text, font, xy, angle, fill):
    """Draw rotated text onto an image with transparent background and drop shadow"""
    dummy_draw = ImageDraw.Draw(Image.new("RGB", (1, 1)))
    bbox = dummy_draw.textbbox((0, 0), text, font=font)
    width = bbox[2] - bbox[0]
    height = bbox[3] - bbox[1]
    
    # Create transparent background with extra padding for shadow and large text
    padding = 400
    txt_img = Image.new("RGBA", (width + padding, height + padding), (255, 255, 255, 0))
    d = ImageDraw.Draw(txt_img)
    
    # Draw drop shadow (1px offset black text)
    shadow_offset = 1
    d.text((padding//2 + shadow_offset, padding//2 + shadow_offset), text, font=font, fill=(0, 0, 0, 220))
    # Draw main text
    d.text((padding//2, padding//2), text, font=font, fill=fill)
    
    rotated = txt_img.rotate(angle, expand=1, resample=Image.BICUBIC)
    
    # Paste with alpha channel
    paste_x = int(xy[0] - rotated.width / 2)
    paste_y = int(xy[1] - rotated.height / 2)
    img.paste(rotated, (paste_x, paste_y), rotated)

def fetch_laer_klokken_chats():
    """Fetch chats from Laer-Klokken (aesthetic.computer chat)"""
    print("Fetching Laer-Klokken chats...")
    chats = []
    
    try:
        # Try with chat-clock instance first (max limit is 100)
        headers = {'User-Agent': 'Mozilla/5.0'}
        res = requests.get(f"{CHAT_API}?instance=chat-clock&limit=100", headers=headers, timeout=10)
        print(f"  Chat API status: {res.status_code}")
        
        if res.status_code == 200:
            data = res.json()
            messages = data.get("messages", [])
            print(f"  Fetched {len(messages)} chat-clock messages")
            for msg in messages:
                if msg.get("text") and len(msg.get("text", "")) > 2:
                    user = msg.get("from", "anon")
                    text = msg.get("text", "")
                    chats.append(f"{user}: {text}")
        else:
            print(f"  Response: {res.text[:200]}")
            
    except Exception as e:
        print(f"  Error: {e}")
    
    # If we got no chats, use some real examples from the API
    if len(chats) == 0:
        print("  Using example chat-clock messages...")
        chats = [
            "@prutti: hi hi Kejser kom her ind: 'laer-klokken'",
            "@prutti: @kejser just click",
            "@prutti: hvordan gik det at synge i var det kirken?",
            "@prutti: ??",
            "@hamstergoth: hii",
            "@hamstergoth: Im goth",
            "@hamstergoth: like thats my name",
            "@KTGWorks: har hørt fra Horsens, vi venter bare",
            "@nathansonic: hi",
            "@prutti: @JAS were are you and we need to talk, about that wire file",
        ]
    
    return chats

def draw_text_along_edge(img, draw, font, text, edge, w, h, margin_pct=0.07):
    """Draw text along one edge of the canvas with rotation"""
    margin_w = int(w * margin_pct)
    margin_h = int(h * margin_pct)
    
    color = get_random_color_with_opacity()
    
    if edge == 'top':
        # Spread across full width
        y = random.randint(10, max(15, margin_h - 20))
        x = random.randint(50, max(100, w - 500))
        angle = random.randint(-15, 15)  # INSANE rotation!
        draw_rotated_text(img, text, font, (x, y), angle, color)
        
    elif edge == 'right':
        # Spread across full height with more x variation
        x = w - random.randint(100, max(150, margin_w - 100))
        y = random.randint(50, max(100, h - 500))
        angle = 90 + random.randint(-15, 15)  # INSANE rotation!
        draw_rotated_text(img, text, font, (x, y), angle, color)
        
    elif edge == 'bottom':
        # Spread across full width with more y variation
        y = h - random.randint(100, max(150, margin_h - 100))
        x = random.randint(50, max(100, w - 500))
        angle = random.randint(-15, 15)  # INSANE rotation!
        draw_rotated_text(img, text, font, (x, y), angle, color)
        
    elif edge == 'left':
        # Spread across full height with more x variation
        x = random.randint(100, max(150, margin_w - 100))
        y = random.randint(50, max(100, h - 500))
        angle = -90 + random.randint(-15, 15)  # INSANE rotation!
        draw_rotated_text(img, text, font, (x, y), angle, color)

def main():
    # Load base image
    print("Loading base image...")
    base_img = Image.open(INPUT_IMAGE_PATH).convert("RGB")
    final_w, final_h = base_img.size
    print(f"  Base image: {final_w}x{final_h}")
    
    # Load font at larger size (128pt for readable text without memory issues)
    try:
        font = ImageFont.truetype("/workspaces/aesthetic-computer/system/public/assets/type/webfonts/ProggyClean.ttf", 128)
        print(f"  Loaded ProggyClean font at size 128")
        # Verify font size
        test_draw = ImageDraw.Draw(Image.new("RGB", (1, 1)))
        test_bbox = test_draw.textbbox((0, 0), "Test", font=font)
        print(f"  Font verification - 'Test' renders as {test_bbox[2] - test_bbox[0]}x{test_bbox[3] - test_bbox[1]} pixels")
    except Exception as e:
        print(f"  ERROR loading font: {e}")
        # Try fallback to processing font
        try:
            font = ImageFont.truetype("/workspaces/aesthetic-computer/system/public/assets/type/webfonts/ywft-processing-bold.ttf", 2048)
            print(f"  Loaded fallback processing font at size 2048")
        except:
            font = ImageFont.load_default()
            print(f"  Using tiny default font")
    
    # Fetch chats
    chats = fetch_laer_klokken_chats()
    print(f"  Got {len(chats)} chat messages")
    
    # Extend chats to ensure we have enough for very high density
    while len(chats) < 1000:
        chats.extend(chats[:min(100, 1000 - len(chats))])
    
    # Work at final resolution for proper text size
    print(f"  Working at full resolution: {final_w}x{final_h}")
    
    ac_layer = Image.new("RGBA", (final_w, final_h), (255, 255, 255, 0))
    draw = ImageDraw.Draw(ac_layer)
    
    # Snake chats around all 4 edges
    # Strategy: cycle through edges, place chats sequentially
    edges = ['top', 'right', 'bottom', 'left']
    chat_idx = 0
    
    # Place ~1000 chat messages around the edges (INSANE!)
    print(f"  Placing 1000 INSANE text messages...")
    for i in range(1000):
        if chat_idx >= len(chats):
            chat_idx = 0
        
        edge = edges[chat_idx % 4]
        text = chats[chat_idx]
        
        if i % 10 == 0:
            print(f"    Placed {i} messages...")
        
        draw_text_along_edge(ac_layer, draw, font, text, edge, final_w, final_h, margin_pct=0.07)
        chat_idx += 1
    
    # Create mask for 7% margin
    print("  Creating margin mask...")
    mask = Image.new("L", (final_w, final_h), 0)
    mask_draw = ImageDraw.Draw(mask)
    margin_w = int(final_w * 0.07)
    margin_h = int(final_h * 0.07)
    
    mask_draw.rectangle([0, 0, final_w, margin_h], fill=255)  # Top
    mask_draw.rectangle([0, final_h - margin_h, final_w, final_h], fill=255)  # Bottom
    mask_draw.rectangle([0, 0, margin_w, final_h], fill=255)  # Left
    mask_draw.rectangle([final_w - margin_w, 0, final_w, final_h], fill=255)  # Right
    
    # Composite with alpha channel
    print("  Compositing...")
    final_canvas = base_img.copy()
    final_canvas.paste(ac_layer, (0, 0), ac_layer)
    
    # Add QR code
    print("  Adding QR code...")
    qr = qrcode.QRCode(version=1, box_size=10, border=2)
    qr.add_data("https://aesthetic.computer/")
    qr.make(fit=True)
    qr_img = qr.make_image(fill_color="black", back_color="white").convert("RGB")
    
    qr_size = 600
    qr_img = qr_img.resize((qr_size, qr_size), Image.NEAREST)
    
    final_canvas.paste(qr_img, (100, final_h - qr_size - 100))
    
    # Save
    print("  Saving...")
    final_canvas.save(OUTPUT_IMAGE_PATH)
    print(f"✓ Saved to {OUTPUT_IMAGE_PATH}")

if __name__ == "__main__":
    main()
