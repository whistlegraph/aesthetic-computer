import qrcode
from PIL import Image, ImageDraw, ImageFont, ImageOps
import random
import os
import requests
import json
from datetime import datetime
from io import BytesIO

# Paths
BASE_DIR = "/workspaces/aesthetic-computer"
INPUT_IMAGE_PATH = os.path.join(BASE_DIR, "system/public/assets/pruttipal/ads/wire-2025.png")
OUTPUT_IMAGE_PATH = os.path.join(BASE_DIR, "system/public/assets/pruttipal/ads/wire-2025-ac.png")
FONT_PATH = os.path.join(BASE_DIR, "system/public/assets/type/MatrixChunky8.bdf")

# API URLs
API_BASE = "https://aesthetic.computer" 
CHAT_API = f"{API_BASE}/api/chat/messages"
MOOD_API = f"{API_BASE}/api/mood/all"
MEDIA_API = f"{API_BASE}/media-collection"

# Static Content
KIDLISP_SNIPPETS = [
    "(wipe \"blue\")", "(ink \"lime\")", "(line 90 10 30 80)",
    "(repeat 100 point)", "(spin 1)", "(+ 1 2 3)", "(osc 440)",
    "(bgm \"satie\")", "(type \"hello\")", "(zoom 1.1)",
    "(play \"notes\")", "(camera 1)", "(download \"me\")"
]

PAINTING_CODES = ["#2025", "#wire", "#ac", "#art", "#code", "#love", "#glitch"]

# Colors
BG_COLOR = (180, 100, 60) 
TEXT_COLOR = (255, 255, 240) 
ACCENT_COLOR = (255, 180, 100) 
MATRIX_COLOR = (200, 255, 180) 
TIMESTAMP_COLOR = (220, 180, 150)

def fetch_data():
    print("Fetching data from API...")
    chats = []
    moods = []
    handles = set()
    paintings = []

    # Fetch Chats
    try:
        # Try without instance parameter first
        res = requests.get(f"{CHAT_API}?limit=150", timeout=10)
        print(f"Chat API status: {res.status_code}")
        if res.status_code == 200:
            data = res.json()
            messages = data.get("messages", [])
            print(f"  Got {len(messages)} messages")
            for msg in messages:
                if msg.get("text") and len(msg.get("text", "")) > 2:
                    chats.append({
                        "user": msg.get("from", "anon"),
                        "text": msg.get("text", ""),
                        "when": msg.get("when", "")
                    })
                    handles.add(msg.get("from", "anon"))
    except Exception as e:
        print(f"Error fetching chats: {e}")
    
    # Fallback static chats if fetch failed
    if len(chats) == 0:
        print("Using fallback chat data...")
        chats = [
            {"user": "@prutti", "text": "soak it in AC and send it back here", "when": "2025-11-24T09:00:00Z"},
            {"user": "@prutti", "text": "gotta support your work wherever I can!", "when": "2025-11-24T09:01:00Z"},
            {"user": "@jeffrey", "text": "welcome 2 aesthetic ;)", "when": "2025-11-24T09:02:00Z"},
            {"user": "@hvad93", "text": "let there be light - wuii", "when": "2025-11-24T09:03:00Z"},
            {"user": "@sat", "text": "yo whats good", "when": "2025-11-24T09:04:00Z"},
            {"user": "@nectarine", "text": "THE Ahh THING KEEPS SCARING ME", "when": "2025-11-24T09:05:00Z"},
            {"user": "@pibdge", "text": "VERY SILLYYYY", "when": "2025-11-24T09:06:00Z"},
            {"user": "@botce", "text": "I am a bot.", "when": "2025-11-24T09:07:00Z"},
            {"user": "@whistlegraph", "text": "drawing...", "when": "2025-11-24T09:08:00Z"},
        ]
        for c in chats:
            handles.add(c["user"])

    # Fetch Moods
    try:
        res = requests.get(MOOD_API)
        if res.status_code == 200:
            data = res.json()
            for m in data.get("moods", [])[:150]:
                moods.append({
                    "user": m.get("handle", "anon"),
                    "mood": m.get("mood", ""),
                    "when": m.get("when", "")
                })
                handles.add(m.get("handle", "anon"))
    except Exception as e:
        print(f"Error fetching moods: {e}")

    # Fetch Paintings (from a few active users)
    target_users = list(handles)[:5] if handles else ["prutti", "jeffrey", "hvad93"]
    # Ensure we have some defaults
    for u in ["prutti", "jeffrey"]:
        if u not in target_users: target_users.append(u)
        
    print(f"Fetching paintings for: {target_users}")
    for user in target_users:
        try:
            # Clean handle
            user_clean = user.replace("@", "")
            res = requests.get(f"{MEDIA_API}?for={user_clean}/painting")
            if res.status_code == 200:
                data = res.json()
                files = data.get("files", [])
                if files:
                    # Pick 1 random painting per user
                    paintings.append(random.choice(files))
        except Exception as e:
            print(f"Error fetching paintings for {user}: {e}")

    return chats, moods, list(handles), paintings

def download_image(url):
    try:
        res = requests.get(url)
        if res.status_code == 200:
            return Image.open(BytesIO(res.content)).convert("RGB")
    except Exception as e:
        print(f"Failed to download {url}: {e}")
    return None

def format_timestamp(iso_str):
    try:
        dt = datetime.fromisoformat(iso_str.replace("Z", "+00:00"))
        return dt.strftime("%H:%M")
    except:
        return ""

def generate_qr(url):
    qr = qrcode.QRCode(
        version=1,
        error_correction=qrcode.constants.ERROR_CORRECT_H,
        box_size=4,
        border=2,
    )
    qr.add_data(url)
    qr.make(fit=True)
    img = qr.make_image(fill_color="black", back_color="white")
    return img

def draw_rotated_text(img, text, font, xy, angle, fill):
    """Draws rotated text onto an image."""
    # Create a temporary image for the text
    # Estimate size
    dummy_draw = ImageDraw.Draw(Image.new("RGB", (1, 1)))
    bbox = dummy_draw.textbbox((0, 0), text, font=font)
    width = bbox[2] - bbox[0]
    height = bbox[3] - bbox[1]
    
    # Add some padding for rotation
    txt_img = Image.new("RGB", (width + 20, height + 20), (255, 255, 255))
    d = ImageDraw.Draw(txt_img)
    d.text((0, 0), text, font=font, fill=fill)
    
    # Rotate
    rotated = txt_img.rotate(angle, expand=1, resample=Image.BICUBIC, fillcolor=(255, 255, 255))
    
    # Paste
    # Center the paste on xy
    paste_x = int(xy[0] - rotated.width / 2)
    paste_y = int(xy[1] - rotated.height / 2)
    
    img.paste(rotated, (paste_x, paste_y))

def draw_multicolor_text(draw, xy, text, font):
    x, y = xy
    for char in text:
        # Random color from palette, fully opaque
        # Palette: Matrix Green, Accent Orange, Cream, Cyan, Magenta, Yellow
        palette = [
            MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR, 
            (0, 255, 255), (255, 0, 255), (255, 255, 0)
        ]
        color = random.choice(palette)
        draw.text((x, y), char, font=font, fill=color)
        # Advance x
        w = draw.textlength(char, font=font)
        x += w

def main():
    # Fetch dynamic data
    chats, moods, handles, painting_urls = fetch_data()
    
    if len(handles) < 10:
        handles.extend(["@prutti", "@jass", "@jeffrey", "@hvad93", "@ac_bot", "@wire_mag"])

    # Load base image
    try:
        base_img = Image.open(INPUT_IMAGE_PATH).convert("RGB")
    except FileNotFoundError:
        print(f"Error: Could not find {INPUT_IMAGE_PATH}")
        return

    # Load font
    try:
        # Use native BDF font
        font = ImageFont.load(FONT_PATH)
        print(f"Loaded font from {FONT_PATH}")
    except Exception as e:
        print(f"Warning: Could not load font {FONT_PATH}: {e}")
        font = ImageFont.load_default()

    print(f"Fetched {len(chats)} chats, {len(moods)} moods, {len(painting_urls)} paintings.")

    # Dimensions - Use Original Image Size
    final_w, final_h = base_img.size
    
    # Pixel Scale for the "AC Layer"
    # We want chunky pixels.
    PIXEL_SCALE = 12
    low_w = final_w // PIXEL_SCALE
    low_h = final_h // PIXEL_SCALE
    
    # Create low-res canvas for text/effects (Match base image)
    ac_layer = Image.new("RGB", (low_w, low_h), (255, 255, 255))
    draw = ImageDraw.Draw(ac_layer)
    
    # 1. Draw Matrix Background (Strictly in 3% margin)
    background_items = handles + KIDLISP_SNIPPETS + PAINTING_CODES
    MARGIN_PCT = 0.03
    
    for _ in range(2000): 
        # Pick a zone: Top, Bottom, Left, Right
        zone = random.choice(['top', 'bottom', 'left', 'right'])
        
        if zone == 'top':
            x = random.randint(0, low_w)
            y = random.randint(0, int(low_h * MARGIN_PCT))
        elif zone == 'bottom':
            x = random.randint(0, low_w)
            y = random.randint(int(low_h * (1 - MARGIN_PCT)), low_h)
        elif zone == 'left':
            x = random.randint(0, int(low_w * MARGIN_PCT))
            y = random.randint(0, low_h)
        else: # right
            x = random.randint(int(low_w * (1 - MARGIN_PCT)), low_w)
            y = random.randint(0, low_h)

        if random.random() > 0.6:
             text = random.choice(background_items)
             # Color is handled by draw_multicolor_text
        else:
             text = random.choice("0123456789ABCDEF<>[]{}")
         
        # Random rotation for some text (skip multicolor for rotated to keep simple, or implement later)
        if random.random() > 0.85:
             angle = random.randint(-90, 90)
             # Solid color for rotated
             color = random.choice([MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR])
             draw_rotated_text(ac_layer, text, font, (x, y), angle, color)
        else:
             draw_multicolor_text(draw, (x, y), text, font)

    # 2. Draw text around the border edges
    # Top edge - horizontal text
    x_pos = 5
    for item in (handles + KIDLISP_SNIPPETS)[:50]:
        if x_pos < low_w - 20:
            draw_multicolor_text(draw, (x_pos, 2), item, font)
            x_pos += draw.textlength(item, font=font) + 10
    
    # Bottom edge - horizontal text
    x_pos = 5
    for item in (handles + PAINTING_CODES)[:50]:
        if x_pos < low_w - 20:
            draw_multicolor_text(draw, (x_pos, low_h - 10), item, font)
            x_pos += draw.textlength(item, font=font) + 10
    
    # Left edge - vertical text (rotated)
    y_pos = 20
    for item in (KIDLISP_SNIPPETS + handles)[:40]:
        if y_pos < low_h - 20:
            draw_rotated_text(ac_layer, item, font, (5, y_pos), -90, random.choice([MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR]))
            y_pos += 15
    
    # Right edge - vertical text (rotated)
    y_pos = 20
    for item in (handles + PAINTING_CODES)[:40]:
        if y_pos < low_h - 20:
            draw_rotated_text(ac_layer, item, font, (low_w - 5, y_pos), 90, random.choice([MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR]))
            y_pos += 15
    
    # 3. Overlay Chat Messages (Left Edge, inset from border text)
    y_offset = 40
    x_offset = 15
    # Random sample
    display_chats = random.sample(chats, min(len(chats), 20))
    
    for chat in display_chats:
        user = chat['user']
        msg = chat['text']
        ts = format_timestamp(chat['when'])
        full_text = f"[{ts}] {user}: {msg}"
        
        # Wrap
        words = full_text.split()
        lines = []
        current_line = []
        for word in words:
            current_line.append(word)
            if len(" ".join(current_line)) > 20: 
                lines.append(" ".join(current_line[:-1]))
                current_line = [word]
        lines.append(" ".join(current_line))
        
        for line in lines:
            # Random slight rotation for chaos
            angle = random.randint(-5, 5)
            
            # Draw background rect (hard with rotation, skip for now or draw manually)
            # Just draw text with shadow
            draw.text((x_offset+1, y_offset+1), line, font=font, fill=(0,0,0))
            draw.text((x_offset, y_offset), line, font=font, fill=TEXT_COLOR)
            
            y_offset += 10
        y_offset += 8

    # 4. Overlay Moods (Right Edge, inset from border text)
    y_offset = 40
    # Random sample
    display_moods = random.sample(moods, min(len(moods), 20))
    
    for mood in display_moods:
        user = mood['user']
        text = mood['mood']
        ts = format_timestamp(mood['when'])
        full_text = f"{user} is {text} ({ts})"
        
        words = full_text.split()
        lines = []
        current_line = []
        for word in words:
            current_line.append(word)
            if len(" ".join(current_line)) > 18: 
                lines.append(" ".join(current_line[:-1]))
                current_line = [word]
        lines.append(" ".join(current_line))
        
        for line in lines:
            w = draw.textlength(line, font=font)
            draw_x = low_w - 10 - w
            
            draw.text((draw_x+1, y_offset+1), line, font=font, fill=(0,0,0))
            draw.text((draw_x, y_offset), line, font=font, fill=ACCENT_COLOR)
            y_offset += 10
        y_offset += 8

    # Upscale AC Layer
    print("Upscaling AC layer...")
    ac_layer_upscaled = ac_layer.resize((final_w, final_h), Image.NEAREST)

    # 2. Paste Painting Bits (High Res)
    print("Processing painting bits...")
    for url in painting_urls:
        p_img = download_image(url)
        if p_img:
            # Cut 10-20 bits from each painting (More blended pixels)
            for _ in range(random.randint(10, 20)):
                # Random crop size
                cw = random.randint(200, 800)
                ch = random.randint(200, 800)
                if cw < p_img.width and ch < p_img.height:
                    cx = random.randint(0, p_img.width - cw)
                    cy = random.randint(0, p_img.height - ch)
                    crop = p_img.crop((cx, cy, cx+cw, cy+ch))
                    
                    # Rotate
                    angle = random.randint(-45, 45)
                    crop = crop.rotate(angle, expand=True, resample=Image.BICUBIC)
                    
                    # Paste location (Strict 3% margin)
                    zone = random.choice(['top', 'bottom', 'left', 'right'])
                    MARGIN_PCT = 0.03
                    margin_w = int(final_w * MARGIN_PCT)
                    margin_h = int(final_h * MARGIN_PCT)

                    if zone == 'top':
                        px = random.randint(0, final_w)
                        py = random.randint(0, margin_h)
                    elif zone == 'bottom':
                        px = random.randint(0, final_w)
                        py = random.randint(final_h - margin_h, final_h)
                    elif zone == 'left':
                        px = random.randint(0, margin_w)
                        py = random.randint(0, final_h)
                    else: # right
                        px = random.randint(final_w - margin_w, final_w)
                        py = random.randint(0, final_h)

                    # Paste painting crop
                    ac_layer_upscaled.paste(crop, (px, py))

    # 3. Composite
    print("Compositing...")
    # Blend AC layer with base image
    # Use a mask to only show the margin areas
    final_canvas = base_img.copy()
    
    # Create a mask for the margin areas (3%)
    mask = Image.new("L", (final_w, final_h), 0)
    mask_draw = ImageDraw.Draw(mask)
    margin_w = int(final_w * 0.03)
    margin_h = int(final_h * 0.03)
    
    # Fill margin areas with white (255 = visible)
    mask_draw.rectangle([0, 0, final_w, margin_h], fill=255)  # Top
    mask_draw.rectangle([0, final_h - margin_h, final_w, final_h], fill=255)  # Bottom
    mask_draw.rectangle([0, 0, margin_w, final_h], fill=255)  # Left
    mask_draw.rectangle([final_w - margin_w, 0, final_w, final_h], fill=255)  # Right
    
    # Composite using the mask
    final_canvas.paste(ac_layer_upscaled, (0, 0), mask)

    # Add QR Code
    qr_img = generate_qr("https://aesthetic.computer/")
    qr_size = 600
    qr_img = qr_img.resize((qr_size, qr_size), Image.NEAREST)
    final_canvas.paste(qr_img, (final_w - qr_size - 100, final_h - qr_size - 100))

    # Save
    final_canvas.save(OUTPUT_IMAGE_PATH)
    print(f"Saved processed image to {OUTPUT_IMAGE_PATH}")

if __name__ == "__main__":
    main()
