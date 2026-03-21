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
MEDIA_API = f"{API_BASE}/media-collection"

# Colors
BG_COLOR = (180, 100, 60) 
TEXT_COLOR = (255, 255, 240) 
ACCENT_COLOR = (255, 180, 100) 
MATRIX_COLOR = (200, 255, 180) 

def fetch_chats():
    print("Fetching chats from API...")
    chats = []
    
    try:
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
    
    return chats

def fetch_paintings():
    print("Fetching paintings...")
    paintings = []
    target_users = ["prutti", "jeffrey", "hvad93", "sat", "whistlegraph"]
    
    for user in target_users:
        try:
            res = requests.get(f"{MEDIA_API}?for={user}/painting")
            if res.status_code == 200:
                data = res.json()
                files = data.get("files", [])
                if files:
                    paintings.append(random.choice(files))
        except Exception as e:
            print(f"Error fetching paintings for {user}: {e}")
    
    return paintings

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
    qr = qrcode.QRCode(version=1, error_correction=qrcode.constants.ERROR_CORRECT_H, box_size=4, border=2)
    qr.add_data(url)
    qr.make(fit=True)
    return qr.make_image(fill_color="black", back_color="white")

def draw_rotated_text(img, text, font, xy, angle, fill):
    dummy_draw = ImageDraw.Draw(Image.new("RGB", (1, 1)))
    bbox = dummy_draw.textbbox((0, 0), text, font=font)
    width = bbox[2] - bbox[0]
    height = bbox[3] - bbox[1]
    
    txt_img = Image.new("RGB", (width + 20, height + 20), (255, 255, 255))
    d = ImageDraw.Draw(txt_img)
    d.text((0, 0), text, font=font, fill=fill)
    rotated = txt_img.rotate(angle, expand=1, resample=Image.BICUBIC, fillcolor=(255, 255, 255))
    
    paste_x = int(xy[0] - rotated.width / 2)
    paste_y = int(xy[1] - rotated.height / 2)
    img.paste(rotated, (paste_x, paste_y))

def draw_multicolor_text(draw, xy, text, font):
    x, y = xy
    palette = [MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR, (0, 255, 255), (255, 0, 255), (255, 255, 0)]
    for char in text:
        color = random.choice(palette)
        draw.text((x, y), char, font=font, fill=color)
        x += draw.textlength(char, font=font)

def main():
    # Fetch data
    chats = fetch_chats()
    painting_urls = fetch_paintings()
    
    # Load base image
    try:
        base_img = Image.open(INPUT_IMAGE_PATH).convert("RGB")
    except FileNotFoundError:
        print(f"Error: Could not find {INPUT_IMAGE_PATH}")
        return

    # Load font
    try:
        font = ImageFont.load(FONT_PATH)
        print(f"Loaded font from {FONT_PATH}")
    except Exception as e:
        print(f"Warning: Could not load font {FONT_PATH}: {e}")
        font = ImageFont.load_default()

    print(f"Fetched {len(chats)} chats, {len(painting_urls)} paintings.")

    # Dimensions
    final_w, final_h = base_img.size
    PIXEL_SCALE = 12
    low_w = final_w // PIXEL_SCALE
    low_h = final_h // PIXEL_SCALE
    
    # Create low-res layer
    ac_layer = Image.new("RGB", (low_w, low_h), (255, 255, 255))
    draw = ImageDraw.Draw(ac_layer)
    
    # Prepare chat items
    chat_items = []
    for chat in chats:
        user = chat['user']
        msg = chat['text']
        ts = format_timestamp(chat['when'])
        chat_items.append(f"[{ts}] {user}: {msg}")
    
    # Repeat if needed
    while len(chat_items) < 200:
        chat_items.extend(chat_items[:min(50, 200 - len(chat_items))])
    
    chat_idx = 0
    
    # Snake chats around all edges
    # Top edge (left to right)
    x_pos = 5
    while x_pos < low_w - 20 and chat_idx < len(chat_items):
        text = chat_items[chat_idx]
        draw_multicolor_text(draw, (x_pos, 2), text, font)
        x_pos += draw.textlength(text, font=font) + 15
        chat_idx += 1
    
    # Right edge (top to bottom)
    y_pos = 20
    while y_pos < low_h - 20 and chat_idx < len(chat_items):
        text = chat_items[chat_idx]
        draw_rotated_text(ac_layer, text, font, (low_w - 5, y_pos), 90, random.choice([MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR]))
        y_pos += 20
        chat_idx += 1
    
    # Bottom edge (right to left)
    x_pos = low_w - 10
    while x_pos > 20 and chat_idx < len(chat_items):
        text = chat_items[chat_idx]
        text_width = draw.textlength(text, font=font)
        x_pos -= text_width
        if x_pos > 0:
            draw_multicolor_text(draw, (x_pos, low_h - 10), text, font)
            x_pos -= 15
        chat_idx += 1
    
    # Left edge (bottom to top)
    y_pos = low_h - 40
    while y_pos > 20 and chat_idx < len(chat_items):
        text = chat_items[chat_idx]
        draw_rotated_text(ac_layer, text, font, (5, y_pos), -90, random.choice([MATRIX_COLOR, ACCENT_COLOR, TEXT_COLOR]))
        y_pos -= 20
        chat_idx += 1

    # Upscale
    print("Upscaling...")
    ac_layer_upscaled = ac_layer.resize((final_w, final_h), Image.NEAREST)

    # Paste painting bits
    print("Processing painting bits...")
    for url in painting_urls:
        p_img = download_image(url)
        if p_img:
            for _ in range(random.randint(10, 20)):
                cw = random.randint(200, 800)
                ch = random.randint(200, 800)
                if cw < p_img.width and ch < p_img.height:
                    cx = random.randint(0, p_img.width - cw)
                    cy = random.randint(0, p_img.height - ch)
                    crop = p_img.crop((cx, cy, cx+cw, cy+ch))
                    crop = crop.rotate(random.randint(-45, 45), expand=True, resample=Image.BICUBIC)
                    
                    # Paste in 3% margin
                    zone = random.choice(['top', 'bottom', 'left', 'right'])
                    margin_w = int(final_w * 0.03)
                    margin_h = int(final_h * 0.03)

                    if zone == 'top':
                        px = random.randint(0, final_w)
                        py = random.randint(0, margin_h)
                    elif zone == 'bottom':
                        px = random.randint(0, final_w)
                        py = random.randint(final_h - margin_h, final_h)
                    elif zone == 'left':
                        px = random.randint(0, margin_w)
                        py = random.randint(0, final_h)
                    else:
                        px = random.randint(final_w - margin_w, final_w)
                        py = random.randint(0, final_h)

                    ac_layer_upscaled.paste(crop, (px, py))

    # Composite
    print("Compositing...")
    final_canvas = base_img.copy()
    
    # Mask for 3% margins
    mask = Image.new("L", (final_w, final_h), 0)
    mask_draw = ImageDraw.Draw(mask)
    margin_w = int(final_w * 0.03)
    margin_h = int(final_h * 0.03)
    
    mask_draw.rectangle([0, 0, final_w, margin_h], fill=255)
    mask_draw.rectangle([0, final_h - margin_h, final_w, final_h], fill=255)
    mask_draw.rectangle([0, 0, margin_w, final_h], fill=255)
    mask_draw.rectangle([final_w - margin_w, 0, final_w, final_h], fill=255)
    
    final_canvas.paste(ac_layer_upscaled, (0, 0), mask)

    # Add QR Code
    qr_img = generate_qr("https://aesthetic.computer/")
    qr_size = 600
    qr_img = qr_img.resize((qr_size, qr_size), Image.NEAREST)
    final_canvas.paste(qr_img, (final_w - qr_size - 100, final_h - qr_size - 100))

    # Save
    final_canvas.save(OUTPUT_IMAGE_PATH)
    print(f"Saved to {OUTPUT_IMAGE_PATH}")

if __name__ == "__main__":
    main()
