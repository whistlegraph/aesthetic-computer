#!/usr/bin/env python3
"""Generate PDF from layout-test.html using playwright"""

import subprocess
import sys
import os

# Check if playwright is installed
try:
    from playwright.sync_api import sync_playwright
except ImportError:
    print("Installing playwright...")
    subprocess.run([sys.executable, "-m", "pip", "install", "playwright"], check=True)
    subprocess.run([sys.executable, "-m", "playwright", "install", "chromium"], check=True)
    from playwright.sync_api import sync_playwright

def generate_pdf():
    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page()
        
        # Load the HTML file
        file_path = os.path.abspath("layout-test.html")
        page.goto(f"file://{file_path}")
        
        # Wait for content to load
        page.wait_for_load_state("networkidle")
        
        # Generate PDF with 8.5x11 letter size
        page.pdf(
            path="fundraiser-poster.pdf",
            format="Letter",
            print_background=True,
            margin={
                "top": "0",
                "right": "0",
                "bottom": "0",
                "left": "0"
            }
        )
        
        browser.close()
        print("✓ PDF generated: fundraiser-poster.pdf")

if __name__ == "__main__":
    generate_pdf()
