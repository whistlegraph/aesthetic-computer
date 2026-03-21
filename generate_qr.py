import qrcode

# Data to encode
data = "https://venmo.com/u/FabianneGstottenmayr"

# Create QR code instance
qr = qrcode.QRCode(
    version=1,
    error_correction=qrcode.constants.ERROR_CORRECT_H,
    box_size=10,
    border=4,
)

# Add data
qr.add_data(data)
qr.make(fit=True)

# Create an image from the QR Code instance
img = qr.make_image(fill_color="black", back_color="white")

# Save it
output_path = "/workspaces/aesthetic-computer/fundraiser checklist/venmo_qr.png"
img.save(output_path)
print(f"QR code generated successfully at {output_path}")
