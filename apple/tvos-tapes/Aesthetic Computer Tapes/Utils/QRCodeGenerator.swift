// QRCodeGenerator.swift
// Generate QR codes using CoreImage
// 2026.01.01

import SwiftUI
import CoreImage.CIFilterBuiltins

struct QRCodeGenerator {
    static func generate(from string: String, size: CGFloat = 200) -> UIImage? {
        let context = CIContext()
        let filter = CIFilter.qrCodeGenerator()
        
        filter.message = Data(string.utf8)
        filter.correctionLevel = "M"
        
        guard let outputImage = filter.outputImage else { return nil }
        
        // Scale the QR code to desired size
        let scaleX = size / outputImage.extent.size.width
        let scaleY = size / outputImage.extent.size.height
        let scaledImage = outputImage.transformed(by: CGAffineTransform(scaleX: scaleX, y: scaleY))
        
        guard let cgImage = context.createCGImage(scaledImage, from: scaledImage.extent) else { return nil }
        
        return UIImage(cgImage: cgImage)
    }
}

// SwiftUI view for displaying a QR code
struct QRCodeView: View {
    let url: URL
    let size: CGFloat
    
    init(url: URL, size: CGFloat = 120) {
        self.url = url
        self.size = size
    }
    
    var body: some View {
        if let qrImage = QRCodeGenerator.generate(from: url.absoluteString, size: size) {
            Image(uiImage: qrImage)
                .interpolation(.none)
                .resizable()
                .scaledToFit()
                .frame(width: size, height: size)
                .background(Color.white)
                .cornerRadius(8)
        } else {
            Rectangle()
                .fill(Color.gray.opacity(0.3))
                .frame(width: size, height: size)
                .cornerRadius(8)
        }
    }
}
