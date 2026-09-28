import Foundation
import Vision
import ImageIO
let expected=CommandLine.arguments[1]
for path in CommandLine.arguments.dropFirst(2) {
 guard let source=CGImageSourceCreateWithURL(URL(fileURLWithPath:path) as CFURL,nil),let image=CGImageSourceCreateImageAtIndex(source,0,nil) else { fatalError("Cannot read image") }
 let request=VNDetectBarcodesRequest();request.symbologies=[.qr]
 try VNImageRequestHandler(cgImage:image).perform([request])
 print(path+": "+(request.results ?? []).compactMap{$0.payloadStringValue}.joined(separator:" | "))
 if !(request.results ?? []).contains(where: {$0.payloadStringValue == expected}) { exit(1) }
}
