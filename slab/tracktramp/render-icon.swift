import AppKit

@main
private enum TrackDrumIconRenderer {
    static func main() throws {
        guard CommandLine.arguments.count == 2 else {
            fputs("usage: render-icon output.png\n", stderr)
            exit(2)
        }

        let image = TrackDrumIcon.image(size: 1024, accent: .systemPink)
        guard let tiff = image.tiffRepresentation,
              let bitmap = NSBitmapImageRep(data: tiff),
              let png = bitmap.representation(using: .png, properties: [:]) else {
            exit(1)
        }
        try png.write(
            to: URL(fileURLWithPath: CommandLine.arguments[1]),
            options: .atomic
        )
    }
}
