import AppKit

@main
private enum NoPaintIconRenderer {
    static func main() throws {
        guard CommandLine.arguments.count == 2 else {
            fputs("usage: render-nopaint-icon OUTPUT.png\n", stderr)
            exit(2)
        }

        let image = NoPaintIcon.image()
        guard let tiff = image.tiffRepresentation,
              let bitmap = NSBitmapImageRep(data: tiff),
              let png = bitmap.representation(using: .png, properties: [:]) else {
            fputs("could not render No Paint icon\n", stderr)
            exit(1)
        }
        try png.write(to: URL(fileURLWithPath: CommandLine.arguments[1]), options: .atomic)
    }
}
