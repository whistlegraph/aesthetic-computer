import Foundation

struct StoryTape {
    static var script: String {
        guard let root = Bundle.main.url(forResource: "Web", withExtension: nil),
              let encoder = try? String(contentsOf: root.appendingPathComponent("canvas-tape.mjs")),
              let story = try? String(contentsOf: root.appendingPathComponent("story-tape.js")) else { return "" }
        let font = (try? Data(contentsOf: root.appendingPathComponent("ComicRelief-Regular.ttf")))?.base64EncodedString() ?? ""
        let bold = (try? Data(contentsOf: root.appendingPathComponent("ComicRelief-Bold.ttf")))?.base64EncodedString() ?? ""
        let loadFont = "window.whistlegraphStoryFont = Promise.all([new FontFace('WhistlegraphComic', 'url(data:font/ttf;base64,\(font))'), new FontFace('WhistlegraphComicBold', 'url(data:font/ttf;base64,\(bold))', {weight:'700'})].map(font => font.load().then(loaded => document.fonts.add(loaded))));\n"
        return loadFont + encoder.replacingOccurrences(of: "export function", with: "function") + "\n" + story
    }
}
