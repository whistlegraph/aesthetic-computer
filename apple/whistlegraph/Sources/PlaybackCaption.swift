import SwiftUI

struct PlaybackCaption: View {
    let text: String
    let spokenRange: NSRange?
    let accent: Color
    private var caption: Text {
        let value = text as NSString
        guard let range = spokenRange, range.location != NSNotFound, NSMaxRange(range) <= value.length else { return Text(text) }
        return Text(value.substring(to: range.location)) +
            Text(value.substring(with: range)).bold().foregroundColor(accent) +
            Text(value.substring(from: NSMaxRange(range)))
    }
    var body: some View { caption }
}
