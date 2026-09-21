import SwiftUI

/// Keep the desktop's provider identity visible while the native CLI bridge is
/// being ported. Unavailable engines must never silently fall back to AC.
struct AeselProviderPicker: View {
    let busy: Bool
    @Environment(\.paint) private var paint
    @State private var expanded = false
    @State private var palURL = URL(string: "https://pals.aesthetic.computer/random.webp?menu=\(UUID().uuidString)")!

    private func mark(_ provider: String) -> some View {
        Group {
            if provider == "ac" {
                AsyncImage(url: palURL) { image in
                    image.resizable().scaledToFit()
                } placeholder: {
                    Image("provider-ac").resizable().renderingMode(.template).scaledToFit().foregroundStyle(paint.ac)
                }
            } else {
                Image("provider-\(provider)").resizable()
                    .renderingMode(provider == "codex" ? .template : .original).scaledToFit()
            }
        }
        .frame(width: 25, height: 25).accessibilityHidden(true)
    }

    var body: some View {
        Button { expanded.toggle() } label: {
            HStack(spacing: 9) {
                mark("ac")
                Text("AC")
                Spacer()
                Image(systemName: "chevron.down").font(.system(size: 11))
            }
            .frame(maxWidth: .infinity).frame(height: 36)
            .contentShape(Rectangle())
        }
        .frame(height: 36)
        .disabled(busy)
        .accessibilityLabel("Provider, AC")
        .accessibilityValue(expanded ? "Expanded" : "Collapsed")
        .popover(isPresented: $expanded, arrowEdge: .bottom) {
            VStack(alignment: .leading, spacing: 4) {
                option("ac", title: "AC", available: true)
                option("claude", title: "Claude", available: false)
                option("codex", title: "Codex", available: false)
            }
            .padding(8).frame(width: 285)
            .font(Paint.font(15)).foregroundStyle(paint.ink)
            .background(paint.bg)
            .buttonStyle(AeselButtonStyle())
            .presentationCompactAdaptation(.popover)
        }
    }

    private func option(_ provider: String, title: String, available: Bool) -> some View {
        Button { expanded = false } label: {
            HStack(spacing: 9) {
                mark(provider)
                Text(title)
                Spacer()
                if available {
                    Image(systemName: "checkmark")
                } else {
                    Text("Needs desktop bridge").font(Paint.font(11))
                }
            }
            .padding(6).frame(maxWidth: .infinity, minHeight: 36)
            .contentShape(Rectangle())
        }
        .disabled(!available)
    }
}
