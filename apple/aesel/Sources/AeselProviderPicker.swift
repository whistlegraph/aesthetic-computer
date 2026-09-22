import SwiftUI

/// Provider choices reflect the connected host; never silently fall back to AC.
struct AeselProviderPicker: View {
    let session: Session
    let host: SessionHost
    @Environment(\.paint) private var paint
    @State private var expanded = false
    @State private var palURL = URL(string: "https://pals.aesthetic.computer/random.webp?menu=\(UUID().uuidString)")!

    private func mark(_ provider: String, size: CGFloat = 28) -> some View {
        Group {
            if provider == "ac" {
                AsyncImage(url: palURL) { image in
                    image.resizable().scaledToFill()
                } placeholder: {
                    Image("provider-ac").resizable().renderingMode(.template).scaledToFill().foregroundStyle(paint.ac)
                }
            } else {
                Image("provider-\(provider)").resizable()
                    .renderingMode(provider == "codex" ? .template : .original).scaledToFit().padding(3)
            }
        }
        .frame(width: size, height: size).background(paint.ink.opacity(0.06))
        .clipShape(RoundedRectangle(cornerRadius: 4))
        .overlay { RoundedRectangle(cornerRadius: 4).stroke(paint.ink.opacity(0.18), lineWidth: 0.5) }
        .accessibilityHidden(true)
    }

    var body: some View {
        Button { expanded.toggle() } label: {
            HStack(spacing: 9) {
                mark(session.provider)
                providerName(session.provider)
                Spacer()
                Image(systemName: "chevron.down").font(.system(size: 11))
            }
            .frame(maxWidth: .infinity).frame(height: 36)
            .contentShape(RoundedRectangle(cornerRadius: 8))
        }
        .frame(height: 36)
        .disabled(session.busy || session.hostOperationID != nil)
        .accessibilityLabel("Provider, \(session.provider == "ac" ? "Aesthetic.Computer" : session.provider.capitalized)")
        .accessibilityValue(expanded ? "Expanded" : "Collapsed")
        .popover(isPresented: $expanded, arrowEdge: .bottom) {
            VStack(alignment: .leading, spacing: 0) {
                option("ac", title: "Aesthetic.Computer", available: true)
                option("claude", title: "Claude", available: session.providers.first { $0.id == "claude" }?.available == true)
                option("codex", title: "Codex", available: session.providers.first { $0.id == "codex" }?.available == true)
                Button("Refresh connection") { host.refreshProviders() }.padding(6)
            }
            .padding(8)
            .frame(width: 310)
            .font(Paint.font(15)).foregroundStyle(paint.ink)
            .background(paint.bg, in: RoundedRectangle(cornerRadius: 14))
            .clipShape(RoundedRectangle(cornerRadius: 14))
            .buttonStyle(AeselButtonStyle())
            .presentationCompactAdaptation(.popover)
        }
    }

    private func providerName(_ provider: String) -> Text {
        if provider == "ac" {
            return Text("Aesthetic") + Text(".").foregroundColor(Color(red: 1, green: 0.25, blue: 0.6)) + Text("Computer")
        }
        return Text(provider.capitalized)
    }

    private func option(_ provider: String, title: String, available: Bool) -> some View {
        Button { host.setProvider(provider); expanded = false } label: {
            HStack(spacing: 9) {
                mark(provider, size: 28)
                providerName(provider)
                Spacer()
                if session.provider == provider {
                    Image(systemName: "checkmark")
                } else if !available {
                    Text("Not connected").font(Paint.font(11))
                }
            }
            .padding(.horizontal, 8).frame(maxWidth: .infinity, minHeight: 40)
            .contentShape(RoundedRectangle(cornerRadius: 8))
        }
        .disabled(!available)
    }
}
