import SwiftUI

/// The desktop launch chooser, adapted for touch: New media and saved Threads.
struct AeselHomeView: View {
    let session: Session
    let host: SessionHost
    let resume: () -> Void
    @State private var tab = 0
    @Environment(\.paint) private var paint

    private let media: [(id: String, name: String)] = [
        ("piece", "Piece"), ("picture", "Picture"), ("sound", "Sound"),
        ("paper", "Paper"), ("gameboy", "Game Boy")
    ]

    var body: some View {
        VStack(spacing: 0) {
            HStack {
                if !session.route.isEmpty {
                    Button("/resume", action: resume).foregroundStyle(paint.dim)
                }
                Spacer()
                if session.signedIn {
                    AeselHandle(handle: session.handle, colors: session.handleColors)
                } else {
                    Button("/login") { host.signIn() }.foregroundStyle(paint.dim)
                }
            }
            .font(Paint.font(20))
            .padding(18)

            Spacer(minLength: 24)
            AeselWordmark().scaleEffect(1.5).padding(.bottom, 36)

            VStack(alignment: .leading, spacing: 22) {
                HStack(spacing: 18) {
                    tabButton("New media", index: 0)
                    tabButton("Threads", index: 1)
                }
                if tab == 0 {
                    VStack(spacing: 3) {
                        ForEach(media, id: \.id) { medium in
                            Button {
                                host.newSession(medium: medium.id)
                                resume()
                            } label: {
                                HStack(spacing: 12) {
                                    Text(medium.id == "piece" ? "›" : " ")
                                    Text(medium.name)
                                    Spacer(minLength: 0)
                                    if medium.id != "piece" {
                                        Text("coming later").font(Paint.font(17)).foregroundStyle(paint.dim)
                                    }
                                }
                                .padding(.horizontal, 12)
                                .frame(height: 48)
                                .foregroundStyle(medium.id == "piece" ? paint.ink : paint.dim)
                                .background(medium.id == "piece" ? Color(rgb: 0xc81e64) : .clear)
                            }
                            .disabled(medium.id != "piece" || session.busy)
                            .accessibilityLabel(medium.id == "piece" ? "New piece" : medium.name + ", not available in this app yet")
                        }
                    }
                } else {
                    ScrollView {
                        LazyVStack(alignment: .leading, spacing: 2) {
                            if session.history.isEmpty {
                                Text("No saved threads").foregroundStyle(paint.dim).padding(.vertical, 12)
                            }
                            ForEach(session.history) { thread in
                                Button {
                                    host.resumeSession(id: thread.id)
                                    resume()
                                } label: {
                                    VStack(alignment: .leading, spacing: 5) {
                                        Text("› " + thread.title)
                                            .lineLimit(2)
                                            .foregroundStyle(paint.ink)
                                        Text(thread.medium + " · " + date(thread.updatedAt))
                                            .font(Paint.font(17))
                                            .foregroundStyle(paint.dim)
                                        if !thread.route.isEmpty {
                                            Text(thread.route).font(Paint.font(17)).foregroundStyle(paint.dim).lineLimit(1)
                                        }
                                    }
                                    .frame(maxWidth: .infinity, alignment: .leading)
                                    .padding(12)
                                    .background(thread.id == session.currentSessionID ? paint.deep.opacity(0.65) : .clear)
                                }
                                .disabled(session.busy)
                            }
                        }
                    }
                    .frame(height: 252)
                }
            }
            .font(Paint.font(22))
            .frame(maxWidth: 380)
            .padding(.horizontal, 24)

            Spacer(minLength: 20)
            AeselDonkey(busy: session.busy, failed: session.health == .failed)
                .padding(.bottom, 10)

        }
        .buttonStyle(AeselButtonStyle())
        .foregroundStyle(paint.ink)
        .background { AeselCloth().ignoresSafeArea() }
    }

    private func tabButton(_ label: String, index: Int) -> some View {
        Button { tab = index } label: {
            Text(label)
                .padding(.horizontal, 8).padding(.vertical, 7)
                .foregroundStyle(tab == index ? paint.ink : paint.dim)
                .background(tab == index ? Color(rgb: 0xc81e64) : .clear)
        }
        .accessibilityAddTraits(tab == index ? [.isSelected] : [])
    }

    private func date(_ text: String) -> String {
        String(text.prefix(16)).replacingOccurrences(of: "T", with: " ")
    }
}
