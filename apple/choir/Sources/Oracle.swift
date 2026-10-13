import SwiftUI

/// Fía (10-12, 22:29): "an affirmation/roll of dice option where like co star you
/// can get a directive or a sort of spiritual guidance kind of like pulling a
/// tarot card … artists need affirmations." (22:37) "pull your daily card so it's
/// just one a day … let's hide it for now": one card per person per day, no
/// re-rolls, and the button stays off until `shown` flips.
struct Card: Hashable {
    let title: String
    let words: String
}

enum Deck {
    static let shown = false   // Fía, 22:37: hidden for now
    static let cards: [Card] = [
        Card(title: "The Studio", words: "Go in today even if you only sit there. Being in the room is the work."),
        Card(title: "The Edit", words: "Take one thing out. What's left will say it louder."),
        Card(title: "The Sketchbook", words: "Make something bad on purpose. It's the door to the good one."),
        Card(title: "The Opening", words: "Show up for a friend's show. The scene is the people who come."),
        Card(title: "The Rejection", words: "A no is a sorting machine, not a verdict. Send the next one."),
        Card(title: "The Rest", words: "Lie down. Ideas arrive when you stop holding the door shut."),
        Card(title: "The Material", words: "Touch something you haven't worked with before. Let it lead."),
        Card(title: "The Archive", words: "Look at your old work kindly. You were already saying it."),
        Card(title: "The Invoice", words: "Ask for the fee. Your time is part of the piece."),
        Card(title: "The Window", words: "Sit by a window for ten minutes. Write down three colors."),
        Card(title: "The Chorus", words: "Tell another artist exactly what you love about their work."),
        Card(title: "The Fool", words: "Start before you're ready. Nobody is ever ready."),
        Card(title: "The Moon", words: "Trust the strange idea. It came to you for a reason."),
        Card(title: "The Sun", words: "Share something unfinished today. Light helps it grow."),
        Card(title: "The Tower", words: "If it's falling apart, it's making room. Keep the pieces."),
        Card(title: "The Star", words: "You don't have to be the best. You have to be specific."),
        Card(title: "The Hermit", words: "Turn your phone over for an hour. Listen to what's left."),
        Card(title: "The Lovers", words: "Make something for one person. Give it to them."),
        Card(title: "The Wheel", words: "Slow seasons are seasons too. The work keeps rooting."),
        Card(title: "The Hanged One", words: "Look at the piece upside down. Literally."),
        Card(title: "Strength", words: "Say the title out loud. Let it be as big as it wants."),
        Card(title: "Temperance", words: "Half the day for making, half for living. Both feed it."),
        Card(title: "The World", words: "Name one thing you made this year that you're proud of. Out loud."),
        Card(title: "The Proposal", words: "Write the wild version of the project first. Trim later."),
        Card(title: "The Walk", words: "Go outside without headphones. Bring back one image."),
        Card(title: "The Library", words: "Read something from outside your field. Steal a word."),
        Card(title: "The Mentor", words: "Ask someone ahead of you one real question."),
        Card(title: "The Student", words: "Teach something you know. You'll find out what you know."),
        Card(title: "The Draft", words: "Done is a material. Finish something small today."),
        Card(title: "The Mirror", words: "Your taste is a compass. Follow it even when it's quiet."),
        Card(title: "The Mess", words: "Don't clean the studio first. Make in the mess."),
        Card(title: "The Gift", words: "Give away a piece, a print, an idea. Generosity circulates."),
        Card(title: "The Deadline", words: "Pick the one that matters. Let the others go with love."),
        Card(title: "The Score", words: "Write instructions for a work instead of making it."),
        Card(title: "The Garden", words: "Water the slow project. Five minutes counts."),
        Card(title: "The Crowd", words: "You're allowed to take up space in the room. Stay for the whole thing."),
        Card(title: "The Voice", words: "Say it the way you'd say it to a friend. That's your statement."),
        Card(title: "The Daisy", words: "You are part of a choir. Your note is needed, not alone."),
        Card(title: "The Thread", words: "Follow the thing you keep returning to. It's not a rut, it's a thread."),
        Card(title: "The Night", words: "Make something small after dark. Nobody's watching."),
        Card(title: "The Morning", words: "Before anything else, one line, one mark, one note."),
        Card(title: "The Question", words: "What would you make if no one could ever see it? Make a bit of that."),
    ]

    /// Same card all day for the same person; a new one tomorrow.
    static func ofTheDay(for who: String, on day: Date = .now) -> Card {
        let stamp = day.formatted(.iso8601.year().month().day())
        var h: UInt64 = 1469598103934665603   // FNV-1a, stable across launches (Hasher isn't)
        for b in (who + "|" + stamp).utf8 { h = (h ^ UInt64(b)) &* 1099511628211 }
        return cards[Int(h % UInt64(cards.count))]
    }
}

struct OracleView: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    @Environment(\.dismiss) private var dismiss
    @State private var card: Card?
    @State private var flipped = false
    @State private var sharing = false
    @State private var note: String?

    var body: some View {
        VStack(spacing: 22) {
            Text(flipped ? "Your card for today" : "Pull your daily card")
                .font(Theme.type(14)).foregroundStyle(Theme.ink.opacity(0.6))
            ZStack {
                back.opacity(flipped ? 0 : 1)
                front.opacity(flipped ? 1 : 0).rotation3DEffect(.degrees(180), axis: (0, 1, 0))
            }
            .frame(width: 260, height: 380)
            .rotation3DEffect(.degrees(flipped ? 180 : 0), axis: (0, 1, 0), perspective: 0.6)
            .onTapGesture { if !flipped { turn() } }
            .accessibilityAddTraits(.isButton)
            .accessibilityLabel(flipped ? "\(card?.title ?? ""). \(card?.words ?? "")" : "Turn over your card")

            if flipped {
                HStack(spacing: 12) {
                    Text("A new one tomorrow").font(Theme.type(13)).foregroundStyle(Theme.ink.opacity(0.55))
                    if account.handle != nil {
                        Button(sharing ? "Sharing…" : "Share to chat") { Task { await share() } }
                            .buttonStyle(CuteDoorStyle(new: true, small: true)).disabled(sharing)
                    }
                }
                .transition(.opacity)
            } else {
                Text("Tap to turn it over").font(Theme.type(13)).foregroundStyle(Theme.ink.opacity(0.5))
            }
            if let note { Text(note).font(Theme.type(12)).foregroundStyle(Theme.ink.opacity(0.7)) }
        }
        .padding(24)
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(RuledPaper())
        .overlay(alignment: .topTrailing) {
            Button("Done") { dismiss() }.font(Theme.type(15)).tint(Theme.ink).padding(18)
        }
        .onAppear {
            card = Deck.ofTheDay(for: account.handle ?? Trail.session)
            Trail.send("oracle:open")
        }
    }

    var back: some View {
        RoundedRectangle(cornerRadius: 18).fill(Theme.ink)
            .overlay(RoundedRectangle(cornerRadius: 12).stroke(Color.white.opacity(0.5), lineWidth: 1).padding(12))
            .overlay(Image("Mark").renderingMode(.template).resizable().scaledToFit().foregroundStyle(.white).padding(56))
            .shadow(color: .black.opacity(0.25), radius: 12, y: 6)
    }

    var front: some View {
        RoundedRectangle(cornerRadius: 18).fill(Theme.paper)
            .overlay(RoundedRectangle(cornerRadius: 12).stroke(Theme.ink.opacity(0.25), lineWidth: 1).padding(12))
            .overlay {
                VStack(spacing: 18) {
                    Image("Mark").renderingMode(.template).resizable().scaledToFit().frame(height: 34).foregroundStyle(Theme.head)
                    Text(card?.title ?? "").font(.system(size: 26, weight: .semibold, design: .serif)).foregroundStyle(Theme.ink)
                    Text(card?.words ?? "").font(.system(size: 18, design: .serif)).foregroundStyle(Theme.ink)
                        .lineSpacing(3).multilineTextAlignment(.center)
                }
                .padding(30)
            }
            .shadow(color: .black.opacity(0.18), radius: 12, y: 6)
    }

    func turn() {
        withAnimation(.spring(duration: 0.7, bounce: 0.25)) { flipped = true }
        Trail.send("oracle:turn", ["card": card?.title ?? ""])
    }

    func share() async {
        guard let card else { return }
        sharing = true; defer { sharing = false }
        do {
            _ = try await API.post(.init(text: "🃏 I pulled \(card.title): “\(card.words)”", tags: [], replyTo: nil), token: await account.token())
            Trail.send("oracle:share", ["card": card.title])
            await room.load(); dismiss()
        } catch { note = error.localizedDescription }
    }
}
