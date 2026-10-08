import Foundation

/// Portable appearance only: no transcript, subject, paths, or model prose.
/// Seeds are hex strings so JavaScript consumers never lose UInt64 precision.
struct ProxCreature: Codable, Equatable {
    enum Stage: String, Codable { case egg, stirring, hatchling, familiar }
    enum Feature: String, Codable, CaseIterable { case ears, sprout, fins, feet, tail }
    enum Provider: String, Codable { case local = "apple-foundation-model", haiku = "claude-haiku" }
    struct Trait: Codable, Equatable {
        let feature: Feature
        let acquiredAt: Double
        let activeSeconds: Double
        let provider: Provider
    }

    let schemaVersion: Int
    let id: String
    let name: String
    let seed: String
    let bornAt: Double
    private(set) var activeSeconds: Double
    private(set) var stage: Stage
    private(set) var traits: [Trait]

    static func egg(id: String, name: String, seed: UInt64, now: Date = Date()) -> Self {
        Self(schemaVersion: 1, id: id, name: name,
             seed: String(format: "%016llx", seed), bornAt: now.timeIntervalSince1970 * 1000,
             activeSeconds: 0, stage: .egg, traits: [])
    }

    var numericSeed: UInt64 { UInt64(seed, radix: 16) ?? 0 }
    var appearanceKey: String { "v1:\(seed):\(stage.rawValue):\(traits.map { $0.feature.rawValue }.joined(separator: ","))" }
    var canAcquireFeature: Bool {
        traits.count < 3 && activeSeconds - (traits.last?.activeSeconds ?? 0) >= 2 * 3600
    }

    private static func stage(at seconds: Double) -> Stage {
        if seconds >= 8 * 3600 { return .familiar }
        if seconds >= 2 * 3600 { return .hatchling }
        if seconds >= 30 * 60 { return .stirring }
        return .egg
    }

    mutating func grow(by seconds: Double) {
        guard seconds.isFinite, seconds > 0 else { return }
        activeSeconds += seconds
        stage = Self.stage(at: activeSeconds)
    }

    @discardableResult
    mutating func acquire(_ feature: Feature, provider: Provider, now: Date = Date()) -> Bool {
        guard canAcquireFeature, !traits.contains(where: { $0.feature == feature }) else { return false }
        traits.append(Trait(feature: feature, acquiredAt: now.timeIntervalSince1970 * 1000,
                            activeSeconds: activeSeconds, provider: provider))
        return true
    }

    /// Reject unsupported or corrupt imports without replacing a saved identity.
    var isValid: Bool {
        schemaVersion == 1 && !id.isEmpty && !name.isEmpty && seed.count == 16
            && UInt64(seed, radix: 16) != nil && bornAt.isFinite
            && activeSeconds.isFinite && activeSeconds >= 0
            && stage == Self.stage(at: activeSeconds) && traits.count <= 3
            && Set(traits.map(\.feature)).count == traits.count
            && traits.enumerated().allSatisfy { index, trait in
                let prior = index == 0 ? 0 : traits[index - 1].activeSeconds
                return trait.acquiredAt.isFinite && trait.acquiredAt >= bornAt
                    && trait.activeSeconds.isFinite && trait.activeSeconds <= activeSeconds
                    && trait.activeSeconds - prior >= 2 * 3600
            }
    }

    var inferenceInstruction: String {
        let existing = traits.map { $0.feature.rawValue }.joined(separator: ", ")
        let available = Feature.allCases.filter { feature in !traits.contains { $0.feature == feature } }
            .map(\.rawValue).joined(separator: ", ")
        return """
        This session also has a persistent egg creature. Its existing features are: \(existing.isEmpty ? "none" : existing).
        \(canAcquireFeature ? "You may suggest ONE small new feature from: \(available), or none. Let a recurring quality of the work suggest it: ears for listening, a sprout for cultivation, fins for exploration, feet for building, a tail for play. Prefer none when the evidence is weak. Never replace its identity or existing features." : "It is too soon for another feature. Choose none.")
        Return a JSON object with exactly two keys: "memoir" (the summary paragraph) and "feature" (one allowed feature name or "none"). No Markdown.
        """
    }
}

/// A bad feature must not discard a good memoir; a bad response must never
/// become a character mutation. Plain paragraphs remain compatible with older models.
struct ProxCreatureInference {
    let memoir: String
    let feature: ProxCreature.Feature?

    static func parse(_ text: String) -> Self? {
        var clean = text.trimmingCharacters(in: .whitespacesAndNewlines)
        if clean.hasPrefix("```"), clean.hasSuffix("```"), let newline = clean.firstIndex(of: "\n") {
            clean = String(clean[clean.index(after: newline)...].dropLast(3))
                .trimmingCharacters(in: .whitespacesAndNewlines)
        }
        if let data = clean.data(using: .utf8),
           let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
           let memoir = obj["memoir"] as? String {
            let paragraph = memoir.trimmingCharacters(in: .whitespacesAndNewlines)
            guard !paragraph.isEmpty, paragraph.count <= 900 else { return nil }
            return Self(memoir: paragraph, feature: (obj["feature"] as? String).flatMap(ProxCreature.Feature.init(rawValue:)))
        }
        guard !clean.isEmpty, clean.count <= 900, !clean.hasPrefix("{"), !clean.hasPrefix("[") else { return nil }
        return Self(memoir: clean, feature: nil)
    }
}
