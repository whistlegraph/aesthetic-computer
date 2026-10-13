# choir on TestFlight and the App Store

Bundle `computer.aesthetic.choir`, team FB5948YR3S, seller Jeffrey (individual
account). Every lane except `create` runs on the Admin API key S4TQKG6U99.
Run everything from `apple/choir`. Lanes marked **jeffrey** need him.

## 1. Before the first build

1. **jeffrey, Apple ID:** create the app record (Apple's API cannot):
   `FASTLANE_USER=<Apple ID> fastlane ios create`
   It asks for his password and a 2FA code. If the name "choir" is taken,
   rerun with `CHOIR_STORE_NAME="choir: comments on culture"`.
2. Deploy lith so `https://choir.ac/terms/`, `/privacy/` and `/support/`
   are live (review checks the URLs), and set `CHOIR_BLOCKED_WORDS` in the vault
   `lith/.env` (comma-separated slur list; the filter reads it).
3. `fastlane ios meta`: name, subtitle, description, keywords, URLs, category,
   copyright, review notes and the age rating (`age_rating.json`). Creates
   version `MARKETING_VERSION` (1.0.0) if missing.
4. `fastlane ios privacy`: App Privacy label from `app_privacy_details.json`.
   If Apple refuses the key, fill the same answers by hand in App Store
   Connect › App Privacy (**jeffrey**).
5. **jeffrey, decisions in App Store Connect:** price (Free), availability
   (all countries or US only), the review contact phone
   (Fastfile reads `CHOIR_REVIEW_PHONE`; App Review also wants it in
   App Information), and the Content Rights answer (Discover shows third-party
   headlines and images).

## 2. TestFlight

6. `fastlane ios build`: xcodegen, archive, cloud-signed IPA in
   `~/Library/Developer/Choir-Releases/<version>-<build>/export/Choir.ipa`.
   The share extension's bundle id (`computer.aesthetic.choir.share`) is
   registered by Xcode during this step. Needs Xcode with the iOS platform
   installed (see the AC lane notes: archive on blueberry if neo lacks it).
7. `fastlane ios upload`, then `fastlane ios status` until the build reads VALID.
8. Internal testing works at once. For outside testers:
   `fastlane ios distribute` (submits the build to Beta App Review and makes
   the "Choir Friends" group), then `fastlane ios invite email:<email> name:<first>`.

## 3. App Review (later, only when jeffrey says ship)

9. The app must ask members to agree to the rules before they post, and the
   Report button should call `POST /api/choir?report=1` instead of opening Mail.
   Both server doors are live once lith deploys.
10. Screenshots: 6.9" iPhone (1320 x 2868) PNGs into `screenshots/ios/en-US/`,
    then `fastlane ios shots`.
11. **jeffrey:** `fastlane ios ship` attaches the newest VALID build to the
    version and submits it. Irreversible.

Each new version: bump `MARKETING_VERSION` and/or `CURRENT_PROJECT_VERSION` in
`../project.yml`, move `release_notes.next.txt` to
`metadata/ios/en-US/release_notes.txt` (the first version cannot have one),
then steps 3, 6, 7, 11.

## Lanes

`bundle` · `create` · `meta` · `shots` · `privacy` · `build` · `upload` ·
`distribute` · `invite` · `status` · `ship`
