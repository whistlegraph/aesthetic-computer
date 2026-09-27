# Oskiewar build and release plan

Researched September 27, 2026. Prices are USD before tax. Estimates below are
planning assumptions, not measured performance or promised approval dates.

**Keep AppVeyor for validation, use shader live reload, and use a persistent
Windows worker when native iteration warrants it.** Paying for another hosted
job helps concurrency; retaining compiler state addresses the repeated work in
our current build.

[AppVeyor build 499](https://ci.appveyor.com/project/whistlegraph/aesthetic-computer/build/1.0.499)
took approximately 4m46s from submission to completion;
[build 500](https://ci.appveyor.com/project/whistlegraph/aesthetic-computer/build/1.0.500)
took 4m49s. Build 500's timestamped log gives this approximate breakdown:

| Stage | Time |
| --- | ---: |
| Waiting / starting worker | 66s |
| Checkout, dependency cache, setup | 20s |
| Windows portable tests | 51s |
| GDK desktop build and smoke test | 49s |
| UWP build and packaging | 86s |
| Artifact upload and cache completion | 15s |

The UWP link alone spends **54 seconds generating optimized code**. It already
uses `/LTCG:incremental`, but fresh workers discard the `.iobj`/`.ipdb` state.
QuickJS is compiled separately for portable tests, GDK desktop, and UWP;
`test-native-windows.cmd` deletes its output directory on every run. The GDK
dependency cache already restores in about two seconds. These are clean native
builds with a warm dependency cache; a fully cold dependency build was not
measured.

Applied September 27: gameplay-only changes now bypass AppVeyor through a
native-input filter in `appveyor.yml`. JS continues to publish straight to the
Xbox, without a Windows package. Generated photo atlases are optional again in
clean builds; missing local `.rgba` files no longer fail packaging. A fleet
Windows tower is currently offline, so it is not an available worker. No VM has
been provisioned and no paid plan changed.

Recommended changes, in order:

1. Use live JS/shader delivery so those edits avoid native builds. Native build
   52 applied a shader on the Xbox in **85 ms** on September 27. An invalid shader
   was rejected while the game kept running; resetting the packaged shader also
   passed. This path currently covers the post-processing pixel shader.
2. Publish the Xbox development artifact after portable checks and the UWP
   build; run GDK desktop validation outside that artifact's critical path.
   Keep full validation required for releases. This removes approximately 49s
   of measured waiting from the development path.
3. Preserve incremental compiler/linker state on a persistent worker and stop
   deleting portable test outputs. Cache keys must include compiler, flags,
   platform, sources and headers; desktop and UWP objects remain separate.
4. Benchmark `/p:PreferredToolArchitecture=x64`; the current UWP log invokes
   the 32-bit-hosted tools. Upload the MSIX, x64 dependency and certificate
   before the remaining installer files; the current glob uploads 31 files.
5. If necessary, benchmark a development configuration retaining `/O2` while
   disabling whole-program optimization. Preserve the release configuration
   and verify game performance before adopting the development variant.

Microsoft documents [incremental LTCG](https://learn.microsoft.com/en-us/cpp/build/reference/ltcg-link-time-code-generation?view=msvc-170)
and [64-bit-hosted build tools](https://learn.microsoft.com/en-us/cpp/build/reference/msbuild-visual-cpp-overview?view=msvc-170).
A 30–90s warm loop for small native edits is a target to test after these
changes, not an existing result.

| Build option | Cost and tradeoff |
| --- | --- |
| Current AppVeyor | $0 extra; optimize the development path first. |
| AppVeyor Premium | $99/month for two concurrent jobs; potentially $49.50 with an approved OSS discount. Basic $29 and Pro $59 still provide one job. Published priority is technical support, not a faster single-build guarantee. [Pricing](https://www.appveyor.com/pricing/) |
| Existing Windows PC + AppVeyor agent | No additional AppVeyor fee: the free plan includes five self-hosted jobs. Retains installed tools and intermediates. [Worker setup](https://www.appveyor.com/docs/byoc/windows/) |
| AWS Lightsail Windows | $124/month for 4 vCPU, 16GB RAM, 320GB SSD, Windows license included. Burstable CPU suits intermittent builds. Charges continue while stopped. [Bundles](https://docs.aws.amazon.com/lightsail/latest/userguide/amazon-lightsail-bundles.html), [licensing/performance](https://docs.aws.amazon.com/lightsail/latest/userguide/amazon-lightsail-faq-instances.html), [billing](https://docs.aws.amazon.com/en_en/lightsail/latest/userguide/amazon-lightsail-frequently-asked-questions-faq-billing-and-account-management.html) |
| Azure Windows D4as_v5 | Approximately $81/month at 176 running hours, or $278 at 730 hours, including a P10 disk. Official retail API queried September 27: West US 2 Windows compute $0.356/hour; P10 LRS disk $17.92/month. Networking extra. Deallocate between sessions to stop compute billing while retaining disk. [Price API](https://learn.microsoft.com/en-us/rest/api/cost-management/retail-prices/azure-retail-prices), [size](https://learn.microsoft.com/en-us/azure/virtual-machines/sizes/general-purpose/dasv5-series), [billing states](https://learn.microsoft.com/en-us/azure/virtual-machines/states-billing) |
| DigitalOcean | Not a supported Windows Droplet path. Paperspace Windows templates are unavailable to new accounts since July 2024. [Droplet policy](https://docs.digitalocean.com/support/can-i-use-windows-on-a-droplet/), [Paperspace policy](https://docs.digitalocean.com/products/paperspace/machines/how-to/connect/) |

Also benchmark GitHub Actions if its recorded billing lock is cleared: public
Windows runners provide 4 CPU/16GB at no charge. They still start fresh, so do
not provide persistent incremental state.
[GitHub runner specifications](https://docs.github.com/en/actions/how-tos/write-workflows/choose-where-workflows-run/choose-the-runner-for-a-job)

**Steam is the shorter release path: approximately 3–5 weeks with a fixed launch
scope. The partner account is cleared; the game still needs store/build review.**

[STEAM.md](steam/STEAM.md) records the $100 fee paid September 1, app ID
5280790 assigned September 15, a packaged Electron shell, and Windows/macOS/Linux
depot setup. [The fill sheet](steam/store-page/fill.md) records the store page
filled, trailer uploaded September 16, and $9.99 price chosen.

A read-only check of the authenticated Steamworks dashboard on September 27
supersedes the older account notes:

| Steam gate | Verified state |
| --- | --- |
| Partner account / tax | Full partner dashboard available; company page explicitly says the organization's tax information is verified. |
| Pricing permission | The package pricing form opens normally. The old payee restriction is gone. No price is currently set, approved or published; zero pending changes. |
| Store review | Store Presence checklist complete, but still offers “Mark as ready for review…”; this is ready to submit, not approved. |
| Game build | Checklist incomplete: no configured build, platform support mismatch, and missing package pricing / published pricing. |
| Public release | App unavailable; store package hidden. Public Coming Soon has not started. October 15 is the configured target date, not a cleared release date. |

Evidence: [app dashboard](https://partner.steamgames.com/apps/landing/5280790)
and [package pricing](https://partner.steamgames.com/packages/pricing/1827387)
(account access required). No values were edited and no review was submitted.

The existing shell needs maintenance before upload. A read-only check of its
`trim()` function against today's `mac-test.html` fails at the
`__oskiewarVersusCapable` replacement. Its README records macOS boot verification,
but Windows/Linux were only cross-built and still need target-system tests.
Using this shell avoids waiting for the native Xbox renderer port. Store assets
and feature claims must match whichever version we choose to release.

Valve requires **14 days of public Coming Soon**. Store-page and build reviews
typically take 3–5 business days each; Valve asks for at least seven business
days of allowance. Build preparation and review can overlap the Coming Soon
window. October 15 is possible only if Coming Soon is public by October 1 and
the other gates clear; mid-to-late October is a more practical conditional
target. [Coming Soon](https://partner.steamgames.com/doc/store/coming_soon),
[review process](https://partner.steamgames.com/doc/store/review_process)

There is a current source discrepancy: Valve's English
[onboarding page](https://partner.steamgames.com/doc/gettingstarted/onboarding)
says **21 days** after paying the app fee, while
[Steam Direct](https://partner.steamgames.com/steamdirect) still says **30**.
The authenticated Oskiewar dashboard explicitly applies **21 days since the
first app-credit purchase**, resolving which rule this account currently sees.
The recorded September 1 payment is already more than 21 days old; the public
Coming Soon requirement remains the material waiting period. The per-product
fee remains $100, recoupable after
$1,000 adjusted gross revenue. [Fee terms](https://partner.steamgames.com/doc/gettingstarted/appfee)

**Retail Xbox needs a separate engineering effort: budget 8–12 weeks or more,
with program access and devkit timing unresolved.** Today's Dev Mode deployment
is already useful for testing, but Microsoft says new UWP games are no longer
accepted in the Xbox Store.
[UWP guidance](https://learn.microsoft.com/en-us/windows/uwp/gaming/getting-started)

The GDK desktop smoke test now passes, but it is not a console game build. The
remaining work includes the D3D11-to-D3D12.x renderer/text port, platform APIs,
Xbox users and achievements, lifecycle handling, packaging and hardware tests.
The game source is now bundled in the native package; the older claim in
`PUBLISHING.md` that only the smoke piece ships is obsolete. Dev live-reload
facilities remain development-only. A narrow local-only launch bounds the work.
[Console graphics requirements](https://learn.microsoft.com/en-us/gaming/gdk/docs/gdk-dev/intro/introduction),
[console SDK access](https://github.com/microsoft/GDK)

Microsoft account approval has positive evidence too: July 18 emails titled
“Welcome to Windows Dev Center” and “We've verified your profile and you may
start exploring Partner Center” confirm the Aesthetic Computer publisher
account was ready and its Partner Center profile verified. These messages were
read from the existing mail archive, which contains mail through September 27.

ID@Xbox program/concept/title approval and console GDK access remain
**unverified**, rather than known absent. On September 27, both the Xbox partner
portal and Partner Center redirected the existing signed-in account to a
“We're updating our terms” Microsoft Services Agreement screen. No terms were
accepted. A narrow search of the relevant mail accounts found the account
approvals above, but no ID@Xbox concept approval notice. The next check is the
title's status in those portals after the account owner completes that screen;
Dev Mode registration alone does not establish retail-title approval.

ID@Xbox application is free. Microsoft lists concept
decisions at 10–15 business days, account verification usually 3–5 business days,
and dev-store setup usually five; engineering can proceed alongside onboarding.
Once submission-ready, digital-final certification has a published **four
business day SLA per submission**. Failed submissions require fixes and another
pass; that SLA is not a release-date promise.
[Onboarding](https://learn.microsoft.com/en-us/gaming/game-publishing/onboarding/overview),
[certification](https://learn.microsoft.com/en-us/gaming/game-publishing/concepts/certification/certification-guide)

Do not describe Xbox Creators as definitively discontinued: current Store
policy still names it, while current UWP guidance rejects its historical
package type. A usable enrollment shortcut was not verified. Plan around
ID@Xbox/GDK unless Microsoft confirms another route for this title.
[Current Store policy](https://learn.microsoft.com/en-us/windows/apps/publish/store-policies#1013-gaming-and-xbox)

The practical sequence is to prepare the Steam launch build and start its public
Coming Soon clock, verify the existing Xbox program/title status in parallel,
and continue immediate Xbox testing through Dev Mode. No services were purchased
or provisioned, no applications submitted, and no messages sent during this
research.
