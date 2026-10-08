fastlane documentation
----

# Installation

Make sure you have the latest version of the Xcode command line tools installed:

```sh
xcode-select --install
```

For _fastlane_ installation instructions, see [Installing _fastlane_](https://docs.fastlane.tools/#installing-fastlane)

# Available Actions

## iOS

### ios create

```sh
[bundle exec] fastlane ios create
```

Create the first app record using a terminal Apple ID login (once)

### ios build

```sh
[bundle exec] fastlane ios build
```

Bundle, archive Release with two compiler jobs, and export a signed IPA

### ios upload

```sh
[bundle exec] fastlane ios upload
```

Upload the signed IPA to TestFlight using the Apple API key

### ios distribute

```sh
[bundle exec] fastlane ios distribute
```

Submit this version/build for external beta review and notify its private group

### ios invite

```sh
[bundle exec] fastlane ios invite
```

Add one external tester: fastlane ios invite email:<email> name:<first-name>

### ios status

```sh
[bundle exec] fastlane ios status
```

Read App Store Connect versions and builds using the Apple API key

----

This README.md is auto-generated and will be re-generated every time [_fastlane_](https://fastlane.tools) is run.

More information about _fastlane_ can be found on [fastlane.tools](https://fastlane.tools).

The documentation of _fastlane_ can be found on [docs.fastlane.tools](https://docs.fastlane.tools).
