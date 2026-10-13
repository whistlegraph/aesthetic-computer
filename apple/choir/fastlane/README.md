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

### ios bundle

```sh
[bundle exec] fastlane ios bundle
```

Register the bundle id with the API key

### ios create

```sh
[bundle exec] fastlane ios create
```

Create the app record (once): FASTLANE_USER=<Apple ID> fastlane ios create

### ios build

```sh
[bundle exec] fastlane ios build
```

Archive Release and export a signed IPA

### ios upload

```sh
[bundle exec] fastlane ios upload
```

Upload the IPA to TestFlight

### ios distribute

```sh
[bundle exec] fastlane ios distribute
```

Make the private external group if missing, then submit this build to it

### ios invite

```sh
[bundle exec] fastlane ios invite
```

Add one external tester: fastlane ios invite email:<email> name:<first-name>

### ios status

```sh
[bundle exec] fastlane ios status
```

App Store Connect state for this build

----

This README.md is auto-generated and will be re-generated every time [_fastlane_](https://fastlane.tools) is run.

More information about _fastlane_ can be found on [fastlane.tools](https://fastlane.tools).

The documentation of _fastlane_ can be found on [docs.fastlane.tools](https://docs.fastlane.tools).
