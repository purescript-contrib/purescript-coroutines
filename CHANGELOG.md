# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

- Function `unfoldrProducer` constructs a `Producer` like `Unfoldable.unfoldr` does.
- Function `transformWithState` constructs a `Transformer` using a step function with state.

Bugfixes:

Other improvements:

## [v7.0.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v7.0.0) - 2022-04-27

Breaking changes:

- Update project and deps to PureScript v0.15.0 (#39 by @JordanMartinez)

New features:

Bugfixes:

Other improvements:

- Added `purs-tidy` formatter (#38 by @thomashoneyman)

## [v6.0.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:

- Added support for PureScript 0.14 and dropped support for all previous versions (#34)

New features:

Bugfixes:

Other improvements:

- Added an in-depth library explanation to the docs folder (#31)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#33)

## [v5.0.1](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v5.0.1) - 2019-08-27

- Raised upper bound on `purescript-freet` (@safareli)

## [v5.0.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v5.0.0) - 2018-05-27

- Updated for PureScript 0.12

## [v3.1.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v3.1.0) - 2016-11-18

- Added `pullFrom` combinator to bias a process in favour of the consumer

## [v3.0.1](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v3.0.1) - 2016-11-14

- Fixed shadowed name warning

## [v3.0.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v3.0.0) - 2016-10-22

- Updated dependencies

## [v2.0.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v2.0.0) - 2016-07-17

- Now uses `MonadPar` to step fused coroutines in parallel.

## [v1.3.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v1.3.0) - 2016-06-18

- Added `composeCoTransformers`

## [v1.2.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v1.2.0) - 2016-06-18

- Added `transformCoTransformL` and `transformCoTransformR`

## [v1.1.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v1.1.0) - 2016-06-18

- Added `CoTransformer`

## [v1.0.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v1.0.0) - 2016-06-05

- Updated for 1.0 core libraries

## [v0.5.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.5.0) - 2015-09-22

- Bumped dependencies, fixed warnings (@garyb)

## [v0.4.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.4.0) - 2015-08-26

- Updated `transformers` dependency. This release requires compiler versions >= 0.7.4.

## [v0.3.1](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.3.1) - 2015-08-07

- Added fork and join operators.

## [v0.3.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.3.0) - 2015-08-07

- Now depends on `purescript-freet`.

## [v0.2.4](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.2.4) - 2015-08-07

- Added instances (@garyb)
- Updated to run consumer effects before producer (@garyb)

## [v0.2.3](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.2.3) - 2015-08-06

- Added `bimapCo` and `interpret` (@garyb)

## [v0.2.2](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.2.2) - 2015-08-02

- Added `producer` and `consumer` functions.

## [v0.2.1](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.2.1) - 2015-07-31

- Made slight API changes.

## [v0.2.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.2.0) - 2015-07-31

- Added more composition operators.

## [v0.1.0](https://github.com/purescript-contrib/purescript-coroutines/releases/tag/v0.1.0) - 2015-07-29

- Initial versioned release.
