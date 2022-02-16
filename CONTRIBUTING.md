# Contributing

We love pull requests from everyone. By participating in this project, you
agree to abide by our [code of conduct](CODE_OF_CONDUCT.md), and
[License](LICENSE).

Fork, then clone the repo:

    git clone git@github.com:johnstonskj/rust-asdi.git

Ensure you have a good Rust install, usually managed by [Rustup](https://rustup.rs/).
You can ensure the latest tools with the following:

    rustup update

Make sure the tests pass:

    cargo test --package asdi --no-fail-fast --all-features -- --exact

Make your change. Add tests, and documentation, for your change.

    cargo doc --all-features --no-deps
    cargo fmt
    cargo clippy

If you have made any changes to `Cargo.toml`, also check:

    cargo outdated
    cargo audit

Push to your fork and [submit a pull request](https://github.com/johnstonskj/rust-asdi/compare/).

At this point you're waiting on us. We like to at least comment on pull requests
within three business days (and, typically, one business day). We may suggest
some changes or improvements or alternatives.

Some things that will increase the chance that your pull request is accepted:

* Write tests.
* Write a [good commit message](https://cbea.ms/git-commit/https://cbea.ms/git-commit/).
