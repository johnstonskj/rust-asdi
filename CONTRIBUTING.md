# Contributing

You can contribute to [ASDI](https://github.com/johnstonskj/rust-asdi) in a number of ways,

1. file [bugs](https://github.com/johnstonskj/rust-asdi/issues/new?assignees=&labels=bug&template=bug_report.md&title=) and
   [enhancement requests](https://github.com/johnstonskj/rust-asdi/issues/new?assignees=&labels=enhancement&template=feature_request.md&title=)
2. review the [documentation](https://docs.rs/asdi) and the [book](https://simonkjohnston.life/rust-asdi/) and let us
   know if you find are issues there
3. Fix or Add something and send us a pull request.

We love pull requests from everyone. By participating in this project, you
agree to abide by our [code of conduct](https://github.com/johnstonskj/rust-asdi/CODE_OF_CONDUCT.md), and
[License](https://github.com/johnstonskj/rust-asdi/LICENSE).

Fork, then clone the repo:

    git clone git@github.com:johnstonskj/rust-asdi.git

Ensure you have a good Rust install, usually managed by [Rustup](https://rustup.rs/).
You can ensure the latest tools with the following:

    rustup update

Make sure the tests pass:

    cargo test --package asdi --no-fail-fast --all-features -- --exact

Make your change. Add tests, and documentation, for your change. Ensure not only that tests pass, but the following all run successfully.

    cargo doc --all-features --no-deps
    cargo fmt
    cargo clippy

If you made changes to the book source, ensure the following runs successfully

    mdbook build

If you have made any changes to `Cargo.toml`, also check:

    cargo outdated
    cargo audit

Push to your fork and [submit a pull request](https://github.com/johnstonskj/rust-asdi/compare/).

At this point you're waiting on us. We like to at least comment on pull requests
within three business days (and, typically, one business day). We may suggest
some changes or improvements or alternatives.

Some things that will increase the chance that your pull request is accepted:

* Write tests.
* Write API documentation.
* Write a [good commit message](https://cbea.ms/git-commit/https://cbea.ms/git-commit/).
