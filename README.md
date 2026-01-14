# CCS2026

We vendor in many of the dependencies we need to interact closely with; this notably includes [`cryptovampire`](https://github.com/SecPriv/CryptoVampire) and [`egg`](https://github.com/egraphs-good/egg).

The tools corresponding to the submission are [`indistinguishability`](./crates/indistinguishability/) and [`golgge`](./crates/golgge/).

## Building
### `cargo`
With a version of Rust in accordance with [`rust-toolchain.toml`](./rust-toolchain.toml), one simply needs to run:
```bash
cargo build --release
```
This will create an `indistinguishability` executable in [`target/release`](./target/release).

### `nix`
Simply run:
```bash
nix build
```

## Running

The tool is run with `indistinguishability <arguments>`, `cargo build --release -- <arguments>` or `nix run -- <arguments>`. We will assume the first solution in the rest of this section.
[`vampire`](https://github.com/vprover/vampire) must be available in your PATH.

The entry point to `indistinguishability` is Scheme via [`steel`](https://github.com/mattwparas/steel). `steel` is quite standard-compliant with other Scheme flavors, but some subtle changes might be present.

One can use steel's `repl` mode for an interactive shell or simply run a Scheme file.
To get access to the specific features, one needs to import `(require-builtin cryptovampire)` to get access to the raw bindings or `(require "cryptovampire/v2")` for friendlier but less complete bindings (this imports the [`v2.scm`](./crates/indistinguishability/assets/preludes/v2.scm) file).

Sample files are present in the [`test`](./crates/indistinguishability/tests/) directory.

### Configuring
You are invited to run `indistinguishability --help` and let it guide you through the options.

It is possible to programmatically reconfigure the tool via Scheme. Note that this overwrites the arguments given in the command line.

### Reproducing the table
To reproduce the presented results, simply run `make` in [`./crates/indistinguishability/tests/passing`](./crates/indistinguishability/tests/passing).

You will likely need to change the timeouts set in the files to fit your machine. Because the timeouts are configured in Scheme, you need to modify the files for the change to take effect as Scheme overrides the command line.