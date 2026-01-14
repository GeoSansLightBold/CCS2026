# CCS2026

We vendor in many of the dependancies we need to interact closesly with, this notably includes [`cryptovampire`](https://github.com/SecPriv/CryptoVampire) and [`egg`](https://github.com/egraphs-good/egg).

The tool corresponding to the submission are [`indistinguishability`](./crates/indistinguishability/) and [`golgge`](./crates/golgge/).

## Building
### `cargo`
With a version of rust in accordance with [`rust-toolchain.toml`](./rust-toolchain.toml) one simply need to do
```bash
cargo build --release
```
This will make an `indistinguishability` executable in [`target/release`](./target/release).

### `nix`
Simply 
```bash
nix build
```

## Running

The tool is ran with `indistinguishability <arguments>`, `cargo build --release -- <arguments>` or `nix run -- <arguments>`. We will assume the first solution in the rest of this section.
[`vampire`](https://github.com/vprover/vampire) must be available in your path.

The entrypoint to `indistinguishability` is scheme via [`steel`](https://github.com/mattwparas/steel). `steel` is quite standard complient with other scheme flavours, but some subtle changes might be present.

One can use steel's `repl` mode for an interactive shell or simply run a scheme file.
To get access to the specific features, one need to import `(require-builtin cryptovampire)` to get access to the raw bindings or `(require "cryptovampire/v2")` for friendlier but less complete bindings. (this import the [`v2.scm`](./crates/indistinguishability/assets/preludes/v2.scm) file)

sample files are present in the [`test`](./crates/indistinguishability/tests/) directory.

### Configuring
You are invited to run `indistinguishability --help` and let it guide you through the options.

It is possible to programatically reconfigure the tool via scheme. Note that this overwites the arguments given in the command line.

### Reproducing the table
To reproduce the presented result simpy run `make` in [`./crates/indistinguishability/tests/passing`](./crates/indistinguishability/tests/passing).

You will likely need to change the timeouts set in the files to fit your machine. Because the timeout are configured in scheme, you need to modify the files for the change to take effect as scheme overides the command line.