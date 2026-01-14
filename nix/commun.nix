{
  mkPkg = (
    manifestFile:
    {
      lib,
      rustPlatform,
      src ? ./..,
      vampire,
    }:

    let
      manifest = (lib.importTOML manifestFile).package;
      pkg = rustPlatform.buildRustPackage {
        name = manifest.name;
        version = manifest.version;
        cargoLock = {
          lockFile = "${src}/Cargo.lock";

          outputHashes = {
            "steel-core-0.7.0" = "sha256-+QNeyE2vUy8g/8cXPM/LkyTSH9bQAUT5PDvb0I2PraA=";
          };
        };
        src = lib.cleanSource src;
        # patches = [ "${src}/nix.patch" ];
        buildInputs = [ vampire ];
        doCheck = false;
      };
    in
    pkg
  );
}
