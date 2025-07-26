{
  description = "nix flake for demo";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/armv7a";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-2305";
  inputs.mac2ios.url = "github:zw3rk/mobile-core-tools";
  inputs.hackage = {
    url = "github:input-output-hk/hackage.nix";
    flake = false;
  };
  inputs.haskellNix.inputs.hackage.follows = "hackage";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, haskellNix, nixpkgs, flake-utils, mac2ios, ... }:
    let systems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ]; in
    flake-utils.lib.eachSystem systems (system:
      # this android26 overlay makes the pkgsCross.{aarch64-android,armv7a-android-prebuilt} to set stdVer to 26 (Android 8).
      let android26 = final: prev: {
        pkgsCross = prev.pkgsCross // {
          aarch64-android = import prev.path {
            inherit system;
            inherit (prev) overlays;
            crossSystem = prev.lib.systems.examples.aarch64-android // { sdkVer = "26"; };
          };
          armv7a-android-prebuilt = import prev.path {
            inherit system;
            inherit (prev) overlays;
            crossSystem = prev.lib.systems.examples.armv7a-android-prebuilt // { sdkVer = "26"; };
          };
        };
      }; in
      # `appendOverlays` with a singleton is identical to `extend`.
      let pkgs = haskellNix.legacyPackages.${system}.appendOverlays [android26]; in
      let drv' = { extra-modules, pkgs', ... }: pkgs'.haskell-nix.project {
        compiler-nix-name = "ghc963";
        index-state = "2023-12-12T00:00:00Z";
        # We need this, to specify we want the cabal project.
        # If the stack.yaml was dropped, this would not be necessary.
        projectFileName = "cabal.project";
        src = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "demo";
          src = ./.;
        };
        sha256map = import ./sha256map.nix;
        modules = [
        ({ pkgs, lib, ...}: lib.mkIf (!pkgs.stdenv.hostPlatform.isWindows) {
          # This patch adds `dl` as an extra-library to direct-sqlciper, which is needed
          # on pretty much all unix platforms, but then blows up on windows m(
          packages.direct-sqlcipher.patches = [ ./scripts/nix/direct-sqlcipher-2.3.27.patch ];
        })
        ({ pkgs,lib, ... }: lib.mkIf (pkgs.stdenv.hostPlatform.isAndroid) {
          packages.demo.components.library.ghcOptions = [ "-pie" ];
        })] ++ extra-modules;
      }; in
      # by defualt we don't need to pass extra-modules.
      let drv = pkgs': drv' { extra-modules = []; inherit pkgs'; }; in
      # This will package up all *.a in $out into a pkg.zip that can
      # be downloaded from hydra.
      let withHydraLibPkg = pkg: pkg.overrideAttrs (old: {
        postInstall = ''
          mkdir -p $out/_pkg
          find $out/lib -name "*.a" -exec cp {} $out/_pkg \;
          (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg.zip *)
          rm -fR $out/_pkg
          mkdir -p $out/nix-support
          echo "file binary-dist \"$(echo $out/*.zip)\"" \
              > $out/nix-support/hydra-build-products
        '';
      }); in
      rec {
        packages = {
            "lib:demo" = (drv pkgs).demo.components.library;
        } // ({
            "x86_64-linux" =
              let
                  androidPkgs = pkgs.pkgsCross.aarch64-android;
                  android32Pkgs = pkgs.pkgsCross.armv7a-android-prebuilt;
                  # For some reason building libiconv with nixpgks android setup produces
                  # LANGINFO_CODESET to be found, which is not compatible with android sdk 23;
                  # so we'll patch up iconv to not include that.
                  androidIconv = (androidPkgs.libiconv.override { enableStatic = true; }).overrideAttrs (old: {
                      postConfigure = ''
                      echo "#undef HAVE_LANGINFO_CODESET" >> libcharset/config.h
                      echo "#undef HAVE_LANGINFO_CODESET" >> lib/config.h
                      '';
                  });
                  # Similarly to icovn, for reasons beyond my current knowledge, nixpkgs andorid
                  # toolchain makes configure believe we have MEMFD_CREATE, which we don't in
                  # sdk 23.
                  androidFFI = androidPkgs.libffi.overrideAttrs (old: {
                      dontDisableStatic = true;
                      hardeningDisable = [ "fortify" ];
                  });
                  android32FFI = android32Pkgs.libffi.overrideAttrs (old: {
                      dontDisableStatic = true;
                      hardeningDisable = [ "fortify" ];
                  }
              );in {
              "aarch64-android:lib:demo" = (drv' {
                pkgs' = androidPkgs;
                extra-modules = [{
                  packages.text.flags.simdutf = false;
                }];
              }).demo.components.library.override (p: {
                smallAddressSpace = true;
                # we do not want a dynamically linked object, even though we _do_
                # want to produce a _shared_ object. But `shared` implied -dyanmic
                # with cabal, so we disable and pass `-shared` explicitly.
                enableShared = false;
                # we do want static (e.g. pass all dependencies in, so we get -staticlib)
                enableStatic = true;
                # for android we build a shared library, passing these arguments is a bit tricky, as
                # we want only the threaded rts (HSrts_thr) and ffi to be linked, but not fed into iserv for
                # template haskell cross compilation. Thus we just pass them as linker options (-optl).
                setupBuildFlags = p.component.setupBuildFlags
                # flags to tell GHC we want to produce a -shared object, and we want to also link
                # - the ffi library (ffi)
                ++ map (x: "--ghc-option=${x}") [
                  "-shared" "-o" "libdemo.so"
                  "-threaded"
                  # "-debug"
                  "-optl-lffi"
                  "-optl-llog"
                ]
                # This is fairly idiotic. LLD will strip out foreign exported
                # symbols (a GHC bug? Codegen bug?). So we need to pass `-u <sym>`
                # to ensure they stay in the produced library. Having them
                # _undefined_ and _lazy_ (lld will tell with -y <sym> that the
                # symbol is lazy), makes them _defined_. m(
                ++ map (sym: "--ghc-option=-optl-Wl,-u,${sym}") [
                  "demo_start"
                ];
                postInstall = ''
                  set -x
                  ${pkgs.tree}/bin/tree $out
                  mkdir -p $out/_pkg
                  # copy over includes, we might want those, but maybe not.
                  # cp -r $out/lib/*/*/include $out/_pkg/
                  # find the libHS...ghc-X.Y.Z.a static library; this is the
                  # rolled up one with all dependencies included.
                  cp libdemo.so $out/_pkg
                  # find ./dist -name "lib*.so" -exec cp {} $out/_pkg \;
                  # find ./dist -name "libHS*-ghc*.a" -exec cp {} $out/_pkg \;
                  # find ${androidFFI}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${androidPkgs.gmp6.override { withStatic = true; }}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${androidIconv}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  # find ${androidPkgs.stdenv.cc.libc}/lib -name "*.a" -exec cp {} $out/_pkg \;
                  echo ${androidPkgs.openssl}
                  find ${androidPkgs.openssl.out}/lib -name "*.so" -exec cp {} $out/_pkg \;

                  # remove the .1 and other version suffixes from .so's. Androids linker
                  # doesn't play nice with them.
                  for lib in $out/_pkg/*.so; do
                    for dep in $(${pkgs.patchelf}/bin/patchelf --print-needed "$lib"); do
                      if [[ "''${dep##*.so}" ]]; then
                        echo "$lib : $dep -> ''${dep%%.so*}.so"
                        chmod +w "$lib"
                        ${pkgs.patchelf}/bin/patchelf --replace-needed "$dep" "''${dep%%.so*}.so" "$lib"
                      fi
                    done
                  done

                  for lib in $out/_pkg/*.so; do
                    chmod +w "$lib"
                    ${pkgs.patchelf}/bin/patchelf --remove-needed libunwind.so "$lib"
                    [[ "$lib" != *libdemo.so ]] && ${pkgs.patchelf}/bin/patchelf --set-soname "$(basename -a $lib)" "$lib"
                  done

                  ${pkgs.tree}/bin/tree $out/_pkg
                  (cd $out/_pkg; ${pkgs.zip}/bin/zip -r -9 $out/pkg-aarch64-android-libdemo.zip *)
                  rm -fR $out/_pkg
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$(echo $out/*.zip)\"" \
                      > $out/nix-support/hydra-build-products
                '';
              });
            };
        }.${system} or {});
        # build all packages in hydra.
        #hydraJobs = packages;

        devShell = let
	updateCmd = pkgs.writeShellApplication {
          name = "update-sha256map";
          runtimeInputs = [ pkgs.nix-prefetch-git pkgs.jq pkgs.gawk ];
          text = ''
            gawk -f ./scripts/nix/update-sha256.awk cabal.project > ./scripts/nix/sha256map.nix
          '';
        }; in
	pkgs.mkShell {
          buildInputs = [ updateCmd ];
          shellHook = ''
            echo "welcome to the shell!"
          '';
        };
      }
    );
}
