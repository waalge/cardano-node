let
  basePort              = 30000;
  cacheDirDefault       = "${__getEnv "HOME"}/.cache/cardano-workbench";
  stateDir              = "run/current";
in
{ pkgs
, lib
, cicero
, workbench
##
, cacheDir              ? cacheDirDefault
, extraBackendConfig    ? {}
, useCabalRun           ? false
, enableEKG             ? true
##
, ...
}:
with lib;
let
  backend =
    rec
    { name = "cicero";

      services-config = import ./services-config.nix {inherit lib workbench basePort stateDir useCabalRun enableEKG;};

      materialise-profile =
        { profileNix }:
          pkgs.runCommand "workbench-backend-output-${profileNix.name}-${name}"
            {
              ciceronConf = "cicero.conf";
            }
            ''
            mkdir $out
            touch $out/cicero.conf
            '';
    };
in
{
  inherit cacheDir stateDir basePort;
  inherit workbench;
  inherit backend;
}
