with (import (builtins.fetchGit {
  name = "ghcide-for-nix";
  url = https://github.com/magthe/ghcide-for-nix;
  rev = "f48111e41c7a2b1b05bf46ddc4b46928d2a3e8d7";
}) );

let
  t = lib.trivial;
  hl = haskell.lib;

  name = "check-status";
  ci-branch = (lib.maybeEnv "SEMAPHORE_GIT_BRANCH" "not a CI build");
  ci-build = (lib.maybeEnv "SEMAPHORE_WORKFLOW_ID" "not a CI build");
  ci-commit = (lib.maybeEnv "SEMAPHORE_GIT_SHA" "not a CI build");

  addEnvVars = drv: drv.overrideAttrs (old: old // {
    TRAVIS_BRANCH = ci-branch;
    TRAVIS_BUILD_NUMBER = ci-build;
    TRAVIS_COMMIT = ci-commit;
  });

  thePkg = haskellPackages.developPackage {
    root = ./.;
    name = name;

    modifier = (t.flip t.pipe)
      [addEnvVars
       hl.dontHaddock
       hl.enableStaticLibraries
       hl.justStaticExecutables
       hl.disableLibraryProfiling
       hl.disableExecutableProfiling];
  };

  theImage = dockerTools.buildImage {
    name = name;
    tag = "latest";
    created = "now";

    contents = [thePkg];

    config = {
      Cmd = ["/bin/check-status"];
      WorkingDir = "/bin";
      Labels = {
        "com.zimpler.branch" = ci-branch;
        "com.zimpler.build_id" = ci-build;
        "com.zimpler.commit" = ci-commit;
        "com.zimpler.system" = name;
      };
    };
  };

in {
  pkg = thePkg;
  image = theImage;
}
