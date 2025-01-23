{ specFile ? ./docs-0.24.6.json }:
{ gitea-api = import ./openapi-hs-generator {
    pkgs = import ./nixpkgs {};
    inherit specFile;
    packageName = "vikunja-api";
    baseModule = "Vikunja";
    versionFn = x: x + ".0";
    cabalFileReplacements = {
      "Author Name Here" = "Obsidian Systems LLC";
      "author.name@email.com" = "maintainer@obsidian.systems";
      "YEAR - AUTHOR" = "2024 Obsidian Systems LLC";
      "UnspecifiedLicense" = "BSD3";
      "extra-source-files:" = "extra-source-files:\n    ChangeLog.md";
      "cabal-version:  >= 1.10" = ''
        cabal-version:  >= 1.10
        tested-with: GHC ==8.10.7
        license-file: LICENSE
      '';
      # Package version changes:
      "http-media >= 0.4 && < 0.8" = "http-media >= 0.4 && < 0.9";
      "http-client >=0.5 && <0.6" = "http-client >=0.5 && <0.7";
    };
    cabalFileAppendix = ''
      source-repository head
        type: git
        location: https://github.com/obsidiansystems/vikunja-api
    '';
  };
}
