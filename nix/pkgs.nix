{
  extras = hackage:
    {
      packages = {
        "aeson-with" = (((hackage.aeson-with)."0.1.1.0").revisions).default;
        "binary-instances" = (((hackage.binary-instances)."1.0.0.1").revisions).default;
        "comonad-extras" = (((hackage.comonad-extras)."4.0.1").revisions).default;
        "hslua" = (((hackage.hslua)."1.1.2").revisions).default;
        "jira-wiki-markup" = (((hackage.jira-wiki-markup)."1.3.2").revisions).default;
        "path-extensions" = (((hackage.path-extensions)."0.1.0.1").revisions).default;
        "pandoc" = (((hackage.pandoc)."2.10").revisions).default;
        "shakebook" = (((hackage.shakebook)."0.8.0.0").revisions).default;
        "shake-plus" = (((hackage.shake-plus)."0.1.10.0").revisions).default;
        "slick" = (((hackage.slick)."1.0.1.1").revisions).default;
        "text-time" = (((hackage.text-time)."0.2.0").revisions).default;
        "zipper-extra" = (((hackage.zipper-extra)."0.1.3.0").revisions).default;
        mediawiki-shell = ./mediawiki-shell.nix;
        };
      };
  resolver = "nightly-2020-06-29";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }