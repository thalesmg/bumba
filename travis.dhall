let siteAppId = "06890b1e-d940-4c61-b50c-59c6b3344838"

let On =
      { Type = { branch : Optional Text, all_branches : Optional Bool }
      , default = { branch = None Text, all_branches = None Bool }
      }

let distDir = "./dist/"

in  { language = "nix"
    , before_script =
        ''
          nix-env -iA cachix -f https://cachix.org/api/v1/install
          cachix use thalesmg
        ''
    , script =
        ''
          nix-build release.nix | cachix push thalesmg
          mkdir -p ./dist
          cp -r ./result/* ${distDir}
        ''
    , deploy =
      [ { provider = "netlify"
        , edge = True
        , skip_cleanup = True
        , on = On::{ branch = Some "master" }
        , site = siteAppId
        , prod = True
        , dir = distDir
        }
      , { provider = "netlify"
        , edge = True
        , skip_cleanup = True
        , on = On::{ all_branches = Some True }
        , site = siteAppId
        , prod = False
        , dir = distDir
        }
      ]
    }
