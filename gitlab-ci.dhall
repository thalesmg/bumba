let List/map =
      https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Stage = < Build | Deploy >

let showStage =
      λ(stage : Stage) → merge { Build = "build", Deploy = "deploy" } stage

let siteAppId = "06890b1e-d940-4c61-b50c-59c6b3344838"

let distDir = "./dist/"

let funsDir = "./fns/"

let mkDeployForProd =
        λ(isProd : Bool)
      → let prodOpts = if isProd then "--prod" else "--no-prod"

        in  { stage = showStage Stage.Deploy
            , image = "alpine:latest"
            , before_script =
              [ "apk add --no-cache ruby-dev npm"
              , "(cd fns && npm i)"
              , "gem install dpl --pre"
              ]
            , only =
                      if isProd

                then  Some { refs = [ "master" ] }

                else  None { refs : List Text }
            , script =
              [ "dpl netlify --site ${siteAppId} --auth \"\${NETLIFY_TOKEN}\" --dir ${distDir} --functions ${funsDir} ${prodOpts}"
              ]
            }

in  { stages = List/map Stage Text showStage [ Stage.Build, Stage.Deploy ]
    , build =
        { stage = showStage Stage.Build
        , image = "nixos/nix:latest"
        , before_script =
          [ "nix-env -iA cachix -f https://cachix.org/api/v1/install"
          , "cachix use thalesmg"
          , "apk add --no-cache git"
          , "echo \"binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org\" >> /etc/nix/nix.conf"
          , "echo \"binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=\" >> /etc/nix/nix.conf"
          ]
        , script =
          [ "cachix push thalesmg --watch-store &"
          , "nix-build release.nix"
          , "sleep 10"
          , "mkdir -p ${distDir}"
          , "cp -r ./result/* ${distDir}"
          ]
        }
    , `deploy:branch` = mkDeployForProd False
    , `deploy:prod` = mkDeployForProd True
    }
