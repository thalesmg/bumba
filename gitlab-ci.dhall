let List/map =
      https://prelude.dhall-lang.org/List/map sha256:dd845ffb4568d40327f2a817eb42d1c6138b929ca758d50bc33112ef3c885680

let Stage = < Build | Deploy >

let showStage =
      λ(stage : Stage) → merge { Build = "build", Deploy = "deploy" } stage

let siteAppId = "06890b1e-d940-4c61-b50c-59c6b3344838"

let distDir = "\$CI_PROJECT_DIR/dist/"

let funsDir = "\$CI_PROJECT_DIR/fns/"

let cacheDir = "/nix/store/"

let nixConf =
      ''
      binary-caches = https://cache.nixos.org https://nixcache.reflex-frp.org https://thalesmg.cachix.org
      binary-cache-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= thalesmg.cachix.org-1:1kJkwCK80VMItmlefyjnAvnrTG/X4tnr6lBjA5LTO7E=
      sandbox = false
      ''

let build =
      { stage = showStage Stage.Build
      , image = "nixos/nix:latest"
      , before_script =
        [ "echo \"${nixConf}\" > /etc/nix/nix.conf"
        , "nix-env -iA cachix -f https://cachix.org/api/v1/install"
        , "cachix use thalesmg"
        , "apk add --no-cache git"
        ]
      , script =
        [ "cachix push thalesmg --watch-store &"
        , "nix-build release.nix"
        , "sleep 10"
        , "mkdir -p ${distDir}"
        , "mkdir -p ${funsDir}"
        , "cp -r ./result/* ${distDir}"
        ]
      , artifacts = { paths = [ distDir, funsDir ] }
      , cache = { key = "nix-cache", paths = [ cacheDir ] }
      }

let mkDeployForProd =
        λ(isProd : Bool)
      → let prodOpts = if isProd then "--prod" else "--no-prod"

        in  { stage = showStage Stage.Deploy
            , image = "alpine:latest"
            , dependencies = [ "build" ]
            , before_script =
              [ "apk add --no-cache ruby-dev npm"
              , "(cd ${funsDir} && npm i)"
              , "gem install dpl --pre"
              ]
            , only =
                      if isProd

                then  Some { refs = [ "master" ] }

                else  None { refs : List Text }
            , script =
              [ "ls -la ${funsDir}"
              , "dpl netlify --site ${siteAppId} --auth \"\${NETLIFY_TOKEN}\" --dir ${distDir} --functions ${funsDir} ${prodOpts}"
              ]
            }

in  { stages = List/map Stage Text showStage [ Stage.Build, Stage.Deploy ]
    , build = build
    , `deploy:branch` = mkDeployForProd False
    , `deploy:prod` = mkDeployForProd True
    }
