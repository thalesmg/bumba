let
  reflex-platform = import ./nix/reflex-platform.nix;
  pkgs = reflex-platform.nixpkgs;
  project = import ./default.nix {};
  html = pkgs.writeText "index.html" ''
    <!DOCTYPE html>
    <html>
      <head>
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <script language="javascript" src="public/all.js"></script>
        <style>
          body {
            font-family: sans-serif;
          }
          .gold {
             color: gold;
          }
          .darkest-blue {
            background-color: #181a26;
          }
        </style>
      </head>
      <body class="darkest-blue gold">
      </body>
    </html>
  '';
in pkgs.runCommand "bumba-js" {} ''
  mkdir -p $out
  cp ${html} $out/index.html
  ${pkgs.closurecompiler}/bin/closure-compiler \
    --externs=${project.ghcjs.bumba}/bin/bumba.jsexe/all.js.externs \
    --jscomp_off=checkVars \
    --js_output_file="$out/public/all.js" \
    -O ADVANCED \
    -W QUIET \
    ${project.ghcjs.bumba}/bin/bumba.jsexe/all.js
''
