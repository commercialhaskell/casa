{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ postgresql zlib ];
  PORT = 3001;
  AUTHORIZED_PORT=3002;
  DBCONN="devel.db";
}
