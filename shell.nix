{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (
      pkgs: with pkgs; [
        scotty
        mysql-simple
        aeson
        bytestring
        text
        http-types
        jwt
        bcrypt
      ]
    ))
    pkgs.haskellPackages.cabal-install
    pkgs.mariadb
    pkgs.docker
    pkgs.docker-compose
    pkgs.curl
    pkgs.haskell-language-server
  ];
}
