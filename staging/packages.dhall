let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260207/packages.dhall
        sha256:487f07aa110e890cbaa14182c7dd84d6b0246e9d70f31ece39ed0e6b6d0a2db7

in  upstream
  with metadata.version = "v0.15.15"
