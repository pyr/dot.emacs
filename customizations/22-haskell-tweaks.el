(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(setq ghc-ghc-options '("-XDeriveGeneric"))
(setq haskell-program-name "cabal repl ")
