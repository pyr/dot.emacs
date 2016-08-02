;;
;; inline notification of python lines violating PEP8
;;
(require 'flymake-python-pyflakes)

(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(setq flymake-python-pyflakes-executable "flake8")
