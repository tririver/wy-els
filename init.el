(add-to-list 'load-path "~/.emacs.d/")
(setq package-enable-at-startup nil)
(package-initialize)

;; ============= my packages ================

(require 'wy-utils)
(require 'wy-settings)
(require 'wy-keys)
(require 'wy-org)
(require 'wy-appearance)

(require 'wy-tex)
(require 'wy-txt)

(require 'ob-mathematica)
