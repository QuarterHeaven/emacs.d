;; -*- lexical-binding: t -*-
(setq byte-compile-warnings nil)

(defun set-darwin-env ()
  ;; (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/13:/usr/local/opt/libgccjit/lib/gcc/13:/usr/local/opt/gcc/lib/gcc/13/gcc/x86_64-apple-darwin21/13")
  (setenv "NIX_PATH" "nixpkgs=/Users/takaobsid/.nix-defexpr/channels/nixpkgs"))

(if (eq system-type 'darwin) (set-darwin-env))
