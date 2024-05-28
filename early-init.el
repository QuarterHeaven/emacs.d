(setq byte-compile-warnings nil)

(setq-default menu-bar-mode nil
	      tool-bar-mode nil
	      scroll-bar-mode nil
	      tab-bar-mode t)

(if (eq system-type 'darwin)
    (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/13:/usr/local/opt/libgccjit/lib/gcc/13:/usr/local/opt/gcc/lib/gcc/13/gcc/x86_64-apple-darwin21/13"))
