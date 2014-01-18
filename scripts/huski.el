;; Basic script for emacs integration

(add-to-list 'load-path "~/.emacs.d/scheme")

(autoload 'scheme-mode "iuscheme" "Major mode for Scheme." t) 
(autoload 'run-scheme "iuscheme" "Switch to interactive Scheme buffer." t) 
(setq auto-mode-alist (cons '("\\.ss" . scheme-mode) auto-mode-alist))

(autoload 'balanced-toggle "balanced" "Toggle balanced ``mode''" t) 
(autoload 'balanced-on "balanced" "Turn on balanced ``mode''" t) 
(add-hook 'scheme-mode-hook 'balanced-on)

(custom-set-variables '(scheme-program-name "huski"))
