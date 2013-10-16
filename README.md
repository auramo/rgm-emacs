rgm-emacs
=========

Emacs mode for credential management

Add this to your init file (init.el or .emacs)

    (require 'rgm)

Then you can start the mode with M-x rgm-mode. It asks for master password etc. You can get help by pressing h.

If you want to add a command for starting the mode, do something like this:

    (global-set-key (kbd "C-c g") (lambda () (interactive) (rgm-mode)))
