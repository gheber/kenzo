#!/usr/bin/env bash
[[ ! -d ".quicklisp" ]] && { \
echo "Installing quicklisp...";
mkdir -v ./.quicklisp;
curl -O https://beta.quicklisp.org/quicklisp.lisp;
# The installation script is expecting the user to hit the <RETURN> key;
# as well, the @@texinfo:@opt{@@--batch@@texinfo:}@@ expects an EOF marker.
# The use of ~echo~ seems to satisfy both requirements.
echo | sbcl --script ./.devcontainer/basics.lisp;
rm quicklisp.lisp;
mkdir -p /root/.local/share/common-lisp/source;
ln -s /workspaces/kenzo/kenzo.asd /root/.local/share/common-lisp/source/kenzo.asd;
ln -s /workspaces/kenzo/kenzo-test.asd /root/.local/share/common-lisp/source/kenzo-test.asd;
echo "Done installing.";
} || { echo "Quicklisp is already installed."; }
