#
# Makefile for Elpoint.
#

PACKAGE = elpoint

RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l EPT-MK

PREFIX	= NONE
LISP_DIR = NONE
PACKAGE_DIR = NONE
ICON_DIR = NONE

GOMI	= *.elc auto-autoloads.el custom-load.el

elc:
	$(EMACS) $(FLAGS) -f compile-ept \
		$(PREFIX) $(LISP_DIR) $(ICON_DIR)

install-1:	elc
	$(EMACS) $(FLAGS) -f install-ept \
		$(PREFIX) $(LISP_DIR) $(ICON_DIR)

install:	install-1

package:
	$(XEMACS) $(FLAGS) -f compile-ept-package \
		$(PACKAGE_DIR)

install-package-1: package
	$(XEMACS) $(FLAGS) -f install-ept-package $(PACKAGE_DIR)

install-package: install-package-1

clean:
	-$(RM) $(GOMI)
