# -*- mode:makefile-gmake;coding:utf-8 -*-
#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           makefile-gmake
#SYSTEM:             POSIX
#USER-INTERFACE:     NONE
#DESCRIPTION
#
#    Makefile for the Patchwork project.
#
#AUTHORS
#    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
#MODIFICATIONS
#    2012-04-12 <PJB> Added this header.
#BUGS
#LEGAL
#    GPL3
#
#    Copyright Pascal J. Bourguignon 2012 - 2012
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#*****************************************************************************


###---------------------------------------------------------------------
### Variables
###---------------------------------------------------------------------

## Configurable:

#CCL_EXE=/data/languages/ccl/bin/ccl
CCL_EXE=ccl-1.11
CCL=$(CCL_EXE) --no-init --batch
CCL_EVAL=--eval

LISP=$(CCL)
LISP_EVAL=$(CCL_EVAL)


## Should not change that much:
ENSCRIPT_OPT= -X latin1 -TA4 -fCourier9 -FCourier-Bold12 -B -h --header="" --margins=:::12
HELP_FORMAT=$(MAKE) %-30s \# %s\n
VARI_FORMAT=%-20s = %s\n


###---------------------------------------------------------------------
### The default target:
###---------------------------------------------------------------------

all::help

help::
	@printf "$(HELP_FORMAT)" "all" "Default target = help"
	@printf "$(HELP_FORMAT)" "help" "Prints this help."

###---------------------------------------------------------------------
### Suffixes and general rules:
###---------------------------------------------------------------------

.SUFFIXES: .ps .pdf

%.ps:%.txt
	enscript $(ENSCRIPT_OPT) -o $@ $<

%.pdf:%.ps
	ps2pdf -sPAPERSIZE=a4 $< $@

#.pdf.zip: ; zip $@ $<


###---------------------------------------------------------------------
### Clean
###---------------------------------------------------------------------

help::
	@printf "$(HELP_FORMAT)" "clean" "Erases the fasl files."
	@printf "$(HELP_FORMAT)" "cleanall" "+ Erases the backup files files."
clean::
	find ~/.cache/common-lisp -type d -name patchwork -prune -print -exec rm -rf {} +
#	@true rm -rf ~/.cache/common-lisp/

cleanall::clean
	find . -name \*~ -exec rm {} \;


###---------------------------------------------------------------------
### Prints the variables
###---------------------------------------------------------------------

help::
	@printf "$(HELP_FORMAT)" "variables" "Prints the variables."
# (progn (insert "variables::\n")
#        (dolist (var '(MAKE MFLAGS DEVIS_NAME DEVIS_PS_FILES DEVIS_PDF_FILES CCL CCL_EVAL
#                       LISP LISP_EVAL ENSCRIPT_OPT HELP_FORMAT VARI_FORMAT)
#                     (terpri))
#          (insert (format "	@printf \"$(VARI_FORMAT)\" %-20s \"$(%s)\"\n" var var))))
variables::
	@printf "$(VARI_FORMAT)" MAKE                 "$(MAKE)"
	@printf "$(VARI_FORMAT)" MFLAGS               "$(MFLAGS)"
	@printf "$(VARI_FORMAT)" CCL                  "$(CCL)"
	@printf "$(VARI_FORMAT)" CCL_EVAL             "$(CCL_EVAL)"
	@printf "$(VARI_FORMAT)" LISP                 "$(LISP)"
	@printf "$(VARI_FORMAT)" LISP_EVAL            "$(LISP_EVAL)"
	@printf "$(VARI_FORMAT)" ENSCRIPT_OPT         "$(ENSCRIPT_OPT)"
	@printf "$(VARI_FORMAT)" HELP_FORMAT          "$(HELP_FORMAT)"
	@printf "$(VARI_FORMAT)" VARI_FORMAT          "$(VARI_FORMAT)"



###---------------------------------------------------------------------
### Generate the PatchWork application.
###---------------------------------------------------------------------


help::
	@printf "$(HELP_FORMAT)" "application" "Generates the application."
application:clean
	printf '(push :save-image-and-quit *features*)\n(handler-bind ((error (lambda (condition) (uiop:print-backtrace) nil))) (load "generate-application.lisp"))\n'|$(CCL_EXE)

# There's a bug in ccl when generating an application from a loaded file…
# $(LISP) \
# 	$(LISP_EVAL) '(load "generate-application.lisp")' \
# 	$(LISP_EVAL) '(ccl:quit)'
# looks ccl 1.8 doesn't quit always after build-application.


###---------------------------------------------------------------------
### Generate a new ccl image.
###---------------------------------------------------------------------


help::
	@printf "$(HELP_FORMAT)" "generate-new-ccl" "Generates a new ccl."
generate-new-ccl:
	$(LISP) \
		$(LISP_EVAL) '(ccl:rebuild-ccl :full t)' \
		$(LISP_EVAL) '(ccl:quit)'

# TODO: add targets to zip and upload the releases
# scp patchwork-10.1-0.791-ccl-1.11_r16635_darwinx8664-darwin-apple-10.12.5-x86-64.tar.bz2 patchwork-10.1-0.791-ccl-1.11_r16635_darwinx8664-darwin-apple-10.12.5-x86-64.zip    pjb@ftp.informatimago.com:/srv/ftp/users/pjb/patchwork/

#### THE END ####
