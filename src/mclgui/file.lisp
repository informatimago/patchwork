;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements the file dialogs.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-18 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "MCLGUI")




(defun choose-file-dialog (&key
                           (directory *default-pathname-defaults*)
                           mac-file-type
                           (button-string "Open file")
                           (prompt "Open a file"))
  "
The CHOOSE-FILE-DIALOG function displays the standard Macintosh
SFGetFile dialog box, allowing you to select a file for reading.
Unless the dialog is canceled, this function returns a pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

MAC-FILE-TYPE:  An os-type parameter or list of os-type parameters.  If
                specified, only files with the given Macintosh file type are
                displayed in the dialog box.  Os-types are case sensitive.

BUTTON-STRING:  A string. Specifies the text that appears in the
                button that opens the chosen file. The default is
                Open.

PROMPT:         A string, displayed as title of the choose file dialog.
"
  (niy choose-file-dialog directory mac-file-type button-string prompt))



(defun choose-new-file-dialog (&key
                               (directory *default-pathname-defaults*)
                               (prompt "Save a new file")
                               (button-string "Save file"))
  "
The CHOOSE-NEW-FILE-dialog function displays the standard Macintosh
SFPutFile dialog box, allowing you to specify a destination file for
writing. An alert dialog box requests confirmation if an existing file
is chosen.  Unless canceled, it returns a pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

MAC-FILE-TYPE:  An os-type parameter or list of os-type parameters.  If
                specified, only files with the given Macintosh file type are
                displayed in the dialog box.  Os-types are case sensitive.

BUTTON-STRING:  A string. Specifies the text that appears in the
                button that opens the chosen file. The default is
                Save As.

PROMPT:         A string, displayed as title of the choose new file dialog.
"
  (niy choose-new-file-dialog directory mac-file-type button-string prompt))



(defun choose-directory-dialog (&key
                               (directory *default-pathname-defaults*)
                               (prompt "Select a directory"))
  "
The function CHOOSE-DIRECTORY-DIALOG displays a variation of the
standard Macintosh SfGetFile dialog box.  Unless canceled, it returns
a directory pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

PROMPT:         A string, displayed as title of the choose directory dialog.
"
  (niy choose-directory-dialog directory prompt))


(defun choose-file-default-directory ()
  "
The function CHOOSE-FILE-DEFAULT-DIRECTORY returns the namestring of
the last directory selected by the CHOOSE-FILE-DIALOG,
CHOOSE-NEW-FILE-DIALOG, or CHOOSE-DIRECTORY-DIALOG dialog box.
Initially, this is the directory that is the translation of \"home:\".
"
  (niy choose-file-default-directory))


(defun set-choose-file-default-directory (pathname)
  "
The function SET-CHOOSE-FILE-DEFAULT-DIRECTORY sets the default
directory used by the CHOOSE-FILE-DIALOG, CHOOSE-NEW-FILE-DIALOG, or
CHOOSE-DIRECTORY-DIALOG dialog box to pathname.  It returns pathname.
"
  (niy set-choose-file-default-directory pathname))



;;;; THE END ;;;;
