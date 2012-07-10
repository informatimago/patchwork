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
(objcl:enable-objcl-reader-macros)


(defvar *default-directory* nil)


(defun choose-file-dialog (&key
                           (directory *default-directory*)
                           file-types
                           (button-string "Open")
                           (prompt "Open a file"))
  "
The CHOOSE-FILE-DIALOG function displays the standard Macintosh
SFGetFile dialog box, allowing you to select a file for reading.
Unless the dialog is canceled, this function returns a pathname.

DIRECTORY:      A pathname or string. Specifies the directory shown
                when the dialog box first appears. It defaults to the
                last directory shown by the Choose File dialog box or
                Choose New File dialog box.

FILE-TYPES:     An os-type parameter or list of os-type parameters.  If
                specified, only files with the given Macintosh file type are
                displayed in the dialog box.  Os-types are case sensitive.

BUTTON-STRING:  A string. Specifies the text that appears in the
                button that opens the chosen file. The default is
                Open.

PROMPT:         A string, displayed as title of the choose file dialog.
"
  (declare (ignore button-string))
  (let ((panel [NSOpenPanel openPanel]))
    [panel setCanChooseFiles:t]
    [panel setCanChooseDirectories:nil]
    [panel setResolvesAliases:t]
    [panel setAllowsMultipleSelection:nil]
    ;; ---
    ;; [panel setTitle:(objcl:objcl-string title)]
    ;; [panel setMessage:(objcl:objcl-string message)]
    ;; [panel setNameFieldLabel:(objcl:objcl-string label)]
    [panel setPrompt:(objcl:objcl-string prompt)]
    [panel setCanCreateDirectories:nil]
    [panel setCanSelectHiddenExtension:t]
    [panel setExtensionHidden:nil]
    [panel setTreatsFilePackagesAsDirectories:nil]
    [panel setAllowsOtherFileTypes:t]
    [panel setShowsHiddenFiles:nil]
    (if file-types
        [panel setAllowedFileTypes:(list-to-nsarray (ensure-list file-types))]
        [panel setAllowedFileTypes:nil])
    ;; --
    [panel setDirectoryURL:[NSURL fileURLWithPath:(objcl:objcl-string (namestring directory)) isDirectory:t]]
    [panel setNameFieldStringValue:(objcl:objcl-string "")]
    (when (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (setf *default-directory* (pathname (concatenate 'string (objcl:lisp-string [[panel directoryURL] path]) "/")))
      (let* ((urls  (objc:send panel "URLs"))
             (files '()))
        (dotimes (i [urls count])
          (push (pathname (objcl:lisp-string [[urls objectAtIndex:i] path])) files))
        (if (endp (rest files))
            (first files)
            files)))))



(defun choose-new-file-dialog (&key
                               (directory *default-directory*)
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

BUTTON-STRING:  A string. Specifies the text that appears in the
                button that opens the chosen file. The default is
                Save As.

PROMPT:         A string, displayed as title of the choose new file dialog.
"
  (declare (ignore button-string))
  (let ((panel [NSSavePanel savePanel]))
    ;; [panel setTitle:(objcl:objcl-string title)]
    ;; [panel setMessage:(objcl:objcl-string message)]
    ;; [panel setNameFieldLabel:(objcl:objcl-string label)]
    [panel setPrompt:(objcl:objcl-string prompt)]
    [panel setCanCreateDirectories:t]
    [panel setCanSelectHiddenExtension:t]
    [panel setExtensionHidden:nil]
    [panel setTreatsFilePackagesAsDirectories:nil]
    [panel setAllowsOtherFileTypes:t]
    [panel setShowsHiddenFiles:nil]
    [panel setAllowedFileTypes:nil]
    ;; --
    [panel setDirectoryURL:[NSURL fileURLWithPath:(objcl:objcl-string (namestring directory)) isDirectory:t]]
    [panel setNameFieldStringValue:(objcl:objcl-string "")]
    (when (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (setf *default-directory* (pathname (concatenate 'string (objcl:lisp-string [[panel directoryURL] path]) "/")))
      (pathname (objcl:lisp-string [(objc:send panel "URL") path])))))



(defun choose-directory-dialog (&key
                               (directory *default-directory*)
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
  (let ((panel [NSOpenPanel openPanel]))
    [panel setCanChooseFiles:nil]
    [panel setCanChooseDirectories:t]
    [panel setResolvesAliases:t]
    [panel setAllowsMultipleSelection:nil]
    ;; ---
    ;; [panel setTitle:(objcl:objcl-string title)]
    ;; [panel setMessage:(objcl:objcl-string message)]
    ;; [panel setNameFieldLabel:(objcl:objcl-string label)]
    [panel setPrompt:(objcl:objcl-string prompt)]
    [panel setCanCreateDirectories:t]
    [panel setCanSelectHiddenExtension:t]
    [panel setExtensionHidden:nil]
    [panel setTreatsFilePackagesAsDirectories:nil]
    [panel setAllowsOtherFileTypes:t]
    [panel setShowsHiddenFiles:nil]
    [panel setAllowedFileTypes:nil]
    ;; --
    [panel setDirectoryURL:[NSURL fileURLWithPath:(objcl:objcl-string (namestring directory)) isDirectory:t]]
    [panel setNameFieldStringValue:(objcl:objcl-string "")]
    (when (= [panel runModal] #$NSFileHandlingPanelOKButton)
      (setf *default-directory* (pathname (concatenate 'string (objcl:lisp-string [[panel directoryURL] path]) "/")))
      (let* ((urls  (objc:send panel "URLs"))
             (files '()))
        (dotimes (i [urls count])
          (push (pathname (concatenate 'string (objcl:lisp-string [[urls objectAtIndex:i] path]) "/")) files))
        (if (endp (rest files))
            (first files)
            files)))))


(defun choose-file-default-directory ()
  "
The function CHOOSE-FILE-DEFAULT-DIRECTORY returns the namestring of
the last directory selected by the CHOOSE-FILE-DIALOG,
CHOOSE-NEW-FILE-DIALOG, or CHOOSE-DIRECTORY-DIALOG dialog box.
Initially, this is the directory that is the translation of \"home:\".
"
  *default-directory*)


(defun set-choose-file-default-directory (pathname)
  "
The function SET-CHOOSE-FILE-DEFAULT-DIRECTORY sets the default
directory used by the CHOOSE-FILE-DIALOG, CHOOSE-NEW-FILE-DIALOG, or
CHOOSE-DIRECTORY-DIALOG dialog box to pathname.  It returns pathname.
"
  (setf *default-directory* (pathname pathname)))



(defun initialize/file ()
  (setf *default-directory* (user-homedir-pathname)))

;;;; THE END ;;;;
