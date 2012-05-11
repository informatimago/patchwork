;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The MCLGUI package implements the Mac OS GUI classes of MCL
;;;;    over the OpenStep API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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

(defpackage "MCLGUI"
  (:export

   ;; Extensions:
   
   "INITIALIZE"

   "*TEXT-MODES*"

   ;; Conditions:
   "UNKNOWN-TRANSFER-MODE" "UNKNOWN-TRANSFER-VALUE"
   "INVALID-FONT-SPEC-ERROR" "INVALID-FONT-SPEC"
   "INVALID-FONT-SPEC-REASON" "INVALID-FONT-SPEC-OPTION"

   
   
   ;; Chapter 2: Points and Fonts

   "MAKE-POINT" "POINT-STRING" "POINT-H" "POINT-V" "POINT<="
   "ADD-POINTS" "SUBTRACT-POINTS" "POINT-TO-LIST"

   "*FONT-LIST*" "*PEN-MODES*" "*STYLE-ALIST*"

   "*BLACK-PATTERN*" "*DKGRAY-PATTERN*" "*GRAY-PATTERN*"
   "*LTGRAY-PATTERN*" "*WHITE-PATTERN*"

   "*SCREEN-WIDTH*" "*SCREEN-HEIGHT*"
   "*PIXELS-PER-INCH-X*" "*PIXELS-PER-INCH-Y*"

   "REAL-FONT" "FONT-SPEC" "STRING-WIDTH" "FONT-INFO"
   "FONT-CODES" "FONT-CODES-INFO" "FONT-CODES-LINE-HEIGHT"
   "FONT-CODES-STRING-WIDTH" "FONT-LINE-HEIGHT"
   "MERGE-FONT-CODES"
   
   "VIEW-FONT" "SET-VIEW-FONT"
   "VIEW-FONT-CODES" "SET-VIEW-FONT-CODES"

   ;; Not implemented:
   ;; grafport-write-string
   
   ;; Chapter 3: Menus

   "MENUBAR" "*MENUBAR*" "*MENUBAR-FROZEN*" "*APPLE-MENU*"
   "*FILE-MENU*" "*EDIT-MENU*" "*LISP-MENU*" "*TOOL-MENU*"
   "*WINDOW-MENU*"  "SET-MENUBAR" "FIND-MENU" "DRAW-MENUBAR-IF"

   "MENU-ELEMENT" "MENU-ITEM-OWNER" "MENU-OWNER" "MENU-ITEM-TITLE"
   "MENU-TITLE" "MENU-ITEM-ENABLED-P" "MENU-ENABLED-P"
   "MENU-ITEM-STYLE" "MENU-STYLE" "MENU-UPDATE-FUNCTION"
   "MENU-ITEM-UPDATE-FUNCTION" "HELP-SPEC"

   "MENU" "MENU-ITEMS" "MENU-FONT" "SET-MENU-TITLE" "MENU-INSTALL"
   "MENU-DEINSTALL" "MENU-INSTALLED-P" "MENU-ENABLE" "MENU-DISABLE"
   "MENU-UPDATE"


   "ADD-MENU-ITEMS" "REMOVE-MENU-ITEMS" "FIND-MENU-ITEM"
   "COMMAND-KEY"

   "MENU-ALREADY-INSTALLED" "MENU-ALREADY-INSTALLED-ERROR" "MENU-ITEM"
   "MENU-ITEM-ACTION" "MENU-ITEM-ACTION-FUNCTION"
   
   "*BALLOT-BOX-WITH-CHECK*" "*CHECK-MARK*" "*HEAVY-CHECK-MARK*"
   "*NOT-CHECK-MARK*"

   "MENU-ITEM-CHECK-MARK" "MENU-ITEM-DISABLE" "MENU-ITEM-ENABLE"
   "MENU-ITEM-ICON-HANDLE" "MENU-ITEM-ICON-HANDLE-SLOT"
   "MENU-ITEM-ICON-NUM" "MENU-ITEM-ICON-NUM-SLOT" "MENU-ITEM-ICON-TYPE"
   "MENU-ITEM-ICON-TYPE-SLOT" "MENU-ITEM-NOT-OWNED"
   "MENU-ITEM-NOT-OWNED-ERROR" "MENU-ITEM-NOT-OWNED-ERROR-MENU"
   "MENU-ITEM-SCRIPT" "MENU-ITEM-SCRIPT-SLOT" "MENU-ITEM-UPDATE"

   "SET-COMMAND-KEY" "SET-MENU-ITEM-ACTION-FUNCTION"
   "SET-MENU-ITEM-CHECK-MARK" "SET-MENU-ITEM-ENABLED-P"
   "SET-MENU-ITEM-STYLE" "SET-MENU-ITEM-TITLE"
   "SET-MENU-ITEM-UPDATE-FUNCTION"

   
   "WITH-MENU-DETACHED"

   ;; Not implemented:
   ;; menu-handle menu-id *menu-id-object-alist*
   
   ;; Extensions:

   "MENU-ALREADY-INSTALLED-ERROR" "MENU-ALREADY-INSTALLED"
   "MENU-ITEM-NOT-OWNED-ERROR" "MENU-ITEM-NOT-OWNED-ERROR-MENU"
   "MENU-ITEM-NOT-OWNED"
   
   
   ;; Chapter 4: Views and Windows

   ;; Chapter 5: Dialog Items and Dialogs

   ;; Chapter 6: Color

   "PART-COLOR" "SET-PART-COLOR"

   ;;  Extensions:
   "COLOR-PARTS"

   
   ;; Chapter 10: Events

   ;; Chapter 11: Apple Events

   )
  (:documentation "

The MCLGUI package implements the Mac OS GUI classes of MCL over the
OpenStep API.

Call (mclgui:initialize) before using any other function or variable
exported from this package.


The API is fully documented in:

http://code.google.com/p/mcl/source/browse/Macintosh+Common+Lisp+Ref.pdf

Chapter 2: Points and Fonts
Chapter 3: Menus
Chapter 4: Views and Windows
Chapter 5: Dialog Items and Dialogs
Chapter 6: Color
Chapter 10: Events
\(Chapter 11: Apple Events -- perhaps).

LEGAL:

    GPL3

    Copyright Pascal J. Bourguignon 2012 - 2012

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

"))

;;;; THE END ;;;;
