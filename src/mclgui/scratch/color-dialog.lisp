;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               color-dialog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Color Dialog.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-03-24 <PJB> Extracted from color.
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



(defun user-pick-color (&key (color *black-color*) (prompt "Pick a color") position)
  "
The USER-PICK-COLOR function displays the standard Macintosh Color
Picker at POSITION, set to color COLOR, with prompt PROMPT.  It
returns the selected color if the user clicks OK or throws to the tag
:cancel if the user clicks Cancel.


COLOR:          The default color to bring up in the dialog box. The
                default is *BLACK-COLOR*.

PROMPT:         The prompt to display in the dialog box. The default
                is \"Pick a color\".

POSITION:       The position of the Color Picker on screen. The
                default is calculated by Macintosh Common Lisp.

"
  (declare (ignore prompt position))
  (let ((panel [NSColorPanel sharedColorPanel]))
    [panel setShowsAlpha:YES]
    [panel setMode:#$NSWheelModeColorPanel]
    [panel setColor:[NSColor colorWithCalibratedRed: (color-red color)
                             green: (color-green color)
                             blue: (color-blue color)
                             alpha: (color-alpha color)]]
    [panel makeKeyAndOrderFront:panel]
    (when (= [[NSApplication sharedApplication]runModalForWindow:panel]
             #$NSRunAbortedResponse)
      (throw-cancel))
    (wrap [panel color])))


;;;; THE END ;;;;
