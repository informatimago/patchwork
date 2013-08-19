;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               patchwork-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    Defines some Patchwork packages.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright IRCAM 1986 - 2012
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


(cl:defpackage "PATCH-WORK.SCHEDULER"
  (:use "COMMON-LISP" "CLOSER-MOP" "MCLGUI" "LELISP-MACROS" "MIDI")

  (:shadowing-import-from "CLOSER-MOP"
                          "STANDARD-CLASS" "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD"
                          "DEFMETHOD" "DEFGENERIC")
  
  (:import-from "UI"
                "WITHOUT-INTERRUPTS" "*EVENTHOOK*" "EVENT-DISPATCH")
  ;; TODO:  (:import-from "MIDI" "CLOCK-TIME" "MIDI-WRITE" "MIDI-WRITE-TIME")
  (:export "START" "*ERROR-WHEN-EXTRA-START?*" "DFUNCALL" "APDFUNCALL"
           "RE-DFUNCALL" "ADVANCE" "PRIORITY" "WITH-MORE-PRIORITY"
           "WITH-LESS-PRIORITY" "WITH" "*HIGHEST-PRIORITY*" 
           "EVENT-DISPATCH" "SCHEDULER-STATE" "SET-SCHEDULER-STATE"
           "*SCHEDULER-INITIAL-STATE*" "WITH-SCHEDULER-OOT1" "SCHEDULER-STEP"
           "PRINT-SCHEDULER-QUEUE" "ABORT-TASK" "RESET-SCHEDULER"
           "*HIGHEST-LATENCY*" "*LATE-TASK*" "*PRINT-ON-LATE?*"
           "*STEP-ON-LATE?*" "*RESET-ON-LATE?*" "*EVAL-ON-LATE?*"
           "*ERROR-TASK*" "*CONDITION*" "*PRINT-ON-ERROR?*" "*STEP-ON-ERROR?*"
           "*RESET-ON-ERROR?*"))


(cl:defpackage "PATCH-WORK"
  (:nicknames "PW")
  (:use "COMMON-LISP-STEPPER" "LELISP-MACROS" "UI" "PATCH-WORK.SCHEDULER")

  (:export "ENABLE-PATCHWORK-READER-MACROS"
           "DISABLE-PATCHWORK-READER-MACROS")
  
  (:export "DRAW-CHAR" "DRAW-STRING" "DRAW-LINE" "DRAW-RECT"
           "FILL-RECT*" "DRAW-POINT" "DRAW-ELLIPSE" "FILL-ELLIPSE"
           "WITH-PEN-STATE" "X" "Y" "W" "H")

  (:export "*COLLECTOR-POPUP-MENU*" "*DECOMPILE-CHORDS-MODE*"
           "*TARGET-ACTION-OBJECT*" "-MAKE-LOCK" "ACTIVE-MODE"
           "ADD-ALIAS-TO-PW" "ADD-OUTPUT-TYPE" "ADD-PW-INPUT-TYPE"
           "APPLICATION-OBJECT" "BEGIN-PROCESS" "C-APPLICATION-WINDOW"
           "OPEN-APPLICATION-HELP-WINDOW"
           "C-ARRAY-ITEM" "C-CHORD" "C-CHORD-LINE" "C-NUMBOX"
           "C-NOTE"
           "C-PATCH" "C-PATCH-APPLICATION" "C-PATCH-MIDI-MOD"
           "C-PW-FUNCTIONAL" "C-PW-RESIZE-X" "C-PW-TYPE"
           "C-RADIO-BUTTON" "C-TTYBOX" "CHBUILD" "CHORD-LINE"
           "CHORD-SEQ" "CHORDS" "CLASS-SLOT-NAMES" "COLLECT"
           "COMPILE-ME" "COMPLETE-BOX" "CONST" "CORRECT-EXTENSION-BOX"
           "DECOMPILE" "DEFMETHODP" "DEFUNP" "DEFUNT"
           "DRAW-FUNCTION-NAME" "DRAW-PATCH-EXTRA" "EVCONST" "EVAL-IF-KEY"
           "GET-PW-TYPE-SPECS" 
           "INPUT-OBJECTS" "KEY-PRESSED-EXTRA" "LIST!" "LOCK"
           "MAKE-APPLICATION-OBJECT" "MAKE-INSTRUMENT-FOR-CHORD"
           "MAKE-POPUPBOX" "MAKE-PW-STANDARD-BOX" "MIDI-WRITE"
           "MY-ARRAY" "NEW-LEAFMENU" "NEW-MENU" "NOTES"
           "OPEN-PATCH-WIN" "OPEN-PW-CONTROLS-DIALOG" "OUT-PUT"
           "PATCH-VALUE" "PW-ADDMENU" "PW-CONTROLS" "PW-FUNCTION"
           "PW-FUNCTION-STRING" "PW-OBJECT" "REMOVE-YOURSELF-CONTROL"
           "RESIZE-PATCH-BOX" "SAVE" "SAVED-SELECTED" "SBOX" "SELF"
           "SET-BOX-TITLE" "SET-DIALOG-ITEM-TEXT-FROM-DIALOG"
           "SET-PW-WIN+PW-OBJ" "T-TIME" "TYPE-LIST" "UPDATE-EDITOR"
           "UPDATE-NOTE" "VALUE"  "STOP-PLAY"
           "YOURSELF-IF-COLLECTING"
           
           "SCALE%" "RANDOM2" "MAPCAR-FUN" "CIRLIST" "CUMUL-SUM"
           "INTERPOL" "BREAK-POINT-FUN" "NTH-REMOVE"  "FIRSTN")

  (:export "WRITE-MIDI-NOTE" "WRITE-MIDICENT-NOTE"
           "UPDATE-CENTS-VECTOR" "WRITE-MIDICENT-NOTE"
           "WRITE-PITCH-BEND-VALUE" "WRITE-CONTROLLER-VALUE"
           "WRITE-PROGRAM-CHANGE-VALUE"  "WRITE-PROGRAM-CHANGE-VALUE"
           "MIDI-ONLY-ATTACKS-PITCHES" "QUANTIZE-CHORDS"
           "READ-FROM-MIDI")

  ;; Quickdraw:
  (:export "WITH-RECTANGLE-ARG"  "ORIGIN" "SET-ORIGIN" "CLIP-REGION"
           "SET-CLIP-REGION" "CLIP-RECT" "FRAME-RECT" "PAINT-RECT"
           "ERASE-RECT" "INVERT-RECT" "FILL-RECT" "FRAME-OVAL"
           "PAINT-OVAL" "ERASE-OVAL" "INVERT-OVAL" "FILL-OVAL"
           "FRAME-ROUND-RECT" "PAINT-ROUND-RECT" "ERASE-ROUND-RECT"
           "INVERT-ROUND-RECT" "FILL-ROUND-RECT" "FRAME-ARC"
           "PAINT-ARC" "ERASE-ARC" "INVERT-ARC" "FILL-ARC"
           "FRAME-REGION" "PAINT-REGION" "ERASE-REGION"
           "INVERT-REGION" "FILL-REGION" "START-PICTURE" "GET-PICTURE"
           "DRAW-PICTURE" "KILL-PICTURE" "START-POLYGON" "GET-POLYGON"
           "KILL-POLYGON" "OFFSET-POLYGON" "FRAME-POLYGON"
           "PAINT-POLYGON" "ERASE-POLYGON" "INVERT-POLYGON"
           "FILL-POLYGON" "LOCAL-TO-GLOBAL" "GLOBAL-TO-LOCAL"
           "GET-PIXEL" "SCALE-POINT" "MAP-POINT" "MAP-RECT"
           "MAP-REGION" "MAP-POLYGON" "MAKE-BITMAP" "COPY-BITS"
           "SCROLL-RECT")

  ;; For EPW:
  (:export "NEW-MENU" "PW-ADDMENU"))

(cl:defpackage "C-PATCH-BUFFER"
  (:use "COMMON-LISP-STEPPER" "UI" "PATCH-WORK")
  (:export "C-PATCH-BUFFER" "THE-BUFFER" "BUFFER" "C-RADIO-BUTTON"
           "GET-LOCK-BUTTON-FUN" "VALUE"))

(cl:defpackage "C-PATCH-ACCUM"
  (:use "COMMON-LISP-STEPPER" "UI" "PATCH-WORK"  "C-PATCH-BUFFER")
  (:export "C-PATCH-ACCUM" "THE-BUFFER" "ACCUM" "*ACCUM-BUFFER-LIMIT*"))

(cl:defpackage "C-PATCH-FILE-BUFFER"
  (:use "COMMON-LISP-STEPPER" "LELISP-MACROS" "UI" "PATCH-WORK")
  (:intern "ASCII-WIN" "C-PATCH-ASCII-BUFFER")
  (:export "C-PATCH-FILE-BUFFER"))

;; A few circular dependencies:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(C-patch-buffer:C-patch-buffer
            C-patch-accum:C-patch-accum
            C-patch-file-buffer:C-patch-file-buffer
            C-patch-file-buffer::Ascii-win
            C-patch-file-buffer::C-patch-ascii-buffer)
          "PATCH-WORK"))


;; (cl:defpackage "PW-MIDI"
;;   (:nicknames "MIDI")
;;   (:use "CL")
;;   (:export "MIDI-OPEN" "MIDI-WRITE" "MIDI-CLOSE")
;;   (:intern "*PLAYER*" "*PW-REFNUM*" "MIDI-RESET"))

#-(and)
(cl:defpackage "FFI"
  (:export "kAENoReply" "AEGetKeyDesc" "kCurrentProcess" "EraseRgn"
           "AEDeleteParam" "DisposHandle" "AEGetParamDesc"
           "typeInteger" "GetHandleSize" "keyAddressAttr" "PrPicFile"
           "kAECanSwitchLayer" "insertmenu" "AESizeOfNthItem"
           "typeProcessSerialNumber" "keyEventClassAttr" "EraseRect"
           "NewRgn" "kAEPrintDocuments" "userCanceledErr" "PrOpen"
           "PrClosePage" "InsetRgn" "typeAlias" "PrOpenPage"
           "kAEWaitReply" "keyErrorNumber" "typechar"
           "keyDirectObject" "frameRect" "kAEQuitApplication"
           "AEGetNthDesc" "NewAliasMinimalFromFullPath" "SetRectRgn"
           "popUpMenuSelect" "PrStlDialog" "AEPutAttributeDesc"
           "PrCloseDoc" "typeChar" "AEGetAttributeDesc" "FrameRect"
           "kAEOpenDocuments" "AEPutDesc" "PrJobDialog" "PrOpenDoc"
           "AEPutKeyDesc" "PaintRect" "kAECanInteract"
           "kAEOpenApplication" "kAutoGenerateReturnID"
           "kAEDefaultTimeout" "ppcbrowser" "kAEDontReconnect"
           "DrawChar" "noerr" "AEDeleteItem" "OpenResFile" "InvalRgn"
           "keyEventIDAttr" "kAEQueueReply" "AESizeOfAttribute"
           "keyReturnIDAttr" "setfrontprocess" "PenPat"
           "AEDeleteKeyDesc" "kAEAlwaysInteract" "DiffRgn" "StuffHex"
           "AEPutPtr" "AECountItems" "AESend" "keyErrorString"
           "CmpString" "keyTransactionIDAttr" "PaintOval" "TypeFloat"
           "kAnyTransactionID" "SectRect" "LineTo" "NewPtr"
           "SetPenState" "AECreateAppleEvent" "AECreateList"
           "AEDisposeDesc" "FrameOval" "EmptyRect" "kCoreEventClass"
           "kNoProcess" "kAENeverInteract" "AECreateDesc" "PrClose"
           "GetPenState" "nreturnreceipt" "GetProcessInformation"
           "NoErr" "kAENormalPriority" "AEDuplicateDesc"
           "getcurrentprocess" "AEPutParamDesc" "getnamedresource"
           "errAEWaitCanceled" "MoveTo" "AESizeOfParam" "PtrToHand"
           "pt2rect" "typeTargetID" "GetNextProcess" "AECoerceDesc"
           "typeWildCard" "PenMode"))


;;;; THE END ;;;;
