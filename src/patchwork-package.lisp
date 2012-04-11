;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               patchwork-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines some Patchwork packages.
;;;;  
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-04-09 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    Proprietary
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    All Rights Reserved.
;;;;    
;;;;    This program and its documentation constitute intellectual property 
;;;;    of Pascal J. Bourguignon and is protected by the copyright laws of 
;;;;    the European Union and other countries.
;;;;**************************************************************************

(cl:defpackage "PW-MACOSX"
  (:nicknames "UI")
  (:use "COMMON-LISP")
  (:export

   "COPY-FILE"

   "*WARN-IF-REDEFINE*"
   "*LISP-CLEANUP-FUNCTIONS*"
   "CATCH-CANCEL" "RLET"
   "WITHOUT-INTERRUPTS" "*EVENTHOOK*" "EVENT-DISPATCH"
   "DEF-LOAD-POINTERS"
   "WPTR" "WITH-PSTRS"

   
   "PROMPT"

   

   "POINT" "POINT-P"
   "POINT-X" "POINT-Y" "POINT-H" "POINT-V"
   "X" "U" "H" "V" 
   "MAKE-POINT" "COPY-POINT"
   "ADD-POINTS" "SUBTRACT-POINTS"
   
   "*BLACK-PATTERN*" "*DKGRAY-PATTERN*" "*GRAY-PATTERN*"
   "*LTGRAY-PATTERN*" "*WHITE-PATTERN*"

   "WITH-CURSOR"
   "*WATCH-CURSOR*" "*I-BEAM-CURSOR*" "*ARROW-CURSOR*"


   "COMMAND-KEY"
   "SET-COMMAND-KEY"


   "MENU-ELEMENT"  "MENU-ELEMENT-P"
   
   "MENU-ITEM" "MENU-ITEM-P" "MENU-ITEM-ENABLE"  "MENU-ITEM-DISABLE"
   "MENU-ITEM-TITLE" "MENU-ITEM-ACTION" "MENU-ITEM-CHECK-MARK"
   "SET-MENU-ITEM-CHECK-MARK"
   "SET-MENU-ITEM-ACTION-FUNCTION"
   "MENU-ITEM-ACTION-FUNCTION" "MENU-ITEM-ACTION"
   
   "MENU" "MENUP" "MENU-ENABLED-P"
   "MENU-TITLE" "MENU-ITEMS"  
   "ADD-MENU-ITEMS"
   "FIND-MENU-ITEM"

   "MENUBAR"  "SET-MENUBAR"
   "FIND-MENU"



   "MOUSE-DOWN-P"
   "DOUBLE-CLICK-P"
   "SHIFT-KEY-P"
   "CONTROL-KEY-P"
   "COMMAND-KEY-P"
   "OPTION-KEY-P"
   
   "COMMAND-KEY"
   
   

   "ENABLED"
   "EVENT-DISPATCH" "FILE-NAME"
   "FONT" "ITEM-ACTION" "ITEM-TEXT" "ITEMS"
   "MAXWIDTH"

   

   "TEXT" "TITLE" "WIDTH"
   
   
   "SIMPLE-VIEW"

   "VIEW" "VIEW-CONTAINER" "VIEW-FONT" "VIEW-POSITION" "VIEW-SIZE"
   "VIEW-WINDOW" "SUBVIEWS" "VIEW-SUBVIEWS"
   "TRACK-THUMB-P" "H-SCROLLP" "V-SCROLLP" "BOTTOM-BOARDER" "CHECK-MARK"
   "SET-VIEW-POSITION" "SET-VIEW-SIZE" "SET-VIEW-FONT"
   "ADD-SUBVIEW" "ADD-SUBVIEWS"
   "REMOVE-SUBVIEW" "REMOVE-SUBVIEWS"

   "WITH-FOCUSED-VIEW" "WITH-FONT-FOCUSED-VIEW"

   "SCROLLER" "SCROLLER-MIXIN" "SCROLL-BAR-CORRECTION" "H-SCROLLER" "V-SCROLLER"

   "CONTROL" 
   "BUTTON-STRING" 

   
   "WINDOW"
   "WINDOW-TITLE" "SET-WINDOW-TITLE" 
   "CLOSE-BOX-P"
   "FRED-WINDOW"
   "WINDOW-SELECT" "BRING-TO-FRONT" "FRONT-WINDOW"
   "WINDOW-SHOW" "WINDOW-HIDE"
   "VIEW-ACTIVATE-EVENT-HANDLER"
   "VIEW-DEACTIVATE-EVENT-HANDLER"


   "DIALOG-ITEM" "DIALOG-ITEM-ACTION" "DIALOG-ITEM-TEXT"   
   "SET-DIALOG-ITEM-TEXT"

   "STATIC-TEXT-DIALOG-ITEM"
   "EDITABLE-TEXT-DIALOG-ITEM"
   "TABLE-DIALOG-ITEM"
   "BUTTON-DIALOG-ITEM"
   "RADIO-BUTTON-DIALOG-ITEM"
   "CHECK-BOX-DIALOG-ITEM"

   "DIALOG"  
   "MESSAGE" "MESSAGE-DIALOG" "CHOOSE-FILE-DIALOG" "CHOOSE-NEW-FILE-DIALOG"
   "Y-OR-N-DIALOG"

   "SELECT-ITEM-FROM-LIST"


   "MAKE-POPUPBOX"


   "FIXNUMP"


   "KEY-EQUAL"
   ))



(cl:defpackage "PATCH-WORK"
  (:nicknames "PW")
  (:use "COMMON-LISP" "LELISP-MACROS" "UI")

  (:export "ENABLE-PATCHWORK-READTABLE"
           "DISABLE-PATCHWORK-READTABLE")
  
  (:export "DRAW-CHAR" "DRAW-STRING" "DRAW-LINE" "DRAW-RECT"
           "FILL-RECT*" "DRAW-POINT" "DRAW-ELLIPSE" "FILL-ELLIPSE"
           "WITH-PEN-STATE" "X" "Y" "W" "H")

  
  (:export "*COLLECTOR-POPUP-MENU*" "*DECOMPILE-CHORDS-MODE*"
           "*TARGET-ACTION-OBJECT*" "-MAKE-LOCK" "ACTIVE-MODE"
           "ADD-ALIAS-TO-PW" "ADD-OUTPUT-TYPE" "ADD-PW-INPUT-TYPE"
           "APPLICATION-OBJECT" "BEGIN-PROCESS" "C-APPLICATION-WINDOW"
           "C-ARRAY-ITEM" "C-CHORD" "C-CHORD-LINE" "C-NUMBOX"
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
           "UPDATE-NOTE" "VALUE" "WRITE-MIDI-NOTE"
           "YOURSELF-IF-COLLECTING"
           
           "SCALE%" "RANDOM2" "MAPCAR-FUN" "CIRLIST" "CUMUL-SUM"
           "INTERPOL" "BREAK-POINT-FUN" "NTH-REMOVE"  "FIRSTN"

           "*COMPILE-DEFINITIONS*"
           ))

(in-package :pw)
(defvar *COMPILE-DEFINITIONS* '())
(in-package :cl-user)



(cl:defpackage "PW-APPLEEVENT"
  (:nicknames "AE")
  (:use "COMMON-LISP")
  (:export
   ))


;; (cl:defpackage "PW-MIDI"
;;   (:nicknames "MIDI")
;;   (:use "CL")
;;   (:export "MIDI-OPEN" "MIDI-WRITE" "MIDI-CLOSE")
;;   (:intern "*PLAYER*" "*PW-REFNUM*" "MIDI-RESET"))


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
