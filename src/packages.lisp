;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               package.lisp
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
;;;;    Copyright IRCAM 1986 - 2014
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


(cl:defpackage "CLPF-UTIL"
  (:use "COMMON-LISP" "LELISP-MACROS")
  (:export
   "SYNONYM" "VECTOR-TO-LIST" "COMPILE-FILE?"
   "FILE-COMPARE…" "FILE-COMPARE" "READ-LISTS-FROM"
   "PREFIX-EXPR" "PREFIX-HELP" "*COMPILE-NUM-LAMBDA*" "MAKE-NUM-FUN" "MAKE-NUM-LAMBDA"))


(cl:defpackage "REDIRECTING-STREAM"
  (:use "COMMON-LISP"
        "TRIVIAL-GRAY-STREAMS")
  (:export "REDIRECTING-CHARACTER-OUTPUT-STREAM"
           "REDIRECTING-CHARACTER-INPUT-STREAM"))


(cl:defpackage "PATCHWORK.MIDI"

  (:use "COMMON-LISP"
        #+patchwork.builder::use-cl-midi "UI"
        #+patchwork.builder::use-cl-midi "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        #+patchwork.builder::use-cl-midi "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        #+patchwork.builder::use-midishare "MIDISHARE")

  #-patchwork.builder::use-midishare (:import-from "UI" "DELETEF")
  
  (:export "MIDI-OPEN" "MIDI-CLOSE" "MIDI-WRITE" "MIDI-WRITE-TIME"
           "MIDI-WRITE-NOW" "MIDI-READ" "MIDI-CLEAR"
           "CLOCK-TIME" "MIDI-GET-TIME" "MIDI-RESET"
           "MIDI-RECORD" "MIDI-LOAD" "MIDI-SAVE"
           
           "*PW-REFNUM*" "*PLAYER*"
           "WITH-TEMPORARY-MIDI-FILE-INFOS"
           "WITH-TEMPORARY-PLAYER-STATE"
           
           ;; stuff from midishare:
           "TYPENOTE" "TYPEKEYON" "TYPEKEYOFF" "TYPEKEYPRESS" 
           "TYPECTRLCHANGE" "TYPEPROGCHANGE" "TYPECHANPRESS" "TYPEPITCHWHEEL" 
           "TYPEPITCHBEND" "TYPESONGPOS" "TYPESONGSEL" "TYPECLOCK" "TYPESTART" 
           "TYPECONTINUE" "TYPESTOP" "TYPETUNE" "TYPEACTIVESENS" "TYPERESET" "TYPESYSEX"
           "TYPESTREAM" "TYPEPRIVATE" "TYPEPROCESS" "TYPEDPROCESS" 
           "TYPEQFRAME" "TYPECTRL14B" "TYPENONREGPARAM" "TYPEREGPARAM"
           "TYPESEQNUM" "TYPETEXTUAL" "TYPECOPYRIGHT" "TYPESEQNAME"
           "TYPEINSTRNAME" "TYPELYRIC" "TYPEMARKER" "TYPECUEPOINT"
           "TYPECHANPREFIX" "TYPEENDTRACK" "TYPETEMPO" "TYPESMPTEOFFSET"
           "TYPETIMESIGN" "TYPEKEYSIGN" "TYPESPECIFIC" "TYPEPORTPREFIX"
           "TYPERCVALARM" "TYPEAPPLALARM" "TYPERESERVED" "TYPEDEAD"
           "MIDIERRSPACE" "MIDIERRREFNUM" "MIDIERRBADTYPE" "MIDIERRINDEX"
           "MODEMPORT" "PRINTERPORT" "MIDIEXTERNALSYNC" "MIDISYNCANYPORT"
           "SMPTE24FR" "SMPTE25FR" "SMPTE29FR" "SMPTE30FR"
           "MIDIOPENAPPL" "MIDICLOSEAPPL" "MIDICHGNAME" "MIDICHGCONNECT"
           "MIDIOPENMODEM" "MIDICLOSEMODEM" "MIDIOPENPRINTER"
           "MIDICLOSEPRINTER" "MIDISYNCSTART" "MIDISYNCSTOP" "MIDICHANGESYNC"
           "MIDIOPENDRIVER" "MIDICLOSEDRIVER" "MIDIADDSLOT" "MIDIREMOVESLOT"
           "MIDICHGSLOTCONNECT" "MIDICHGSLOTNAME"
           "INSTALL-MIDISHARE-INTERFACE" "REMOVE-MIDISHARE-INTERFACE"
           "LINK" "DATE" "EVTYPE" "REF" "PORT" "CHAN" "FIELD" "FIELDSLIST" "PITCH" "VEL" "DUR"
           ;; "LINKSE" "LINKST"
           "KPRESS" "CTRL" "PARAM" "NUM" "PREFIX" "TEMPO" "SECONDS"
           "SUBFRAMES" "VALINT" "PGM" "BEND" "CLK" "SONG" "FIELDS" "TEXT" "FMSG" "FCOUNT"
           "TSNUM" "TSDENOM" "TSCLICK" "TSQUARTER" "ALTERATION" "MINOR-SCALE" "INFO"
           "FIRSTEV" "LASTEV" 
           "MIDIGETVERSION" "MIDICOUNTAPPLS" "MIDIGETINDAPPL"
           "MIDIGETNAMEDAPPL" "MIDIOPEN" "MIDICLOSE"
           "MIDIGETNAME" "MIDISETNAME" "MIDIGETINFO" "MIDISETINFO" "MIDINEWFILTER"
           "MIDIFREEFILTER" "MIDIACCEPTCHAN" "MIDIACCEPTTYPE" "MIDIACCEPTPORT"
           "MIDIISACCEPTEDCHAN" "MIDIISACCEPTEDTYPE" "MIDIISACCEPTEDPORT"
           "MIDIGETFILTER" "MIDISETFILTER" "MIDIGETRCVALARM" "MIDISETRCVALARM"
           "MIDIGETAPPLALARM" "MIDISETAPPLALARM"
           "MIDICONNECT" "MIDIISCONNECTED" "MIDIGETPORTSTATE"
           "MIDISETPORTSTATE" "MIDIFREESPACE" "MIDINEWEV" "MIDICOPYEV" "MIDIFREEEV"
           ;; "MIDISETFIELD" "MIDIGETFIELD"
           "MIDIADDFIELD" "MIDICOUNTFIELDS"
           "MIDINEWSEQ" "MIDIADDSEQ" "MIDIFREESEQ" "MIDICLEARSEQ" "MIDIAPPLYSEQ" "MIDIGETTIME"
           "MIDISENDIM" "MIDISEND" "MIDISENDAT"
           "MIDICOUNTEVS" "MIDIGETEV" "MIDIAVAILEV" "MIDIFLUSHEVS"
           ;; "MIDIREADSYNC" "MIDIWRITESYNC"
           "MIDICALL" "MIDITASK" "MIDIDTASK" "MIDIFORGETTASKHDL" "MIDIFORGETTASK"
           "MIDICOUNTDTASKS" "MIDIFLUSHDTASKS" "MIDIEXEC1DTASK"
           "MIDINEWCELL" "MIDIFREECELL" "MIDITOTALSPACE" "MIDIGROWSPACE"
           "MIDIGETSYNCINFO" "MIDISETSYNCMODE" "MIDIGETEXTTIME"
           "MIDIINT2EXTTIME" "MIDIEXT2INTTIME" "MIDITIME2SMPTE"
           "MIDISMPTE2TIME" "MIDICOUNTDRIVERS" "MIDIGETINDDRIVER" "MIDIGETDRIVERINFOS"
           "MIDIGETINDSLOT" "MIDIGETSLOTINFOS" "MIDICONNECTSLOT"
           "MIDIISSLOTCONNECTED"
           "MIDINEWSMPTELOCATION" "MIDIFREESMPTELOCATION"
           "MIDINEWSYNCINFO" "MIDIFREESYNCINFO"
           "NULL-EVENT-P" "NULLPTRP" "NULLPTR"
           ;; "LOAD-FRAMEWORK-BUNDLE" "GET-FUN-ADDR"
           ;; "ADD-STARTUP-ACTION" "ADD-QUIT-ACTION"

           ;; stuff from player
           "KIDLE" "KPAUSE" "KRECORDING" "KPLAYING" "KWAITING" "KMAXTRACK" "KMUTEON"
           "KMUTEOFF" "KSOLOON" "KSOLOOFF" "KMUTE" "KSOLO" "KNOTRACK" "KERASEMODE"
           "KMIXMODE" "KLOOPON" "KLOOPOFF" "KSTEPPLAY" "KSTEPMUTE" "KINTERNALSYNC"
           "KCLOCKSYNC" "KSMPTESYNC" "KEXTERNALSYNC" "KNOSYNCOUT" "KCLOCKSYNCOUT"
           "MIDIFILE0" "MIDIFILE1" "MIDIFILE2" "TICKSPERQUARTERNOTE" "SMPTE24"
           "SMPTE25" "SMPTE29" "SMPTE30" "NOERR" "ERROPEN" "ERRREAD" "ERRWRITE" "ERRVOL"
           "ERRGETINFO" "ERRSETINFO" "ERRMIDIFILEFORMAT" "PLAYERNOERR"
           "PLAYERERRAPPL" "PLAYERERREVENT" "PLAYERERRMEMORY" "PLAYERERRSEQUENCER"
           "PLAYER-FRAMEWORK" "P-BAR" "P-BEAT" "P-UNIT" "S-BAR" "S-BEAT" "S-UNIT" "S-DATE"
           "S-TEMPO" "S-NUM" "S-DENOM" "S-CLICK" "S-QUARTER" "S-STATE"
           "S-SYNCIN" "S-SYNCOUT"
           "MF-FORMAT" "MF-TIMEDEF" "MF-CLICKS" "MF-TRACKS"
           "OPENPLAYER" "CLOSEPLAYER" "OPEN-PLAYER"
           "STARTPLAYER" "CONTPLAYER" "STOPPLAYER" "PAUSEPLAYER"
           "SETRECORDMODEPLAYER" "RECORDPLAYER" "SETRECORDFILTERPLAYER"
           "SETPOSBBUPLAYER" "SETPOSMSPLAYER" 
           "SETLOOPPLAYER" "SETLOOPSTARTBBUPLAYER"
           "SETLOOPENDBBUPLAYER" "SETLOOPSTARTMSPLAYER" "SETLOOPENDMSPLAYER"
           "SETSYNCHROINPLAYER" "SETSYNCHROOUTPLAYER" "SETSMPTEOFFSETPLAYER"
           "SETTEMPOPLAYER" "GETSTATEPLAYER" "GETENDSCOREPLAYER" 
           "FORWARDSTEPPLAYER" "BACKWARDSTEPPLAYER" "GETALLTRACKPLAYER"
           "GETTRACKPLAYER" "SETTRACKPLAYER" "SETALLTRACKPLAYER" "SETPARAMPLAYER"
           "INSERTALLTRACKPLAYER" "INSERTTRACKPLAYER" "MIDIFILESAVE"
           "MIDIFILELOAD" "MIDI-FILE-SAVE" "MIDI-FILE-LOAD" "MIDINEWMIDIFILEINFOS"
           "MIDIFREEMIDIFILEINFOS"
           "MIDINEWPLAYERSTATE" "MIDIFREEPLAYERSTATE" "MIDINEWPOS" "MIDIFREEPOS"
           "VERSION"))



(cl:defpackage "PATCHWORK.SCHEDULER"
  (:use "COMMON-LISP" "CLOSER-MOP" "UI" "LELISP-MACROS" "PATCHWORK.MIDI")

  (:shadowing-import-from "CLOSER-MOP"
                          "STANDARD-CLASS" "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD"
                          "DEFMETHOD" "DEFGENERIC")
  
  (:import-from "UI"
                "WITHOUT-INTERRUPTS" "*EVENTHOOK*" "EVENT-DISPATCH")
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


(cl:defpackage "PATCHWORK"
  (:nicknames "PW")
  (:use "COMMON-LISP")
  ;; (:use "COMMON-LISP-STEPPER")
  (:use "CLOSER-MOP" "UI" "LELISP-MACROS" "PATCHWORK.SCHEDULER")
  (:use "REDIRECTING-STREAM")
  (:import-from "PATCHWORK.MIDI" "MIDI-RECORD" "MIDI-LOAD" "MIDI-SAVE")
  (:shadowing-import-from "CLOSER-MOP"
                          "STANDARD-CLASS" "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD"
                          "DEFMETHOD" "DEFGENERIC")
  (:export "ENABLE-PATCHWORK-READER-MACROS"
           "DISABLE-PATCHWORK-READER-MACROS")

  (:export "INITIALIZE")
  
  (:export "DRAW-CHAR" "DRAW-STRING" "DRAW-POINT" "DRAW-LINE" 
           "DRAW-RECT*" "FILL-RECT*" "DRAW-ELLIPSE" "FILL-ELLIPSE"
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
           "MAKE-POPUPBOX" "MAKE-PW-STANDARD-BOX" 
           "MY-ARRAY" "NEW-LEAFMENU" "NEW-MENU" "NOTES"
           "OPEN-PATCH-WIN" "OPEN-PW-CONTROLS-DIALOG" "OUT-PUT"
           "PATCH-VALUE" "PW-ADDMENU" "PW-CONTROLS" "PW-FUNCTION"
           "PW-FUNCTION-STRING" "PW-OBJECT" "REMOVE-YOURSELF-CONTROL"
           "RESIZE-PATCH-BOX" "SAVE" "SAVED-SELECTED" "SBOX" "SELF"
           "SET-BOX-TITLE" "SET-DIALOG-ITEM-TEXT-FROM-DIALOG"
           "SET-PW-WIN+PW-OBJ" "T-TIME" "TYPE-LIST" "UPDATE-EDITOR"
           "UPDATE-NOTE" "VALUE"  "STOP-PLAY"
           "YOURSELF-IF-COLLECTING"
           
           "GET-LOCK-BUTTON-FUN"
           
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

(cl:defpackage "USER-SUPPLIED-IN-OUTS"
  (:use))

(cl:defpackage "C-GET-NOTE-SLOTS"
  (:use "COMMON-LISP")
  (:use "PATCHWORK" "LELISP-MACROS")
  (:export "GET-NOTE-SLOTS" "SET-NOTE-SLOTS"))

(cl:defpackage "C-GET-SELECTIONS"
  (:use "COMMON-LISP")
  (:import-from "PATCHWORK"
                "DEFUNP" "C-PATCH-MIDI-MOD" "SAVED-SELECTED"
                "APPLICATION-OBJECT" "C-PATCH" "PATCH-VALUE" "INPUT-OBJECTS")
  (:import-from "UI" "SUBVIEWS" "WPTR")
  (:export "GET-SELECTIONS"))

(cl:defpackage "C-PATCH-BUFFER"
  (:use "COMMON-LISP")
  ;; (:use "COMMON-LISP-STEPPER")
  (:use "UI" "PATCHWORK")
  (:export "C-PATCH-BUFFER" "THE-BUFFER" "BUFFER" "C-RADIO-BUTTON"
           "GET-LOCK-BUTTON-FUN" "VALUE"))

(cl:defpackage "C-PATCH-ACCUM"
  (:use "COMMON-LISP")
  ;; (:use "COMMON-LISP-STEPPER")
  (:use "UI" "PATCHWORK"  "C-PATCH-BUFFER")
  (:export "C-PATCH-ACCUM" "THE-BUFFER" "ACCUM" "*ACCUM-BUFFER-LIMIT*"))

(cl:defpackage "C-PATCH-FILE-BUFFER"
  (:use "COMMON-LISP")
  ;; (:use "COMMON-LISP-STEPPER")
  (:use "LELISP-MACROS" "UI" "PATCHWORK")
  (:intern "ASCII-WIN" "C-PATCH-ASCII-BUFFER")
  (:export "C-PATCH-FILE-BUFFER"))

(cl:defpackage "C-PATCH-CHORD-LINE"
  (:use "COMMON-LISP")
  (:use "UI" "LELISP-MACROS" "PATCHWORK")
  (:export "C-PATCH-CHORD-LINE" "CHORD-SEQN"))

(cl:defpackage "C-PW-SEND-MIDI-NOTE"
  (:use "COMMON-LISP")
  (:use "LELISP-MACROS" "PATCHWORK")
  (:import-from "UI" "NIY")
  (:import-from "PATCHWORK.SCHEDULER" "APDFUNCALL" "START" "PRIORITY" "RE-DFUNCALL")
  (:export "SND-MIDINOTE" "C-PW-SEND-MIDI-NOTE"))

(cl:defpackage "C-PW-MIDI-IN"
  (:use "COMMON-LISP" "LELISP-MACROS" "PATCHWORK")
  (:import-from "PATCHWORK.SCHEDULER" "APDFUNCALL" "START" "PRIORITY" "RE-DFUNCALL" )
  (:import-from "PATCHWORK.MIDI" "MIDI-READ")
  (:import-from "UI" "NIY")
  (:export "PW-MIDI-IN" "M-DATA" "C-PW-MIDI-IN" "DELAY" "STATUS" "MIDI-CHAN" "DATA1"
           "DATA2" "MIDI-OPCODE" "C-PW-MIDI-IN-TOP" "C-PW-DELAY-BOX" "C-PW-NOTE-IN"
           "NOTE-IN" "C-PW-NOTE-ON-IN" "NOTE-ON-IN" "C-PW-CHORD-IN" "CHORD-IN"
           "*MIDI-BOX-POPUPMENU*"))

;; A few circular dependencies:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(C-patch-buffer:C-patch-buffer
            C-patch-accum:C-patch-accum
            C-patch-file-buffer:C-patch-file-buffer
            C-patch-file-buffer::Ascii-win
            C-patch-file-buffer::C-patch-ascii-buffer)
          "PATCHWORK"))

(cl:defpackage "COMBINATORIAL-INTERV"
  (:use "COMMON-LISP" "LELISP-MACROS" "PATCHWORK")
  (:export "FIND-INTERVALS" "INT-REC"))

(cl:defpackage "PW-STYPE"
  (:use "COMMON-LISP" "LELISP-MACROS" "PATCHWORK")
  (:export "CAR!" "LIST!" "DEEP-MAPCAR" "DOUBLE-MAPCAR"))

(cl:defpackage "EPW" 
  (:use "COMMON-LISP" "LELISP-MACROS" "CLPF-UTIL" "PATCHWORK" "PW-STYPE")
  (:shadow "MAKE-NUM-FUN")
  (:export "*ASCII-NOTE-C-SCALE*" "*ASCII-NOTE-DO-SCALE*"
           "*ASCII-NOTE-SCALES*" "ARITHM-SER"
           "AVERAGE" "DEFUNE" "DISTOR" "DISTOR-EXT" "F-BINARY-SEARCH"
           "FLAT" "FLAT-ONCE" "FUN-MINMAX" "G-MAX" "G-MIN" "G-SCALING"
           "INCLUDED?" "INTERPOLATION" "L-LAST" "L-MAX" "L-MIN"
           "L-NTH" "L-SCALER/MAX" "L-SCALER/SUM" "L-SUPPRESS"
           "LIST-EXPLODE" "LIST-FILL" "LIST-PART" "LO-FLAT"
           "MAKE-LIST2" "MAT-TRANS" "NTH-RANDOM" "PERMUT-CIRC"
           "PERMUT-RANDOM" "SORT-LIST" "X-APPEND" "X-DIFF"
           "X-INTERSECT" "X-UNION" "X-XOR" "MAKE-NUM-FUN" "LAGRANGE"
           "LINEAR" "POWER-FUN" "POWER/2" "POWER/3" "PARABOLE/2"
           "PARABOLE/3"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import
   '(epw:make-list2 epw::approx-m epw:arithm-ser epw:average epw::band-filter
     epw::band-pass epw::band-reject epw::band-select epw::cartesian
     epw::cents->coef epw::coef->cents epw::create-list epw::densifier
     epw:distor epw:distor-ext epw::dx->x epw::f->mc epw::fibo-ser
     epw:flat epw::flat-low epw:flat-once epw::fun-bin-search epw::g*
     epw::g+ epw::g- epw::g-abs epw::g-alea epw::g-average
     epw::g-ceiling epw::g-div epw::g-exp epw::g-floor epw::g-log
     epw:g-max epw:g-min epw::g-mod epw::g-oper epw::g-power
     epw::g-random epw::g-round epw:g-scaling epw::g-scaling/max
     epw::g-scaling/sum epw::g/ epw::geometric-ser epw:included?
     epw::int->symb epw:interpolation epw::inverse epw::l* epw::l+
     epw::l- epw::l-delete epw::l-exp epw:l-last epw:l-max epw:l-min
     epw:l-nth epw::l-order epw::l-power epw::l-scale% epw:l-scaler/max
     epw:l-scaler/sum epw::l/ epw::last-elem epw::lin->db
     epw:list-explode epw:list-fill epw::list-filter epw::list-modulo
     epw:list-part epw::ll-abs epw::ll-log epw::ll-oper epw::ll/floor
     epw::ll/mod epw::ll/round epw::llalea epw::lldecimals epw:lo-flat
     epw:mat-trans epw::matrix-oper epw::mc->f epw::mc->n
     epw::multi-filter epw::n->mc epw::nbcents-f epw:nth-random
     epw:permut-circ epw:permut-random epw::posn-match epw::posn-order
     epw::prime-factors epw::prime-ser epw::prime? epw::range-filter
     epw::rem-dups epw::sample-fun epw:sort-list epw::symb->int
     epw::table-filter epw::x->dx epw:x-append epw:x-diff
     epw:x-intersect epw:x-union epw:x-xor)
   "PATCHWORK"))



(cl:defpackage "CLOS-APPLE-EVENT"
  (:use "COMMON-LISP" "UI")
  (:nicknames "CLOSAE")
  (:export "APPLEEVENT" "GETPARAM" "PUTPARAM"
           "AEDESC" "GETDESCRECPTR" "ASAEDESC"
           "OBJECTSPECIFIER"
           "GETCLASS" "GETCONTAINER" "GETFORM" "GETDATA"
           "SETCLASS" "SETCONTAINER" "SETFORM" "SETDATA"
           "WITH-AEDESCS" "CREATE-SELF-TARGET" "CREATE-APPLEEVENT"
           "AESEND" "CHECK-REPLY-ERROR"))
 
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
