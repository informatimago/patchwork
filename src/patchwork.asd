;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               patchwork.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;     ASDF system definition for the Patchwork Application.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-01 <PJB> Moved pw-macosx/reader-macros to pw-kernel/environment/reader-macros
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
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

(asdf:defsystem :patchwork
    :name "Patch Work Application"
    :description "Patch Work: IRCAM Computer Assisted Composition"
    :author "IRCAM"
    :version "1.0.0"
    :license "GPL3"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Spring 2012")
                 ((#:albert #:output-dir)          . "../documentation/patchwork/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :depends-on ("alexandria"
                 "closer-mop"
                 "mclgui"
                 ;; "com.informatimago.common-lisp.lisp.stepper"
                 )
    :components ((:file "packages" 
                        :depends-on ("pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/drivers+resources/midi"))
                 (:file "gf" 
                        :depends-on ("packages"))

                 (:file "pw-kernel/environment/reader-macros" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/clpf-utils"))
                 

                 ;; (:file "pw-lib/pwscript/appleevent-toolkit" 
                 ;;        :depends-on ("packages"))
                 ;; (:file "pw-lib/pwscript/pw-ae" 
                 ;;        :depends-on ("packages"
                 ;;                     "pw-lib/pwscript/appleevent-toolkit"))
                 (:file "pw-lib/pwscript/closae" 
                  :depends-on ("packages"))


                 (:file "pw-lib/pwscript/pw-scripting-gf"
                        :depends-on ())
                 
                 (:file "pw-lib/pwscript/pw-scripting" 
                        :depends-on ("packages"
                                     ;; "pw-lib/pwscript/pw-ae"
                                     "pw-lib/pwscript/closae"
                                     "pw-lib/pwscript/pw-scripting-gf" 
                                     "pw-kernel/boxes/data/file-buffer"
                                     "pw-kernel/boxes/data/list-editor"
                                     "pw-kernel/boxes/data/buffer-box"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-music/boxes/midi/pw-midi-box"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))

                 (:file "pw-lib/pwscript/record"
                        :depends-on ("packages"))

                 #-(and)
                 (:file "pw-lib/pwscript/recordables" 
                        :depends-on ("packages"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/abstraction+config/abstraction-m"
                                     "pw-kernel/box-creation/application-box"
                                     ;; "pw-kernel/box-creation/pw-patch"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-window"
                                     "pw-kernel/boxes/control/basic-library-boxes"
                                     "pw-kernel/boxes/control/enumerator"
                                     "pw-kernel/boxes/data/buffer-box"
                                     "pw-kernel/boxes/data/file-buffer"
                                     "pw-kernel/boxes/data/list-editor"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-lib/pwscript/closae"
                                     "pw-lib/pwscript/pw-scripting-gf"
                                     "pw-music/boxes/edit/ch-line-build"
                                     "pw-music/boxes/edit/midi-instrument-editors"
                                     "pw-music/boxes/edit/mn-editor-polif"
                                     "pw-music/boxes/edit/pw-chord-box"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/boxes/edit/rtm-patch"
                                     "pw-music/editors/mn/mn-collector-view"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-music/editors/mn/mn-window"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/rhythm/rtm-window"))

                 
                 #-(and)
                 (:file "pw-lib/midishare/midishare" 
                        :depends-on (#+cl-user::cocoa-midi-player "pw-lib/midishare/cocoa-midi-player"))

                 #-(and)
                 (:file "pw-lib/midishare/midiplay" 
                        :depends-on ("packages"
                                     #+cl-user::cocoa-midi-player "pw-lib/midishare/cocoa-midi-player"
                                     "pw-kernel/drivers+resources/midi"
                                     "pw-music/boxes/edit/rhythm-formation"
                                     "pw-kernel/box-creation/pw-patch"
                                     "pw-music/boxes/edit/rtm-patch"
                                     "pw-music/editors/rhythm/rtm-editor"
                                     "pw-music/editors/rhythm/rtm-paging+kill"
                                     "pw-music/boxes/edit/pw-chord-box"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/mn/mn-collector-view"
                                     "pw-music/editors/mn/mn-chord-ed"
                                     "pw-music/editors/mn/mn-note-chord-chordline"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-lib/midishare/midirecord" 
                        :depends-on ("packages"
                                     "pw-kernel/drivers+resources/midi"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mn-pw-interface"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/types/pw-type-scheme"
                                     ;; "pw-lib/midishare/midiplay"
                                     ;; #+cl-user::cocoa-midi-player "pw-lib/midishare/cocoa-midi-player"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/editors/mn/mn-note-chord-chordline"
                                     ))

                 #+cl-user::cocoa-midi-player
                 (:file "pw-lib/midishare/cocoa-midi-player" 
                        :depends-on ())
                 
                 #-(and)
                 (:file "pw-lib/midishare/playerppc" 
                        :depends-on ("packages"))

                 
                 (:file "pw-lib/cleni/cleni" 
                        :depends-on ("packages"))


                 
                 (:file "pw-kernel/environment/lelisp-macros" 
                        :depends-on ())
                 
                 (:file "pw-kernel/environment/clpf-utils" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/drivers+resources/midi"))
                 
                 (:file "pw-kernel/environment/mac-rsrc" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"))
                 
                 (:file "pw-kernel/environment/midi-note" 
                        :depends-on ("packages"
                                     "pw-kernel/drivers+resources/midi"
                                     ;; "pw-lib/midishare/midishare"
                                     ))
                 
                 (:file "pw-kernel/environment/pw-debug" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-kernel/environment/pw-scheduler" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/drivers+resources/midi"
                                     "pw-kernel/drivers+resources/scheduler" ))
                 
                 (:file "pw-kernel/environment/pw-symbolic-types" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/pw-graphics/controls/pw-controls"))
                 
                 
                 (:file "pw-kernel/environment/epw-package"  
                        :depends-on ("packages"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/environment/pw-symbolic-types"
                                     "pw-kernel/box-creation/resize+extend-patch"
                                     "pw-kernel/environment/clpf-utils"
                                     "pw-kernel/types/pw-type-scheme"))

                 
                 (:file "pw-kernel/drivers+resources/midi" 
                        :depends-on (
                                     ;; "pw-lib/midishare/midishare"
                                     #+cl-user::cocoa-midi-player "pw-lib/midishare/cocoa-midi-player"))
                 
                 
                 (:file "pw-kernel/drivers+resources/scheduler" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/drivers+resources/midi"))

                 (:file "pw-lib/epw-1.0b/epw-menus" 
                        :depends-on ("pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-lib/epw-1.0b/harmonicity"
                                     "pw-lib/epw-1.0b/crime-fm"
                                     "pw-lib/epw-1.0b/chord-filter"
                                     "pw-lib/epw-1.0b/combinatorial"
                                     "pw-lib/epw-1.0b/freq-harmony"))

                 (:file "pw-lib/epw-1.0b/chord-filter" 
                        :depends-on ("pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-lib/epw-1.0b/combinatorial" 
                        :depends-on ("pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-lib/epw-1.0b/crime-fm" 
                        :depends-on ("pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/environment/epw-package"))
                 
                 (:file "pw-lib/epw-1.0b/freq-harmony" 
                        :depends-on ("pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-lib/epw-1.0b/harmonicity" 
                        :depends-on ("pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-type-scheme"))


                 
                 (:file "pw-kernel/types/pw-box-to-menu" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-kernel/types/pw-type-scheme" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/pw-graphics/controls/pw-controls"))

                 
                 (:file "pw-kernel/boxes/arithmetic/basic-funs" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/num-fun-gen/num-fun-gen"
                                     "pw-kernel/environment/clpf-utils"
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-kernel/boxes/data/buffer-box" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"))

                 (:file "pw-kernel/boxes/data/data-boxes" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"))                 
                 

                 (:file "pw-kernel/boxes/data/file-buffer" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/pw-graphics/controls/popupmenu"))
                 
                 (:file "pw-kernel/boxes/data/table-displayer" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-kernel/boxes/data/list-editor" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/boxes/data/table-displayer"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-kernel/pw-graphics/window+menu/application-window"))
                 
                 (:file "pw-kernel/boxes/data/lst-ed-help" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/boxes/data/text-box" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"))
                 
                 (:file "pw-kernel/boxes/list/pw-list-functions" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-kernel/boxes/multidim/multidim" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mn-pw-interface"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-music/boxes/edit/pw-mn-collector"))
                 
                 (:file "pw-kernel/boxes/num-series/num-series" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/num-fun-gen/num-fun-gen"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-lib/epw-1.0b/freq-harmony"))

                 (:file "pw-kernel/boxes/num-fun-gen/num-fun-gen" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/clpf-utils"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/environment/reader-macros"))
                 
                 
                 (:file "pw-kernel/boxes/vision/oscilloscope" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/drivers+resources/scheduler"))
                 
                 (:file "pw-kernel/boxes/vision/points-view" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/types/pw-type-scheme"))

                 
                 (:file "pw-kernel/boxes/bpf/bpf-boxes/multi-bpf" 
                        :depends-on ("packages"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-menu+library"
                                     "pw-kernel/boxes/bpf/bpf-editors/break-point-function"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mini-view"
                                     "pw-kernel/boxes/bpf/bpf-boxes/pw-bpf-boxes"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-boxes/pw-bpf-boxes" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/environment/epw-package"  
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/boxes/data/buffer-box"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-menu+library"
                                     "pw-kernel/boxes/bpf/bpf-editors/break-point-function"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mini-view"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-editor" 
                  :depends-on ("packages"
                               "pw-kernel/pw-graphics/controls/pw-graphics"
                               "pw-kernel/boxes/bpf/bpf-editors/break-point-function"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-hardcopy" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-help-window" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-menu"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-menu+library" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-menu" 
                                     "pw-kernel/boxes/bpf/bpf-editors/break-point-function"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-mini-view" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-editor"
                                     "pw-kernel/boxes/bpf/bpf-editors/break-point-function"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-mn-pw-interface" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/bpf/bpf-editors/break-point-function"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-window" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-help-window" 
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-menu"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mini-view"
                                     "pw-kernel/environment/mac-rsrc" 
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/box-creation/pw-patch"
                                     "pw-kernel/pw-graphics/controls/mouse-window"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-kernel/pw-graphics/window+menu/application-window"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/break-point-function" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"))
                 
                 (:file "pw-kernel/boxes/control/basic-library-boxes" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/pw-scheduler"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/boxes/data/buffer-box"))
                 
                 (:file "pw-kernel/boxes/control/enumerator" 
                        :depends-on ("packages"
                                     "pw-kernel/box-creation/pw-patch"
                                     "pw-kernel/types/pw-types"))

                 (:file "pw-kernel/boxes/control/reducer" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 
                 (:file "pw-kernel/box-creation/application-box" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/pw-graphics/window+menu/application-window"
                                     "pw-kernel/boxes/data/buffer-box"))
                 
                 (:file "pw-kernel/box-creation/browse-typed-boxes" 
                        :depends-on ("packages"
                                     "pw-music/boxes/edit/pw-mn-collector"))
                 
                 (:file "pw-kernel/box-creation/create-patch-box" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/pw-graphics/controls/pw-controls"))
                 
                 (:file "pw-kernel/box-creation/nargs-input" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/box-creation/patch+popupbox" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-kernel/box-creation/pw-lisp-functions" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"))
                 
                 (:file "pw-kernel/box-creation/pw-patch" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-kernel/box-creation/resize+extend-patch" 
                        :depends-on ("packages"))

                 
                 (:file "pw-kernel/pw-graphics/controls/mouse-window" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"))
                 
                 (:file "pw-kernel/pw-graphics/controls/popupmenu" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/pw-graphics/controls/pw-controls" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-kernel/pw-graphics/controls/pw-graphics" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/reader-macros"))
                 
                 (:file "pw-kernel/pw-graphics/window+menu/application-window" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/pw-graphics/window+menu/pw-hardcopy" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/pw-graphics/window+menu/pw-help-window" 
                        :depends-on ("packages"))

                 #-(and)
                 (:file "pw-kernel/environment/make-image"
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/pw-graphics/window+menu/pw-menu"
                        :depends-on ("packages"
                                     "pw-lib/pwscript/record"
                                     ;; "pw-kernel/environment/make-image"
                                     "pw-lib/pwscript/pw-scripting-gf"))


                 
                 (:file "pw-kernel/pw-graphics/window+menu/pw-window" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/environment/pw-scheduler"
                                     "pw-kernel/pw-graphics/window+menu/pw-help-window" 
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))

                 
                 (:file "pw-kernel/abstraction+config/abst-fun" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/environment/pw-scheduler"
                                     "pw-kernel/abstraction+config/abstraction"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/boxes/edit/pw-chord-box"))
                 
                 (:file "pw-kernel/abstraction+config/abstraction" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/box-creation/pw-patch"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"))
                 
                 (:file "pw-kernel/abstraction+config/abstraction-m" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"))
                 
                 (:file "pw-kernel/abstraction+config/config-setup" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/abstraction+config/pw-library-config" 
                        :depends-on ("packages"
                                     "pw-lib/pwscript/record"))






                 
                 (:file "pw-music/utilities/mn-hardcopy" 
                        :depends-on ("packages"
                                     "pw-music/editors/mn/mn-window"))
                 
                 (:file "pw-music/utilities/music-abst-fun" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/pw-scheduler"
                                     "pw-kernel/abstraction+config/abstraction"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/boxes/edit/pw-chord-box"))

                 
                 
                 (:file "pw-music/boxes/midi/pw-midi-box" 
                        :depends-on ("packages"
                                     "pw-kernel/drivers+resources/midi"
                                     "pw-kernel/drivers+resources/midi"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/types/pw-type-scheme"
                                     ;; "pw-lib/midishare/midiplay"
                                     "pw-music/editors/mn/mn-note-chord-chordline"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"))

                 
                 (:file "pw-music/boxes/conversion/conversion" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/epw-package"  
                                     "pw-kernel/types/pw-type-scheme"))

                 
                 (:file "pw-music/boxes/extern+multidim/md-note-slots" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/boxes/multidim/multidim"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/boxes/extern+multidim/structured-time" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/editors/mn/mn-window"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/boxes/extern+multidim/structured-time2" 
                        :depends-on ("packages"))

                 
                 (:file "pw-music/boxes/edit/ascii-chord-box-pw" 
                        :depends-on ("packages"))
                 
                 (:file "pw-music/boxes/edit/ch-line-build" 
                        :depends-on ("packages"
                                     "gf"
                                     ;; "pw-kernel/environment/reader-macros"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/boxes/multidim/multidim"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/boxes/edit/chord-box" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/boxes/edit/midi-instrument-editors" 
                        :depends-on ("packages"
                                     "gf"
                                     ;; "pw-kernel/environment/reader-macros"
                                     "pw-kernel/drivers+resources/scheduler" 
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mini-view"))
                 
                 (:file "pw-music/boxes/edit/midi-instrument-pw" 
                        :depends-on ("packages"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-menu+library"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mini-view"
                                     "pw-music/boxes/edit/pw-mn-collector"))
                 
                 (:file "pw-music/boxes/edit/mn-editor-polif" 
                        :depends-on ("packages"
                                     "pw-kernel/box-creation/application-box"))
                 
                 (:file "pw-music/boxes/edit/pw-chord-box" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/types/pw-types"
                                     "pw-music/editors/mn/mn-window"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/boxes/data/buffer-box"))
                 
                 (:file "pw-music/boxes/edit/pw-mn-collector" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-kernel/boxes/bpf/bpf-editors/bpf-mn-pw-interface"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/types/pw-types"
                                     "pw-kernel/environment/pw-scheduler"
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-music/editors/mn/mn-collector-view"
                                     "pw-music/editors/mn/mn-window"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/boxes/edit/quantizer" 
                        :depends-on ("packages"
                                     "pw-kernel/boxes/arithmetic/basic-funs" 
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/environment/lelisp-macros"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-music/boxes/edit/rtm-patch"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"))
                 
                 (:file "pw-music/boxes/edit/rhythm-formation" 
                        :depends-on ("packages"
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-music/boxes/extern+multidim/md-note-slots"
                                     "pw-music/boxes/edit/rtm-patch"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/boxes/edit/rtm-patch" 
                        :depends-on ("packages"
                                     "pw-kernel/box-creation/application-box"
                                     "pw-kernel/types/pw-type-scheme"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/drivers+resources/scheduler" 
                                     "pw-kernel/pw-graphics/window+menu/pw-window"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-music/menu/mn-menu"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/rhythm/rtm-window"))

                 
                 (:file "pw-music/menu/rtm-menu" 
                        :depends-on ("packages"
                                     "pw-music/editors/rhythm/global-vars"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-music/editors/rhythm/beat-measure-measure-line" 
                        :depends-on ("packages"
                                     "pw-music/editors/mn/mn-chord-ed"
                                     "pw-music/editors/mn/mn-collector-panel"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-music/editors/rhythm/global-vars"
                                     ;; "pw-music/editors/rhythm/rtm-window"
                                     "pw-kernel/drivers+resources/scheduler" 
                                     "pw-kernel/pw-graphics/controls/popupmenu"))
                 
                 (:file "pw-music/editors/rhythm/print-rtm" 
                        :depends-on ("packages"
                                     "pw-music/editors/rhythm/rtm-window"))
                 
                 (:file "pw-music/editors/rhythm/rtm-cleni-interface" 
                        :depends-on ("packages"
                                     "pw-lib/cleni/cleni"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-music/editors/rhythm/rtm-dialog-win" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/rhythm/rtm-window"))
                 
                 (:file "pw-music/editors/rhythm/rtm-editor" 
                        :depends-on ("packages"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-music/editors/rhythm/global-vars"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/editors/rhythm/rtm-help-window" 
                        :depends-on ("packages"))
                 
                 (:file "pw-music/editors/rhythm/rtm-midi-files" 
                        :depends-on ("packages"))
                 
                 (:file "pw-music/editors/rhythm/rtm-paging+kill" 
                        :depends-on ("packages"
                                     "pw-music/editors/rhythm/global-vars"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-music/boxes/edit/rtm-patch"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"))
                 
                 (:file "pw-music/editors/rhythm/rtm-selection-button" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"))
                 
                 (:file "pw-music/editors/rhythm/rtm-window" 
                        :depends-on ("packages"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-music/editors/rhythm/global-vars"
                                     "pw-kernel/pw-graphics/window+menu/application-window"
                                     "pw-music/editors/rhythm/beat-measure-measure-line"))

                 
                 (:file "pw-music/editors/mn/chordbox-help-win" 
                        :depends-on ("packages"))
                 
                 (:file "pw-music/editors/mn/midi-files" 
                        :depends-on ("packages"))
                 
                 (:file "pw-music/editors/mn/mn-chord-ed" 
                        :depends-on ("packages"
                                     "pw-music/menu/mn-menu"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-music/editors/mn/mn-note-chord-chordline"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-kernel/pw-graphics/controls/popupmenu"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-music/editors/mn/mn-collector-panel" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-music/menu/mn-menu"
                                     "pw-music/editors/mn/mn-window"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-music/editors/mn/mn-chord-ed"
                                     "pw-music/editors/mn/mn-note-chord-chordline"
                                     "pw-music/editors/mn/mn-collector-view" 
                                     "pw-kernel/pw-graphics/controls/pw-graphics"))
                 
                 (:file "pw-music/editors/mn/mn-collector-view" 
                        :depends-on ("packages"
                                     "gf"
                                     ;; "pw-kernel/environment/reader-macros"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-music/editors/mn/mn-window"
                                     "pw-music/editors/mn/mn-editor"
                                     "pw-music/editors/mn/mn-note-chord-chordline"))
                 
                 (:file "pw-music/editors/mn/mn-editor" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-kernel/pw-graphics/controls/pw-controls"
                                     "pw-music/menu/mn-menu"
                                     "pw-music/editors/mn/mn-window"))
                 
                 (:file "pw-music/editors/mn/mn-help-window" 
                        :depends-on ("packages"
                                     "pw-music/menu/mn-menu"))
                 
                 (:file "pw-music/editors/mn/mn-note-chord-chordline"
                        :depends-on ("packages"
                                     "gf"
                                     "pw-kernel/drivers+resources/scheduler"
                                     "pw-kernel/pw-graphics/controls/pw-graphics"
                                     "pw-kernel/environment/epw-package"
                                     "pw-music/editors/mn/mn-editor"))

                 (:file "pw-music/editors/mn/mn-window" 
                        :depends-on ("packages"
                                     "pw-music/editors/mn/mn-help-window"
                                     "pw-kernel/pw-graphics/controls/mouse-window"
                                     "pw-kernel/pw-graphics/window+menu/application-window"))

                 (:file "pw-music/menu/music-basic-library" 
                        :depends-on ("packages"
                                     "gf"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-music/menu/mn-menu"
                                     "pw-music/boxes/extern+multidim/md-note-slots"
                                     "pw-music/boxes/midi/pw-midi-box" 
                                     "pw-music/boxes/edit/pw-mn-collector"
                                     "pw-music/boxes/edit/pw-chord-box"
                                     "pw-music/boxes/edit/ch-line-build"))
                 
                 (:file "pw-music/menu/mn-menu" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))
                 
                 (:file "pw-music/editors/rhythm/global-vars" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/types/pw-types" 
                        :depends-on ("packages"
                                     ;; "pw-kernel/environment/reader-macros"
                                     "pw-kernel/pw-graphics/controls/pw-controls"))
                 
                 (:file "pw-kernel/types/object-types" 
                        :depends-on ("packages"))
                 
                 (:file "pw-kernel/pw-graphics/window+menu/kernel-menu-items" 
                        :depends-on ("packages"
                                     "pw-lib/pwscript/record"
                                     "pw-kernel/environment/epw-package"
                                     "pw-kernel/environment/pw-scheduler"
                                     "pw-kernel/types/pw-box-to-menu"
                                     "pw-kernel/abstraction+config/abstraction"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"
                                     "pw-kernel/boxes/data/file-buffer"
                                     "pw-kernel/boxes/data/list-editor"))
                 
                 (:file "pw-kernel/pw-graphics/controls/mini-scroller" 
                        :depends-on ("packages"))
                 
                 ;; (:file "pw-kernel/environment/make-image" 
                 ;;        :depends-on ("packages"))
                 
                 (:file "pw-kernel/boxes/bpf/bpf-editors/bpf-menu" 
                        :depends-on ("packages"
                                     "pw-kernel/pw-graphics/window+menu/pw-menu"))


                 ))
