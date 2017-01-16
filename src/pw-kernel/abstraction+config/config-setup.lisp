;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               config-setup.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;
;;;;    User Patch configuration
;;;;
;;;;AUTHORS
;;;;    Mikael Laurson, Jacques Duthen, Camilo Rueda.
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-07 <PJB> Changed license to GPL3; Added this header.
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
(in-package :pw)

;;;============================================================
;;;A dynamic user library configuration scheme for PW. Each function in a PW
;;;user library can be surrounded by an 'eval-if-key' macro call which does
;;;selecting loading based on an a-list of user supplied keywords. The a-list takes
;;;the form: ((logical-fun-name1 . keyword1)...(logical-fun-nameN . keywordN))
;;;Where 'logical-fun-name' is a symbol representing externally, in an associated
;;;PW configuration dialog, a function or set of related functions in the user library.
;;;'keyword' represents internally this set of related functions. The following example,
;;;taken from the ESQUISSE library configuration, ilustrates this syntax:

;;; the a-list for configuring ESQUISSE
;; (setq *epw-config-a-list*
;;       '(((sortlist list-min list-max) . :sort-and-list )
;;         ((unique union intersection a\\b aΔb included? ensemble? compare) . :set-operations)
;;         ((f-mc mc-f ascii-note) . :pitch-conversion)
;;         ((approx-m midi-center2 midi-center freq-center2 freq-center midi-av freq-av
;;           harm-ser harm-ser-f sharm2 sharm2-f sgrm ring ring-fab plaque-f plaque-fab offset
;;           mul-chord mul-chord2 make-len-ch inter-chord distor-m distor-ch)
;;          . :pitch-ops)))

;;;such an a-list definition should be the first Lisp instruction in the user library.
;;;The user can then structure the library in coherent sets of interdependent functions
;;;that must always be loaded together. The 'eval-if-key' macro serves this purpose, as
;;;illustrated bellow for some ESQUISSE functions:

;; (eval-if-key (:pitch-conversion
;;
;;               (defune f-mc1 ((freq freq)) midic
;;                   "Converts <freq> (Hz) to a midicent pitch."
;;                 (+ (round (* (log (/ freq *diapason-freq*)) #.(/ 12.0 (log 2.0))) .01)
;;                    *diapason-midic* ))
;;
;;               (defune f-mc ((freqs? freqs?)) midics?
;;                   "Converts <freqs?> (Hz) to midicents."
;;                 (deep-mapcar 'f-mc 'f-mc1 freqs?))
;;
;;               ;; ---- midic -> frequency ----
;;
;;               (defune mc-f1 ((midic midic)) freq
;;                   "Converts a midicent pitch to a frequency (Hz)."
;;                 (* *diapason-freq*
;;                    (expt 2.0 (/ (- midic *diapason-midic*) 1200.0)) ))
;;
;;               (defune mc-f ((midics? midics?)) freqs?
;;                   "Converts a midicent pitches to frequencies (Hz)."
;;                 (deep-mapcar 'mc-f 'mc-f1 midics?))
;;
;;               ) )    ; end of eval-if-key
;;
;; (eval-if-key (:pitch-conversion
;;
;;               (PW-addmenu *epw-Pitch-conversion-menu*
;;                           '(f-mc mc-f))
;;               ) )  ;end of eval-if-key


;;;Notice that the corresponding menu insertion call is also surrounded by the macro
;;;so that menus and function loadings remain coherent.

;;;The macro call also accepts lists of keys so that a function needed for several
;;;sets can be loaded if any one of them is requested:

;; (eval-if-key ((:arithmetic :sequences)
;;
;;               (defun foo (m n) (bar m n))
;;
;;               ) )  ; end of eval-if-key

;;;The function FOO will be loaded if any one of the :arithmetic or :sequences functions
;;;is requested

;;;Libraries set-up in this way are totally transparent to the user who may simply
;;;load them directly without using the configurer

;;;============================================================
;;for surrounding definitions in libraries


(defmacro eval-list (form-list)
  `(mapc #'eval ,form-list))

(defmacro eval-if-key (&rest key-code-pairList)
  "evaluates body if key-code is in *library-selection* "
  (let((sym (gensym "pair")))
  `(dolist (,sym ',key-code-pairlist)
     (cond ((null *library-selection*)
            (eval-list (cdr ,sym)))
           ((eql (car ,sym) :allways)
            (eval-list (cdr ,sym)))
           ((consp (car ,sym))
            (if (intersection (car ,sym) *library-selection* )
              (eval-list (cdr ,sym))))
           ((member (car ,sym) *library-selection* )
            (eval-list (cdr ,sym)))))))

(defvar *PW-user-library-extension* "lib")
(defvar *library-selection* ())

;;The path-names for user patches and user libraries. Folders "PW-user-patches" and
;;"PW-user-library" should exist somewhere within the "PW;" path.
(defvar *PW-user-abstract-pathName* "PW-USER:PW-user-patches;**;*.*")
(defvar *PW-user-library-pathName* "PW-USER:PW-user-library;**;*.*")

;;A list of file paths for the current user configuration. Use by the new abstraction patch
(defvar  *user-patch-config* nil)

(defun get-user-patch-config()
  *user-patch-config*)

(defun set-user-patch-config(config)
  (setq *user-patch-config* config) )

;;;; THE END ;;;;
