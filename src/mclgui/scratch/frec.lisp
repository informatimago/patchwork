;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               frec.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Fred Record.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-23 <PJB> Created.
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

(in-package "FRED")

(defun fr.cursor (frec) (fr.buffer frec))
(defun (setf fr.cursor) (new-cursor frec) (setf (fr.buffer frec) new-cursor))

(defstruct (fred-record
             (:constructor cons-fred-record)
             (:conc-name fr.))
  buffer                        ; The buffer displayed in this frec
  curascent                     ; ascent of cursor line
  curdescent                    ; descent of cursor line
                                ; no position. Assumed to be #@(0 0). View system used for other locs.
  size                          ; Our size (point)
  wposm                         ; Desired window position (mark)
  selmarks                      ; Desired selection rgns, list of (b . z) marks
  lead                          ; Additional distance between lines (pixels)
  tabcount                      ; Number of spaces per tab (max 128)
  hscroll                       ; Desired amount of horizontal scroll (pixels)
  margin                        ; Size of left margin (and right, if wrapping)
  plist                         ; plist, for the user
  bticks                        ; Tick count at last blink
  cticks                        ; Tick count at last click
  cposn                         ; Position at last click
  bpoint                        ; Screen position of blinking char
  bpos                          ; Buffer position of blinking char
  curpoint                      ; Screen position of cursor at last display
  curcpos                       ; Buffer position of cursor at last display
  leading                       ; nil, t, float, or fixnum
  hpos                          ; margin-hscroll at last display
  linevec                       ; vector of screen line lengths at last display
  numlines                      ; number of lines in linevec
  bwin                          ; pos of first char in window at last display
  zwin                          ; pos after last char in window at last display
                                ; .. actually distance from last char to end of buf
                                ; .. meaningful if bmod and zmod are before it
                                ; .. what if bmod before and zmod after? meaningless  
  bmod                          ; unchanged top area since last display
  zmod                          ; unchanged bottom area since last display
  selrgn                        ; selection at last display (mac region)
  selposns                      ; positions of selections at last redisplay
  flags-slot                    ; assorted flags
  bpchar                        ; blinking char
  bp-ff                         ; blinking char font&face
  bp-ms                         ; blinking char mode&size
  vpos                          ; Vertical position at zwin
  owner                         ; A view
  lineascents                   ; vector of line ascents at last display
  linedescents                  ; vector of line descents at last display
  lineheights                   ; vector of (+ ascent descent leading)
  linewidths                    ; vector of line widths
  keyscript                     ; keyboard script last time we looked
  truezwin                      ; pos after last char in window at last display
                                ; meaningful if bmod and zmod are after it
                                ; if bmod before and zmod after also meaningless
                                ; if both zwins meaningless - dont do some case or other
  )



(defun reinit-frec (frec owner-or-wptr &optional (owner nil owner-p))
  (niy reinit-frec frec owner-or-wptr owner)
  ;; (frec-arg frec)
  ;; (unless owner-p
  ;;   (setq owner owner-or-wptr))         ; backward compatibility.
  ;; (let ((cursor (fr.cursor frec)))
  ;;   (unless (typep cursor 'buffer-mark)
  ;;     (ccl::report-bad-arg cursor 'buffer-mark)))
  ;; (setf (fr.owner frec) owner)
  ;; (let ((rgn (fr.selrgn frec)))
  ;;   (when (or (eq (ccl::%type-of rgn) 'dead-macptr)  ; may have been in a saved world
  ;;             (%null-ptr-p rgn))
  ;;     (setf (fr.selrgn frec) (setq rgn (%new-rgn)))
  ;;     (setf (fr.sel-valid-p frec) nil))
  ;;   (%check-frec-selrgn frec rgn))
  ;; (without-interrupts
  ;;     (pushnew frec *frec-list*))
  ;; (when (and (view-size owner) (view-position owner))
  ;;   (with-focused-view owner
  ;;     (%update-resized-lines frec)))
  (setf (fr.bmod frec) 0
        (fr.zmod frec) 0) 
  frec)


(defun %set-frec-justification (frec justification)
  (frec-arg frec)
  (setf (buffer-justification (fr.cursor frec)) justification)
  (case justification
    ((:left nil)
     (setf (fr.center-justified-p frec) nil
           (fr.right-justified-p frec) nil))
    (:center
      (setf (fr.center-justified-p frec) t
            (fr.right-justified-p frec) nil))

    (:right
     (setf (fr.center-justified-p frec) nil
           (fr.right-justified-p frec) t))))


(defun make-frec (cursor owner &optional (size (view-size owner)))
  (let* ((curpos (buffer-position cursor))
         (frec   (cons-fred-record)))
    (setf (fr.cursor frec)      cursor
          (fr.owner frec)       owner
          (fr.size frec)        size
          (fr.wposm frec)       (make-mark cursor 0 t)
          (fr.selmarks frec)    (list (cons (make-mark cursor curpos)
                                         (make-mark cursor curpos t)))
          (fr.lead frec) 0
          (fr.flags frec) 0
          (fr.tabcount frec)    (buffer-tabcount cursor)
          (fr.wrap-p frec)      (buffer-wrap-p cursor)
          (fr.word-wrap-p frec) (buffer-word-wrap-p cursor)
                                        ;(fr.justification frec)(buffer-justification cursor)
                                        ;(fr.line-right-p frec)(buffer-line-right-p cursor)  ;???
          (fr.hscroll frec)     0
          (fr.margin frec)      3
          (fr.plist frec)       nil
          (fr.bticks frec)      0 ;(#_TickCount)
          (fr.cticks frec)     -1
          (fr.cposn frec)      -1
          (fr.bpoint frec)     -1
          (fr.bpos frec)        nil
          (fr.curpoint frec)   -1
          (fr.curcpos frec)     0
          (fr.hpos frec)        3  ; should be same as margin
          (fr.linevec frec)     nil
          (fr.numlines frec)    0
          (fr.bwin frec)        0
          (fr.zwin frec)        0
          (fr.truezwin frec)    0
          (fr.bmod frec)        0
          (fr.zmod frec)        0
          (fr.selrgn frec)      nil
          (fr.selposns frec)    (list (cons curpos curpos))
          (fr.bpchar frec)      nil
          (fr.bp-ff frec)       0
          (fr.bp-ms frec)       0
          (fr.vpos frec)        0)
    (setf (fr.caret-on-p frec)  nil)
    (%set-frec-justification frec (buffer-justification cursor))
    (setf (fr.numlines frec) 0)
    (let ((line-count (estimate-line-count frec)))
      (setf (fr.linevec frec)      (make-array line-count :initial-element 0))
      (setf (fr.lineascents frec)  (make-array line-count :initial-element 0))
      (setf (fr.linedescents frec) (make-array line-count :initial-element 0))
      (setf (fr.lineheights frec)  (make-array line-count :initial-element 0))
      (setf (fr.linewidths frec)   (make-array line-count :initial-element 0)))
    (when (setf (fr.line-right-p frec) (not (eql 0 (mclgui:get-sys-just))))      
      (setf (fr.right-justified-p frec) t)) 
    (setf (fr.leading frec) nil)       ; no leading
    (when owner
      (reinit-frec frec owner))     ; Push onto *frec-list*, allocate selrgn
    (niy make-frec cursor owner size)
    #-(and) (ccl::use-buffer cursor)                 ; increment buffer reference count
    frec))



(defmacro fr.flags (x)
  `(the fixnum (fr.flags-slot ,x)))

(defmacro deffrflag (name bit)
  (let ((setf-name (intern (concatenate 'string "SETF-" (symbol-name name)) *package*)))
    `(progn
       (defconstant ,name ,bit)
       (declaim (inline ,name ,setf-name))
       (defmacro ,name (frec) `(logbitp ,,bit (fr.flags ,frec)))
       (defmacro ,setf-name (frec value)
         (let ((frec-var (gensym))
               (value-var (gensym)))
           `(let ((,frec-var ,frec)
                  (,value-var (not (null ,value))))
              (setf (fr.flags ,frec-var)
                    (if ,value-var
                      (logior ,,(ash 1 bit) (fr.flags ,frec-var))
                      (logand ,,(lognot (ash 1 bit)) (fr.flags ,frec-var))))
              ,value-var)))
       (defsetf ,name ,setf-name))))


(deffrflag fr.bpchar-on-p  0)
(deffrflag fr.caret-on-p 1)
(deffrflag fr.frame-sel-p 2)
(deffrflag fr.wrap-p 3)
(deffrflag fr.framed-sel-p 4)
(deffrflag fr.bwin-cr-p 5)
(deffrflag fr.text-edit-sel-p 6)
(deffrflag fr.truetype-p 7)
(deffrflag fr.word-wrap-p 8)
(deffrflag fr.cursor-italic-p 9)
(deffrflag fr.zwin-return-p 10)         ; #\return at fr.zwin
(deffrflag fr.cursor-bol-p 11)          ; cursor at beginning of line.
(deffrflag fr.cursor-bol-p-valid 12)    ; set when frec-click sets fr.cursor-bol-p
                                        ; cleared by %frec-update-internal
(deffrflag fr.center-justified-p 13)
(deffrflag fr.right-justified-p 14)
(deffrflag fr.line-right-p 15)          ; right-to-left line direction

(deffrflag fr.nodrawing-p 27)
(deffrflag fr.buf-changed-p 26)
(deffrflag fr.curs-changed-p 25)
(deffrflag fr.sel-valid-p 24)
(deffrflag fr.changed-p 23)
(deffrflag fr.printing-p 22)  ; new 4/17


(defconstant $fr.flags_non-drawing-bits-mask #x1FFFFF)



(defun call-with-frec (frec thunk)
  (let ((frec-var (frec-arg frec)))
    (progn
      (niy call-with-frec frec thunk)
      #-(and)
      (rlet ((penstate :penstate))
            (unwind-protect
                 (progn
                   (#_GetPenState penstate)
                   (#_PenNormal)
                   (setf (fr.flags frec-var)
                         (logand $fr.flags_non-drawing-bits-mask
                                 (the fixnum (fr.flags frec-var))))
                   (funcall thunk frec-var))
              (#_SetPenState penstate))))))


(defmacro with-frec ((frec-var frec) &body body)
  `(call-with-frec ,frec (lambda (,frec-var) ,@body)))



;;;; THE END ;;;;

