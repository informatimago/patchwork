;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-midi-files.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
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
;;;;    
;;;; -*- mode:lisp; coding:utf-8 -*-
(in-package :PW)

;;===========================================================

(defun  make-measure-midi-file-list  (midi-data measures)
  (let ((time-list (cumul-diff-lst-sum-from-0 (ask-all measures 'calc-measure-length 1 t))) 
        midi-list)
    (while measures
      (push (list 
                 (pop time-list) #xff #x58 #x04
                 (read-from-string (high (car measures)))
                 (case (read-from-string (low (car measures)))
                     (1 0)(2 1)(4 2)(8 3)(16 4)(32 5))
                 24 8)  ;24 8
              midi-list)
      (pop measures))
     (make-variable-length-midi-delta-times
       (sort (append (nreverse midi-list) midi-data) '< :key (lambda (a) (car a))))))

;;===========================================================

(defun make-rtm-midi-file-0 (measure-line)
  (let* ((data (make-midi-file-list (ask-all (collect-all-chord-beat-leafs measure-line) #'beat-chord) t)) 
;;         (high (read-from-string (high (car (measures measure-line)))))
;;         (low (case (read-from-string (low (car (measures measure-line))))
;;                 (1 0)(2 1)(4 2)(8 3)(16 4)(32 5)))
;;         (track-info
;;          (list #x00 #xff #x58 #x04  high low  #x18 #x08
;;                #x00 #xff #x51 #x03 #x07 #xa1 #x20))
        (track-end '(#x0 #xff #x2f #x0)))
     (setq data (make-measure-midi-file-list data (measures measure-line)))
    (append     
     '(#x4D #x54 #x68 #x64 
       #x00 #x00 #x00 #x06  
       #x00 #x00   
       #x00 #x01
       #x00 #x60
 
      #x4D #x54 #x72 #x6B) 
      (covert-length-to-4-byte-list (+ (length data)(length track-end)))
;;      track-info
      data
      track-end))) 

(defun RTM-midi-file-SAVE ()
 (let ((editor-view (editor-collection-object *active-RTM-window*))
         midi-data-list new-name)
   (tell (ask-all (beat-editors editor-view) 'measure-line) 'calc-t-time-measure-line 1)
   (setq midi-data-list
;;     (if (monofonic-mn? editor-view)
      (make-rtm-midi-file-0 (measure-line (car (beat-editors editor-view)))))
;;      (make-midi-file-1
;;         (ask-all (ask-all (editor-objects editor-view) 'chord-line) 'chords))))
   (setq new-name (choose-new-file-dialog :directory "MIDI FILE" :prompt "Save Midi file As…"))
   (delete-file new-name)  
   (WITH-OPEN-FILE  
       (out new-name :direction :output  :if-does-not-exist :create :if-exists :supersede
         :element-type 'unsigned-byte)
     (while midi-data-list
       (write-byte  (pop midi-data-list) out)))
   (set-mac-file-type new-name "Midi")))
