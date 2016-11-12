;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               midi-files.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCL User Interface Classes
;;;;DESCRIPTION
;;;;  
;;;;    XXX
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
(in-package :PW)

(defun variable-length-midi-time (ticks)
  (let ((high-byte1 0)(high-byte2 0)(high-byte3 0)
        (mod-ticks (mod ticks #x80)))
    (cond 
      ((> ticks #x1FFFFF)
       (setq high-byte3 (+ #x80 (truncate (/ ticks #x200000))))
       (setq high-byte2 (+ #x80 mod-ticks))
       (setq high-byte1 (+ #x80 mod-ticks))
       (setq ticks mod-ticks)
       (list high-byte3 high-byte2 high-byte1 ticks))
      ((> ticks #x3FFF)
       (setq high-byte2 (+ #x80 (truncate (/ ticks #x4000))))
       (setq high-byte1 (+ #x80 mod-ticks))
       (setq ticks mod-ticks)
       (list high-byte2 high-byte1 ticks))
      ((> ticks #x7F)
       (setq high-byte1 (+ #x80 (truncate (/ ticks #x80))))
       (setq ticks mod-ticks)
       (list high-byte1 ticks))
      (t (list ticks)))
    ;;    (setq *print-base* 16)
    ;;    (print (list high-byte3 high-byte2 high-byte1 ticks))
    ;;    (setq *print-base* 10) 
    ))

;;(variable-length-midi-time #x64)

(defun covert-length-to-4-byte-list (len)
  (let ((byte1 0)(byte2 0)(byte3 0)(byte4 0))
    (cond 
      ((> len #xFFFFFF)
       (setq byte4 (truncate (/ len #x1000000)))
       (setq byte3 (mod len #x1000000))
       (setq byte3 (mod len #x10000))
       (setq byte2 (mod len #x10000))
       (setq byte1 (mod len #x100)))
      ((> len #xFFFF)
       (setq byte3 (truncate (/ len #x10000)))
       (setq byte2 (mod len #x10000))
       (setq byte1 (mod len #x100)))
      ((> len #xFF)
       (setq byte2 (truncate (/ len #x100)))
       (setq byte1 (mod len #x100)))
      (t  (setq byte1 len)))
    (list byte4 byte3 byte2 byte1)))



;;(covert-length-to-4-byte-list 700)

;;============================================================

(defun  make-variable-length-midi-delta-times  (midi-list)
  (let ((res)(time-now 0))
    (while midi-list
      (push (append (variable-length-midi-time (- (caar midi-list) time-now))
                    (cdar midi-list)) res)
      (setq time-now (caar midi-list))
      (pop midi-list))
    (apply 'append (nreverse res))))

#|
(defun  make-midi-file-list  (chords &optional rtm-fl)
(let ((t-time)(notes)(midi-list))
(while chords
(setq t-time (t-time (car chords)))
(setq notes (notes (car chords)))
(while notes
(push (list 
t-time
(+ #x8f (chan (car notes)))
(truncate (/ (midic (car notes)) 100))
(vel (car notes)))
midi-list)
(push (list 
(+ t-time (dur (car notes)))
(+ #x8f (chan (car notes)))
(truncate (/ (midic (car notes)) 100))
0)
midi-list)
(pop notes))
(pop chords))
(if rtm-fl 
(nreverse midi-list)
(make-variable-length-midi-delta-times
(sort (nreverse midi-list) '< :key (lambda (a) (car a)))))))
|#

(defun  make-midi-file-list  (chords &optional rtm-fl)
  (let ((t-time)(notes)(midi-list))
    (while chords
      (setq t-time (t-time (car chords)))
      (setq notes (notes (car chords)))
      (while notes
        (push (list 
               (round t-time (/ 100 96)) ;;; GA 2/10/95
               (+ #x8f (chan (car notes)))
               (truncate (/ (epw::approx-m (midic (car notes)) 2) 100)) ;;;  GA 2/10/95
               (vel (car notes)))
              midi-list)
        (push (list 
               (round (+ t-time (dur (car notes))) (/ 100 96))  ;;; GA 2/10/95
               (+ #x8f (chan (car notes)))
               (truncate (/ (epw::approx-m (midic (car notes)) 2) 100))  ;;; GA 2/10/95
               0)
              midi-list)
        (pop notes))
      (pop chords))
    (if rtm-fl 
        (nreverse midi-list)
        (make-variable-length-midi-delta-times
         (sort (nreverse midi-list) '< :key (lambda (a) (car a)))))))


(defun make-midi-file-0 (chords)
  (let ((data (make-midi-file-list chords))
        (track-info
          '(#x00 #xff #x58 #x04 #x04 #x02 #x18 #x08
            #x00 #xff #x51 #x03 #x07 #xa1 #x20))
        (track-end '(#x0 #xff #x2f #x0)))
    (append   
     '(#x4D #x54 #x68 #x64 
       #x00 #x00 #x00 #x06
       #x00 #x00 
       #x00 #x01
       #x00 #x60
     
       #x4D #x54 #x72 #x6B) 
     (covert-length-to-4-byte-list (+ (length track-info)(length data)(length track-end)))
     track-info
     data
     track-end))) 

(defun make-midi-file-1 (chords-list)
  (let ((data-list (mapcar #'make-midi-file-list chords-list))
        (track-info
          '(#x00 #xff #x58 #x04 #x04 #x02 #x18 #x08
            #x00 #xff #x51 #x03 #x07 #xa1 #x20))
        (track-end '(#x0 #xff #x2f #x0))
        (tracks)(track-count (length chords-list)))
    (while data-list
      (push
       (append   
        '(#x4D #x54 #x72 #x6B) 
        (covert-length-to-4-byte-list 
         (+ (length track-info)(length (car data-list))(length track-end)))
        track-info
        (pop data-list)
        track-end)
       tracks)) 
    (setq tracks (nreverse tracks))
    (append   
     '(#x4D #x54 #x68 #x64 
       #x00 #x00 #x00 #x06
       #x00 #x01 
       #x00)
     (list track-count)
     '(#x00 #x60)
     (apply 'append tracks))))

;; (progn
;;   (setq *print-base* 16)
;;   (print (length (make-midi-file-0
;;                   (chords (chord-line (car (editor-objects (editor-view-object *active-MN-window*))))))))
;;   (setq *print-base* 10))


;;======================================================


(defun PW-midi-file-SAVE ()
  (let ((editor-view (editor-view-object *active-MN-window*))
        midi-data-list new-name)
    (setq midi-data-list
          (if (monofonic-mn? editor-view)
              (make-midi-file-0
               (chords (chord-line (car (editor-objects editor-view)))))
              (make-midi-file-1
               (ask-all (ask-all (editor-objects editor-view) 'chord-line) 'chords))))
    (setq new-name (choose-new-file-dialog :directory "MIDI FILE" :prompt "Save Midi file As…"))
    ;;   (setq new-file (CREATE-FILE  new-name))
    (delete-file new-name)
    (WITH-OPEN-FILE
        (out new-name :direction :output  :if-does-not-exist :create :if-exists :supersede
                      :element-type 'unsigned-byte)
      (while midi-data-list
        (write-byte  (pop midi-data-list) out)))
    (set-mac-file-type new-name "Midi")))
;;=======================================================

;; (setq m1
;;       '(
;;         #x4D #x54 #x68 #x64 
;;         #x00 #x00 #x00 #x06
;;         #x00 #x00 
;;         #x00 #x01
;;         #x00 #x60
;;       
;;         #x4D #x54 #x72 #x6B 
;;         #x00 #x00 #x00 #x3B
;; 
;;         #x00 #xff #x58 #x04 #x04 #x02 #x18 #x08
;;         #x00 #xff #x51 #x03 #x07 #xa1 #x20 
;;         #x00 #xc0 #x05 
;;         #x00 #xc1 #x2e 
;;         #x00 #xc2 #x46 
;;         #x00 #x92 #x30 #x60 
;;         #x00 #x3c #x60 
;;         #x60 #x91 #x43 #x40  
;;         #x60 #x90 #x4c #x20  
;;         #x81 #x40 #x82 #x30 #x40  
;;         #x00 #x3c #x40  
;;         #x00 #x81 #x43 #x40   
;;         #x00 #x80 #x4c #x40   
;;         #x00 #xff #x2f #x00
;;         ))

;;;; THE END ;;;;
