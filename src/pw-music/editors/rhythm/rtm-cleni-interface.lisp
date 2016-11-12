;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rtm-cleni-interface.lisp
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
(in-package :PW)

;; ==============================================

(defvar *rtm-cleni-score*                 '())
(defvar *cleni-notehead-list*             '())
(defvar *cleni-previous-chord-info-list*  '())
(defvar *cleni-previous-obj-info-list*    '())

;; ==============================================
(defun convert-rtm-unit-to-cleni (unit dots)
  (let ((new-unit
         (case unit
           (-2   1/1)
           (-1   1/2)
           (0   1/4)
           (1   1/8)
           (2   1/16)
           (3   1/32)
           (4   1/64)
           (5   1/128))))
  (case dots
    (dot  (+ new-unit (* 1/2 new-unit)))
    (double-dot  (+ new-unit  (* 3/4 new-unit)))
    (t new-unit))))

;; ==============================================
(defvar *cleni-temperament-mode* 2)

#|(defmethod write-cleni-measure-line ((self C-measure-line) staff)
   (setf *cleni-notehead-list* nil)
   (let ((measures (measures self)))
      (for (i 0 1 (1- (length measures)))
        (write-cleni-measure (nth i measures) (1+ i)))
  `(cleni:describe-score *rtm-cleni-score*  :temperament *cleni-temperament-mode*
           :staff ,staff
             ,@(apply #'append (nreverse *cleni-notehead-list*)))))|#

(defgeneric write-cleni-measure-line (self staff)
  (:method ((self C-measure-line) staff)
    (setf *cleni-notehead-list* nil)
    (setq *cleni-previous-obj-info-list* nil)
    (let ((measures (measures self)))
      (for (i 0 1 (1- (length measures)))
        (write-cleni-measure (nth i measures) (1+ i)))
      (cleni:describe-score *rtm-cleni-score*  :temperament *cleni-temperament-mode*
                                               :staff staff)
      (mapc (lambda (specs)
                                        ;(format t "~D " (incf *counter*))
              (apply #'cleni:describe-score *rtm-cleni-score* specs))
            (nreverse *cleni-notehead-list*)))))

(defgeneric write-cleni-measure (self mcount)
  (:method ((self C-measure) mcount)
    (let ((beats (beat-objects  self)))
      (push `(:measure ,mcount  :signature (,(calc-high-value self) ,(read-from-string (low self))))
            *cleni-notehead-list*)
      (while beats
        (write-cleni-beat (pop beats) self (give-low-number (read-from-string (low self))))))))

;;  :chord 1/4 '(c5 g5)
;;  :rest 1/8
;; note rest slur

(defun convert-note-to-cleani-symbol (note)
 (let ((diatone (diatone note))
       (alteration (alteration note)))
;;  (print (list diatone alteration))
  (read-from-string
    (concatenate 'string
      (string (case (mod diatone 7)
                (0 'c)(1 'd)(2 'e)(3 'f)(4 'g)(5 'a)(6 'b)))
      (format nil "~A" (1- (truncate diatone 7)))
      (case alteration
        (#\Y "s")(#\I "b")(#\y "+")(t ""))))))

;;(convert-note-to-cleani-symbol (make-C-note 6100 36 #\I 100 120 1))

#|
(defmethod write-cleni-beat  ((self C-beat) super-beat low)
  (let* ((rtm-list (rtm-list self))
         (rtm-sum (round (apply #'+ (mapcar #'abs (ask-all (rtm-list self) 'unit-length)))))
         (tuplet-flag (draw-tuplets? rtm-sum (unit-length self)))
         rtm-now rtm-obj note-head-info tuplet-num cleni-tuplet-unit)
     (when (eql (class-name  (class-of  super-beat)) 'C-measure)
         (setf *global-rtm-level-list* nil)
         (setf *notehead-type-list* nil))
     (when tuplet-flag
         (setq tuplet-num (give-tuplet-num2 rtm-sum (unit-length self)))
         (calc-next-note-head  (unit-length self) rtm-sum 1 low)
         (setq cleni-tuplet-unit (convert-rtm-unit-to-cleni (car *global-rtm-level-list*) nil))
         (push
           `(:open-tuplet ,rtm-sum ,cleni-tuplet-unit ,tuplet-num ,cleni-tuplet-unit)
            *cleni-notehead-list*))
     (while rtm-list
      (setq rtm-obj (pop rtm-list))
      (setq rtm-now (unit-length rtm-obj))
      (if (no-beat-leaf? rtm-obj)
        (write-cleni-beat rtm-obj self (+ (calc-next-note-level (unit-length self) rtm-sum) low))
        (progn
            (setq note-head-info
               (calc-next-note-head  (unit-length self) rtm-sum rtm-now low))
;;            (print (list (car *notehead-type-list*) note-head-info))
            (cond
              ((beat-chord rtm-obj)
                (push
                  (setq *cleni-previous-chord-info-list*
                    `(:chord ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info))
                           ',(mapcar #'convert-note-to-cleani-symbol (notes (beat-chord rtm-obj)))))
                           ;;(ask-all (notes (beat-chord rtm-obj)) 'midic)
                 *cleni-notehead-list*))
              ((eql (car *notehead-type-list*) 'rest)
                (push
                  `(:rest ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info)))
                  *cleni-notehead-list*))
              ((floatp rtm-now)              ;slur
                 (nconc *cleni-previous-chord-info-list*
                       `(:tie ',(make-list (length (eval (third *cleni-previous-chord-info-list*))) :initial-element t)))
                 (push
                      `(:chord
                          ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info))
                          ,(third *cleni-previous-chord-info-list*))
                       *cleni-notehead-list*)) )
              (when (eql (second note-head-info) 'slur)
                 (setq *cleni-notehead-list*
                   (cons
                     (append (car *cleni-notehead-list*)
                          `(:tie ',(make-list (length (eval (third *cleni-previous-chord-info-list*))) :initial-element t)))
                     (cdr *cleni-notehead-list*)))
                 (push
                   `(:chord  ,(convert-rtm-unit-to-cleni (+ (car *global-rtm-level-list*) 2) nil)
                             ,(third *cleni-previous-chord-info-list*))
                    *cleni-notehead-list*))
                 )))
       (when tuplet-flag
         (push
           `(:close-tuplet )
            *cleni-notehead-list*))
      ))


(defmethod write-cleni-beat  ((self C-beat) super-beat low)
  (let* ((rtm-list (rtm-list self))
         (rtm-sum (round (apply #'+ (mapcar #'abs (ask-all (rtm-list self) 'unit-length)))))
         (tuplet-flag (draw-tuplets? rtm-sum (unit-length self)))
         rtm-now rtm-obj note-head-info tuplet-num cleni-tuplet-unit
         )
    (when (eql (class-name  (class-of  super-beat)) 'C-measure)
      (setf *global-rtm-level-list* nil)
      (setf *notehead-type-list* nil))
    (when tuplet-flag
      (setq tuplet-num (give-tuplet-num2 rtm-sum (unit-length self)))
      (calc-next-note-head  (unit-length self) rtm-sum 1 low)
      (setq cleni-tuplet-unit (convert-rtm-unit-to-cleni (car *global-rtm-level-list*) nil))
      (push
       `(:open-tuplet ,rtm-sum ,cleni-tuplet-unit ,tuplet-num ,cleni-tuplet-unit)
       *cleni-notehead-list*))
    (while rtm-list ;(print *cleni-notehead-list*)
      (setq rtm-obj (pop rtm-list))
      (setq rtm-now (unit-length rtm-obj))
      (if (no-beat-leaf? rtm-obj)
        (write-cleni-beat rtm-obj self (+ (calc-next-note-level (unit-length self) rtm-sum) low))
        (progn
          (setq note-head-info
                (calc-next-note-head  (unit-length self) rtm-sum rtm-now low))
          ;(print (list (car *notehead-type-list*) note-head-info))
          (cond
           ((beat-chord rtm-obj)
            (push
             (setq *cleni-previous-obj-info-list*
                   (setq *cleni-previous-chord-info-list*
                         `(:chord ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info))
                                  ,(ask-all (notes (beat-chord rtm-obj)) 'midic))))
             *cleni-notehead-list*))
           ((eql (car *notehead-type-list*) 'rest)
            (push
             `(:rest ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info)))
             *cleni-notehead-list*)
            (setq *cleni-previous-obj-info-list* ()))
           ((floatp rtm-now)              ;slur
            (when *cleni-previous-obj-info-list*
              (nconc *cleni-previous-obj-info-list*
                     `(:tie ',(make-list (length  (third *cleni-previous-chord-info-list*)) :initial-element t))))
            (push
             (setq  *cleni-previous-obj-info-list*
                    `(:chord
                      ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info))
                      ,(third *cleni-previous-chord-info-list*)))
             *cleni-notehead-list*)) )
          (when (eql (second note-head-info) 'slur) (print 'slur)
                (setq *cleni-notehead-list*
                      (cons
                       (append (car *cleni-notehead-list*)
                               `(:tie ,(make-list (length  (third *cleni-previous-chord-info-list*)) :initial-element t)))
                       (cdr *cleni-notehead-list*)))
                (push
                 `(:chord  ,(convert-rtm-unit-to-cleni (+ (car *global-rtm-level-list*) 2) nil)
                           ,(third *cleni-previous-chord-info-list*))
                 *cleni-notehead-list*))
          )))
    (when tuplet-flag
      (push
       `(:close-tuplet )
       *cleni-notehead-list*))
    ))
|#

;;changed by aaa from pw-modif
;;modify by GAS
(defgeneric write-cleni-beat (self super-beat low)
  (:method ((self C-beat) super-beat low)
    (let* ((rtm-list (rtm-list self))
           (rtm-sum (round (apply #'+ (mapcar #'abs (ask-all (rtm-list self) 'unit-length)))))
           (tuplet-flag (draw-tuplets? rtm-sum (unit-length self)))
           rtm-now rtm-obj note-head-info tuplet-num cleni-tuplet-unit
           )
      (when (eql (class-name  (class-of  super-beat)) 'C-measure)
        (setf *global-rtm-level-list* nil)
        (setf *notehead-type-list* nil))
      (when tuplet-flag
        (setq tuplet-num (give-tuplet-num2 rtm-sum (unit-length self)))
        (calc-next-note-head  (unit-length self) rtm-sum 1 low)
        (setq cleni-tuplet-unit (convert-rtm-unit-to-cleni (car *global-rtm-level-list*) nil))
        (push
         `(:open-tuplet ,rtm-sum ,cleni-tuplet-unit ,tuplet-num ,cleni-tuplet-unit)
         *cleni-notehead-list*))
      (while rtm-list                   ;(print *cleni-notehead-list*)
        (setq rtm-obj (pop rtm-list))
        (setq rtm-now (unit-length rtm-obj))
        (if (no-beat-leaf? rtm-obj)
            (write-cleni-beat rtm-obj self (+ (calc-next-note-level (unit-length self) rtm-sum) low))
            (progn
              (setq note-head-info
                    (calc-next-note-head  (unit-length self) rtm-sum rtm-now low))
                                        ;(print (list (car *notehead-type-list*) note-head-info))
              (cond
                ((beat-chord rtm-obj)
                 (push
                  (setq *cleni-previous-obj-info-list*
                        (setq *cleni-previous-chord-info-list*
                              `(:chord ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info))
;;; GA 29/9/95 add approx-m
                                       ,(epw::approx-m (ask-all (notes (beat-chord rtm-obj)) 'midic)
                                                       *cleni-temperament-mode*))))
                  *cleni-notehead-list*))
                ((eql (car *notehead-type-list*) 'rest)
                 (push
                  `(:rest ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info)))
                  *cleni-notehead-list*)
                 (setq *cleni-previous-obj-info-list* ()))
                ((floatp rtm-now)       ;slur
                 (when *cleni-previous-obj-info-list*
                   (nconc *cleni-previous-obj-info-list*
                          `(:tie ',(make-list (length  (third *cleni-previous-chord-info-list*)) :initial-element t))))
                 (push
                  (setq  *cleni-previous-obj-info-list*
                         `(:chord
                           ,(convert-rtm-unit-to-cleni (car *global-rtm-level-list*)(second note-head-info))
                           ,(third *cleni-previous-chord-info-list*)))
                  *cleni-notehead-list*)) )
              (when (eql (second note-head-info) 'slur)
                (cond ((eql (caar *cleni-notehead-list*) :rest)
                       (push
                        `(:rest  ,(convert-rtm-unit-to-cleni (+ (car *global-rtm-level-list*) 2) nil))
                        *cleni-notehead-list*))
                      (t
                       (setq *cleni-notehead-list*
                             (cons
                              (append (car *cleni-notehead-list*)
                                      `(:tie ,(make-list (length  (third *cleni-previous-chord-info-list*)) :initial-element t)))
                              (cdr *cleni-notehead-list*)))
                       (push
                        `(:chord  ,(convert-rtm-unit-to-cleni (+ (car *global-rtm-level-list*) 2) nil)
                                  ,(third *cleni-previous-chord-info-list*))
                        *cleni-notehead-list*))))
              )))
      (when tuplet-flag
        (push
         `(:close-tuplet )
         *cleni-notehead-list*))
      )))

;;(length (eval (third *cleni-previous-chord-info-list*)))
;;(length '(6000))
;;==========================================

;;(write-cleni-measure-line (measure-line (car (beat-editors (editor-collection-object *active-rtm-window*)))) 1)

(defun calc-cleni-rtm-score (win)
  (setf *rtm-cleni-score* (cleni:new-score))
  (let ((measure-lines (ask-all (beat-editors (editor-collection-object win)) #'measure-line)))
    (for (i 0 1 (1- (length measure-lines)))
      (write-cleni-measure-line (nth i measure-lines) (1+ i)))))

;;(calc-cleni-rtm-score *active-rtm-window*)
;;(cleni:translate-score *rtm-cleni-score* "root;rtm-test")

(defun save-cleni-rtm-score ()
  (let ((new-name (choose-new-file-dialog
             :directory (window-title *active-rtm-window*)
             :prompt "Save RTM As…")))
     (calc-cleni-rtm-score *active-rtm-window*)
     (cleni:translate-score *rtm-cleni-score* new-name)))

;;(save-cleni-rtm-score)

;;;; THE END ;;;;
