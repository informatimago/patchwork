;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               kernel-menu-items.lisp
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
(in-package :pw)


(defun init-patch-pos (win patch)
  (let* ((wsize (if win
                    (subtract-points (view-size win)
                                     (view-size patch))
                    (make-point 10 10)))
         (ww (point-h wsize))
         (wh (point-v wsize)))
    (unless *position-new-box*
      (setf *position-new-box* (make-point (random ww) (random wh))))
    (let* ((x (point-h *position-new-box*))
           (y (point-v *position-new-box*)))
      (when win
        (setf x (min x ww)
              y (min y wh)))
      (set-view-position patch (make-point x y)))))

(defun new-PW-sub-menu-item (main-menu mtitle patch-type box-title type-forms
                             &optional type-list defaults-list)
  (let ((body 
          `(make-patch-box ',patch-type ',box-title ',type-forms ',type-list ',defaults-list))
        (sub-menu (find-menu-item main-menu mtitle)))
    (unless sub-menu
      (ui:add-menu-items main-menu
                         (setq sub-menu (make-instance 'menu-item :menu-item-title mtitle))))
    (push (eval `(function (lambda () ,body))) *PW-box-instance-list*)
    (set-menu-item-action-function sub-menu
                                   (lambda () (add-patch-box *active-patch-window* (eval body))))
    sub-menu))


;;Hardcopy printing
(defvar *PW-print-setUp* nil)
(defvar *print-PW-menu* nil)


(defun initialize-kernel-menu ()
  
  ;;(PW-addmenu-fun *PWoper-menu* 'config 'C-patch-configurer)

  ;;======================================================
  ;;   submenuitems for *pw-Data-menu*

  (PW-addmenu *pw-data-menu* '(numbox const evconst))

  (PW-addmenu-fun *pw-data-menu* 'C-patch-buffer:Buffer 'C-patch-buffer)

  (PW-addmenu-fun *pw-data-menu* 'C-patch-accum:accum 'C-patch-accum)

  (PW-addmenu-fun *pw-data-menu* 'lst-ed 'C-patch-list-editor:C-patch-list-editor)

  (PW-addmenu-fun *pw-data-menu* 'Text-win 'C-patch-ascii-buffer)

  ;;(PW-addmenu-fun *pw-data-menu* 'Ascii-win 'C-patch-ascii-buffer)

  ;;================================
  ;;Arithmetic

  #|(PW-addmenu *pw-Arith-menu*
  '(l+ l- l* l/ LL/round LL/floor LLdecimals LL/mod LL-log L-exp L-power LL-abs
  l-scale% x->dx dx->x random2 LLalea  LL-oper matrix-oper))|#

  (PW-addmenu *pw-Arith-menu*
              '(g+ g- g* g/ g-power g-exp g-log))

  (ui:add-menu-items *pw-Arith-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-Arith-menu* 
              '(g-div g-mod g-round g-floor g-ceiling g-abs))

  (ui:add-menu-items *pw-Arith-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-Arith-menu*  '(g-min g-max g-random g-average))

  (PW-addmenu *pw-Num-series-menu* '(arithm-ser geometric-ser fibo-ser))

  (ui:add-menu-items *pw-Num-series-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-Num-series-menu* '(g-scaling g-scaling/sum g-scaling/max interpolation 
                                     g-alea x->dx dx->x ))

  (ui:add-menu-items *pw-Num-series-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-Num-series-menu* '(prime-ser prime-factors prime?))

  (PW-addmenu *pw-Num-Fun-Gen-menu* 
              '(epw::make-num-fun sample-fun epw::lagrange-fun epw::linear-fun epw::power-fun))

  (ui:add-menu-items *pw-Num-Fun-Gen-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-Num-Fun-Gen-menu* '(g-oper cartesian inverse))
  ;; epw::parabole/2 epw::parabole/3))

  ;;====================
  ;;Control Modules

  (PW-addmenu-fun *pw-control-menu* 'circ 'C-pw-circ)

  ;;(PW-addmenu-fun *pw-control-menu* 'cirend 'C-pw-circ-end)

  (PW-addmenu-fun *pw-control-menu* 'ev-once 'C-clock-constant)

  (PW-addmenu-fun *pw-control-menu* 'pwrepeat 'C-pw-loop)


  (ui:add-menu-items  *pw-control-menu* 
                      (new-leafmenu "pwmap" 
                                    (lambda () 
                                      (let ((enum (make-PW-standard-box 'C-enum-collect-source 'enum))
                                            (loop (make-PW-standard-box 'C-map-first 'pwmap)))
                                        (init-patch-pos *active-patch-window* loop)
                                        (init-patch-pos *active-patch-window* enum)
                                        (setf *position-new-box* nil)
                                        (add-subviews *active-patch-window* enum loop)
                                        (set-view-position loop (make-point (x loop) (+ (y loop) 45)))
                                        (connect-ctrl loop (car (pw-controls loop)) enum)
                                        (setf (open-state (car (pw-controls loop)) ) nil)
                                        (record-patch "pwmap" (list (x loop) (y loop)) nil)
                                        (tell (controls *active-patch-window*) 'draw-connections)
                                        loop))))

  (ui:add-menu-items  *pw-control-menu* 
                      (new-leafmenu "pwreduce" 
                                    (lambda () 
                                      (let ((enum1 (make-PW-standard-box 'C-enum-collect-source 'enum))
                                            (enum2 (make-PW-standard-box 'C-enum-collect-source 'enum))
                                            (reduce (make-PW-standard-box 'C-reducer 'pwreduce)))
                                        (setf (pw-function-string enum1) "accum")
                                        (init-patch-pos *active-patch-window* enum1)
                                        (init-patch-pos *active-patch-window* enum2)
                                        (init-patch-pos *active-patch-window* reduce)
                                        (setf *position-new-box* nil)
                                        (add-subviews *active-patch-window* enum1 enum2 reduce)
                                        (set-view-position reduce (make-point (x reduce) (+ (y reduce) 90)))
                                        (set-view-position enum2 (make-point (x enum2) (+ (y enum2) 45)))
                                        (connect-ctrl reduce (car (pw-controls reduce)) enum1)
                                        (connect-ctrl reduce (nth 2 (pw-controls reduce)) enum2)
                                        (setf (open-state (car (pw-controls reduce))) nil)
                                        (setf (open-state (nth 2 (pw-controls reduce))) nil)
                                        (record-patch "pwreduce" (list (x reduce) (y reduce)) nil)
                                        (tell (controls *active-patch-window*) 'draw-connections)
                                        reduce))))

  (PW-addmenu-fun *pw-control-menu* 'test 'C-pw-test)

  (PW-addmenu *pw-control-menu* '(trigger))

;;;==============================
;;; List items

  (PW-addmenu *pw-usual-lisp-menu* '(first rest butlast reverse length mapcar list 
                                     apply funcall remove))

  (PW-addmenu *pw-List-menu*
              '(posn-match last-elem x-append))

  (ui:add-menu-items *pw-List-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-List-menu* '(flat flat-once flat-low ))

  (ui:add-menu-items *pw-List-menu* 
                     (new-leafmenu "-" ()))

  (PW-addmenu *pw-List-menu* '(create-list expand-lst rem-dups list-modulo list-explode 
                               mat-trans list-filter table-filter range-filter band-filter))

  #|(ui:add-menu-items *pw-List-menu* 
  (new-leafmenu "-" ())
  *pw-set-menu* *pw-list-gen-menu* *pw-list-trans-menu*
  *pw-list-combin-menu*)|#

  (PW-addmenu *pw-set-menu* '(x-union x-intersect x-Xor x-diff included?))

  ;;(PW-addmenu *pw-list-gen-menu* '(create-list expand-lst))

  #|(PW-addmenu *pw-list-trans-menu*
  '(rem-dups list-modulo list-explode mat-trans list-filter 
  range-filter band-filter))|#

  (PW-addmenu *pw-list-combin-menu* '(sort-list posn-order permut-random
                                      permut-circ nth-random))

  ;;===============================
  ;; Abstraction in-out

  (PW-addmenu-fun *pw-Abs-menu* 'absout 'C-abstract-out)

  (PW-addmenu-fun *pw-Abs-menu* 'absin 'C-abstract-in)

;;;=============================
;;;BPF items

  ;;(PW-addmenu-fun *pw-BPF-menu* 'bpf  'C-patch-function)
  (PW-addmenu-fun *pw-BPF-menu* 'multi-bpf  'C-patch-multi-function)

  ;;(PW-addmenu-fun *pw-BPF-menu* 'env  'C-patch-env)

  ;;(PW-addmenu-fun *pw-BPF-menu* 'transfer  'C-patch-transfer)
  (PW-addmenu *pw-BPF-menu* '(transfer bpf-sample))

  ;;(PW-addmenu-fun *pw-BPF-menu* 'osc  'C-patch-osc)

  ;;(PW-addmenu-fun *pw-BPF-menu* 'osc-period  'C-patch-osc-period)

  ;;(PW-addmenu-fun *pw-BPF-menu* 'oscil-phase  'C-patch-osc-phase)

  (PW-addmenu-fun *pw-BPF-menu* 'bpf-lib  'C-patch-bpf-lib)

  ;;(ui:add-menu-items *pw-BPF-menu* 
  ;;                (new-leafmenu "-" ()))

  ;;(PW-addmenu-fun *pw-BPF-menu* 'points-view 'C-pw-points-view)

  ;;(push-to-object-types 'bpf)
;;;=========================
;;; Vision Items

  ;;(PW-addmenu-fun *pw-Vision-menu* 'oscilloscope 'C-pw-oscilloscope)

  ;;(PW-addmenu-fun *pw-Vision-menu* 'points-view 'C-pw-points-view)

;;;============================
;;; Extern Items
  (pw-addmenu-fun *pw-Extern-menu* 'in 'C-pw-in)

  (pw-addmenu-fun *pw-Extern-menu* 'out 'C-pw-out)


  ;;===========================
  ;;Multidim Objects

  ;;(PW-addmenu-fun *pw-Multidim-menu* 'get-slot 'C-patch-MD-get-slot)

  (PW-addmenu *pw-Multidim-menu* '(get-slot set-slot))

  ;;(PW-addmenu-fun *pw-Multidim-menu* 'set-slot 'C-patch-MD-set-slot)

  ;;(PW-addmenu *pw-Multidim-menu* '(send))


  (setf *PW-print-setUp*
        (new-leafmenu "Page Setup…" (lambda () (win-print-setUp *active-patch-window*))))

  (setf *print-PW-menu* 
        (new-leafmenu "Print…" (lambda () (window-hardcopy  *active-patch-window*))))

  (ui:add-menu-items *pw-menu-file* *PW-print-setUp* *print-PW-menu*))

;;;; THE END ;;;;
