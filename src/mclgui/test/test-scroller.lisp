
(require 'quickdraw)


;;;;;;;;;;;;;;;;;;;;;;
;;
;;  a dialog with a scroller in it
;;

(setq foo (make-instance 'dialog))

(defclass scroller1 (scroller) ())

(defmethod scroll-bar-limits ((view scroller1))
  (normal-scroll-bar-limits view 200 200))

(defmethod view-draw-contents ((self scroller1))
  (frame-rect self 10 10 50 50)
  (paint-oval self 30 30 200 200)
  (erase-oval self 30 30 70 70)
  (call-next-method))

(setq bar (make-instance 'scroller1
                 :view-container foo
                 :view-size #@(125 125)
                 :track-thumb-p t))

(set-view-position bar 30 30)
(set-view-position bar 00 00)
(set-view-position bar 05 05)

(set-view-size bar 150 150)

; How to make the same thing with a scroller-pane 
(setq pane (make-instance 'scroller-pane
             :scroller-class 'scroller1
             :view-size #@(125 125)
             :view-position #@(150 0)
             :track-thumb-p t
             :view-container foo))

;;;;;;;;;;;;;;;;;;;;;;
;;
;;  nested scrollers
;;

(setq dial (make-instance 'dialog))

(defclass scroller2 (scroller) ())

(defmethod scroll-bar-limits ((view scroller2))
  (normal-scroll-bar-limits view 300 300))

(defmethod view-draw-contents ((self scroller2))
  (frame-rect self 110 10 170 170)
  (call-next-method))

(setq first-scroller (make-instance 'scroller2
                            :view-container dial
                            :view-size #@(180 180)
                            :view-position #@(5 5)
                            :track-thumb-p t))


(defclass scroller3 (scroller) ())

(defmethod scroll-bar-limits ((view scroller3))
  (normal-scroll-bar-limits view 170 170))

(defmethod view-draw-contents ((self scroller3))
  (paint-oval self 10 10 70 70)
  (paint-oval self 70 70 170 170)
  (call-next-method))

(setq second-scroller (make-instance 'scroller3
                             :view-container first-scroller
                             :view-size #@(75 155)
                             :view-position #@(10 10)
                             :track-thumb-p t))


;;;;;;;;;;;;;;;;;;;;;;
;;
;;  scrollers with only one scroll bar
;;

(setq foo1 (make-instance 'dialog))

(defclass scroller4 (scroller) ())

(defmethod scroll-bar-limits ((view scroller4))
  (normal-scroll-bar-limits view 200 200))

(defmethod view-draw-contents ((self scroller4))
  (frame-rect self 10 10 50 50)
  (paint-oval self 30 30 200 200)
  (erase-oval self 30 30 70 70)
  (call-next-method))

(setq bar1 (make-instance 'scroller4 :grow-icon-p t
                 :view-container foo1
                 :h-scrollp nil))

(set-view-position bar1 50 50)
(set-view-size bar1 150 150)



(setq foo2 (make-instance 'dialog))
(setq bar2 (make-instance 'scroller4
                 :view-container foo2
                 :v-scrollp nil))

(set-view-position bar2 50 50)
(set-view-size bar2 125 125)

