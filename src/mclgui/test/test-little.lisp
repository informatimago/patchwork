
(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)

(defparameter yes 1)
(defparameter no  0)


@[NSView subClass:LittleView
         slots:((pos :initform (make-point 0 0)
                     :accessor little-pos)
                (vel :initform (make-point (random 10) (random 10))
                     :accessor little-vel))]


;; (defclass lv ()
;;   ((pos :initform (make-point 0 0)
;;         :accessor little-pos)
;;    (vel :initform (make-point (random 10) (random 10))
;;         :accessor little-vel)))
;; (defmethod print-object ((self lv) stream)
;;   (declare (stepper disable))
;;   (print-unreadable-object (self stream :identity nil :type nil)
;;     (prin1 (list (point-to-list (little-pos self))
;;                  (point-to-list (little-vel self)))
;;            stream))
;;   self)
;; 
;; (defmethod update-ball ((self lv) r)
;;   (setf (little-pos self) (add-points (little-pos self) (little-vel self)))
;;   (when (and (< (point-h (little-pos self)) (nsrect-x r))
;;              (minusp (point-h (little-vel self))))
;;     (setf  (little-vel self) (make-point (- (point-h (little-vel self)))
;;                                          (point-v (little-vel self)))))
;;   (when (and (< (+ (nsrect-x r) (nsrect-width r)) (point-h (little-pos self)))
;;              (plusp (point-h (little-vel self))))
;;     (setf (little-vel self) (make-point (- (point-h (little-vel self)))
;;                                         (point-v (little-vel self)))))
;;   (when (and (< (point-v (little-pos self)) (nsrect-y r))
;;              (minusp (point-v (little-vel self))))
;;     (setf (little-vel self) (make-point (point-h (little-vel self))
;;                                         (- (point-v (little-vel self))))))
;;   (when (and (< (+ (nsrect-y r) (nsrect-height r)) (point-v (little-pos self)))
;;              (plusp (point-v (little-vel self))))
;;     (setf (little-vel self) (make-point (point-h (little-vel self))
;;                                         (- (point-v (little-vel self)))))))
;; 
;; (let ((lv (make-instance 'lv)))
;;   (loop repeat 100 do
;;    (print lv)
;;    (update-ball lv (make-nsrect :x 10 :y 10 :width 80 :height 80))))


@[LittleView method: (drawRect:(:<nsr>ect)rect)
             resultType:(:void)
             body:
             (declare (ignore rect))
             (let ((r (get-nsrect [self bounds])))
               (erase-rect* (nsrect-x      r)
                           (nsrect-y      r) 
                           (nsrect-width  r) 
                           (nsrect-height r) )
               (frame-rect* (+ (nsrect-x      r) 4)
                            (+ (nsrect-y      r) 4)
                            (- (nsrect-width  r) 8)
                            (- (nsrect-height r) 8))
               
               (fill-ellipse (- (point-h (little-pos self)) 3)
                             (- (point-v (little-pos self)) 3)
                             6 6)
               (setf (little-pos self) (add-points (little-pos self) (little-vel self)))
               (when (and (< (point-h (little-pos self)) (nsrect-x r))
                          (minusp (point-h (little-vel self))))
                 (setf  (little-vel self) (make-point (- (point-h (little-vel self)))
                                                      (point-v (little-vel self)))))
               (when (and (< (+ (nsrect-x r) (nsrect-width r)) (point-h (little-pos self)))
                          (plusp (point-h (little-vel self))))
                 (setf (little-vel self) (make-point (- (point-h (little-vel self)))
                                                     (point-v (little-vel self)))))
               (when (and (< (point-v (little-pos self)) (nsrect-y r))
                          (minusp (point-v (little-vel self))))
                 (setf (little-vel self) (make-point (point-h (little-vel self))
                                                     (- (point-v (little-vel self))))))
               (when (and (< (+ (nsrect-y r) (nsrect-height r)) (point-v (little-pos self)))
                          (plusp (point-v (little-vel self))))
                 (setf (little-vel self) (make-point (point-h (little-vel self))
                                                     (- (point-v (little-vel self)))))))]

@[NSWindow subClass:LittleWindow
           slots: (scroll-view
                   text-view
                   my-little-view)]

@[LittleWindow method: (initWithContentRect:(:<NSR>ect)contentrect
                                            styleMask:(:int)astyle
                                            backing:(:<NSB>acking<S>tore<T>ype)bufferingtype 
                                            defer:(:<BOOL>)flag)
               resultType:(:id)
               body:
               (setf self [super initWithContentRect:contentrect
                                 styleMask:astyle
                                 backing:bufferingtype
                                 defer:flag])
               (unless (nullp self)
                 (with-slots (scroll-view text-view my-little-view) self
                   (let* ((x      (ccl::%get-double-float contentrect 0))
                          (y      (ccl::%get-double-float contentrect 1))
                          (width  (ccl::%get-double-float contentrect 2))
                          (height (ccl::%get-double-float contentrect 3))
                          (arect  (ns:make-ns-rect 0.0 (/ height 2) width height)))
                     (setf scroll-view [[NSScrollView alloc]
                                        initWithFrame: arect])
                     [scroll-view setAutoresizingMask:#$NSViewNotSizable]
                     [scroll-view setHasHorizontalScroller:YES]
                     [scroll-view setHasHorizontalRuler:YES]
                     [scroll-view setHasVerticalScroller:YES]
                     [scroll-view setHasVerticalRuler:YES]
                     [scroll-view setRulersVisible:YES]
                     [[self contentView] addSubview:scroll-view]
                     
                     (setf text-view [[NSText alloc] initWithFrame:aRect])
                     ;;  [text-view setOpaque:YES]
                     [text-view setAutoresizingMask:#$NSViewNotSizable]
                     [scroll-view setDocumentView:text-view]

                     ;; *** Step 7:  Show a selection in the key window ***
                     [text-view selectAll:*null*]
                     
                     ;; NSViewNotSizable
                     ;; NSViewMinXMargin|NSViewWidthSizable|NSViewMaxXMargin
                     ;; NSViewMinYMargin|NSViewHeightSizable|NSViewMaxYMargin   

                     (setf aRect  (ns:make-ns-rect 0.0 0.0 width (/ height 2)))
                     (setf my-little-view [[LittleView alloc]initWithFrame:arect])
                     [my-little-view setAutoresizingMask:#$NSViewNotSizable]
                     [[self contentView] addSubview:my-little-view]))

                 [self setDelegate:self])
               self]



@[LittleWindow method: (windowDidResize:(:id)notification)
               resultType: (:void)
               body:
               (declare (ignore notification))
               (let* ((above (get-nsrect [[self contentView] bounds]))
                      (below (copy-nsrect above))
                      (mid (truncate (nsrect-height above) 2)))
                 (incf (nsrect-y above) mid)
                 (decf (nsrect-height above) mid)
                 (decf (nsrect-height below) mid)
                 [(slot-value self 'scroll-view)    setFrame:(unwrap above)]
                 [(slot-value self 'my-little-view) setFrame:(unwrap below)]
                 [self update])]


(defun test/little ()
  (let (myWindow myPanel myMenu item button aRect)
    ;; Step 1:  Set up a Window 
    (setf aRect (ns:make-ns-rect 100.0 250.0 300.0 300.0))
    (setf myWindow [[LittleWindow alloc]
                    initWithContentRect:aRectents
                    styleMask:(logior #$NSTitledWindowMask
                                      #$NSClosableWindowMask
                                      #$NSResizableWindowMask
                                      #$NSMiniaturizableWindowMask)
                    backing:#$NSBackingStoreBuffered
                    defer:NO])
    [myWindow setTitle:@"A Little Demonstration"]

    ;; Step 2:  Set up a Panel 
    (setf aRect (ns:make-ns-rect 100.0 200.0 300.0 100.0))
    (setf myPanel [[NSPanel alloc]
                   initWithContentRect:aRect
                   styleMask:(logior #$NSTitledWindowMask #$NSClosableWindowMask)
                   backing:#$NSBackingStoreBuffered
                   defer:YES])
    [myPanel setTitle:@"About Little"]
    ;;  [myPanel removeFromEventMask:(NS_KEYDOWNMASK | NS_KEYUPMASK)]
    (setf button [[NSButton alloc] initWithFrame:(ns:make-ns-rect 50.0 25.0 200.0 50.0)])
    [button setTitle:@"Little"]
    [button setTarget:myPanel]
    [button setAction:(objc:@selector "performClose:")]
    [button setAutoresizingMask: (logior #$NSViewMinXMargin #$NSViewMaxXMargin
                                         #$NSViewMinYMargin #$NSViewMaxYMargin)]
    [[myPanel contentView] addSubview:button]


    ;; Step 3:  Set up a Menu 
    (setf myMenu  [[NSMenu alloc] initWithTitle:@"Little"])

    (setf item [myMenu addItemWithTitle:@"Info..."
                       action:(objc:@selector "makeKeyAndOrderFront:")
                       keyEquivalent:@""])
    [item setTarget:myPanel]
    [item setEnabled:YES]

    (setf item [myMenu addItemWithTitle:@"Hide"
                       action:(objc:@selector "hide:")
                       keyEquivalent:@"h"])
    [item setEnabled:YES]

    (setf item [myMenu addItemWithTitle:@"Quit"
                       action:(objc:@selector "terminate:")
                       keyEquivalent:@"q"])
    [item setEnabled:YES]

    [myMenu sizeToFit]

    (setf item [[NSMenuItem alloc] initWithTitle:@"Little"
                action:oclo:*null*
                keyEquivalent:@""])
    [item setSubmenu:myMenu]
    [item setEnabled:YES]
    [[[NSApplication sharedApplication] mainMenu] addItem:item]


    ;; Step 4:  Display all windows that aren't deferred 
    [myWindow display]


    ;; Step 5:  Move myWindow on-screen 
    [myWindow orderFront:nil]


    ;; Step 6:  Make it the key window 
    [myWindow makeKeyWindow]))


;; (test/little)
;; (loop repeat 1000 do [(slot-value [[NSApplication sharedApplication] mainWindow] 'my-little-view) display])

