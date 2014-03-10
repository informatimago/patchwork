(in-package :ui)

(objcl:set-objective-cl-syntax)

;;------------------------------------------------------------

;; (eql ccl:+null-ptr+ ccl:+null-ptr+) 
;; (class-of )#<built-in-class ccl:macptr>

#-(and)
(defun wrap-circularly (object)
  (with-circular-references ()
    (labels ((walk (key object)
               (declare (ignore key))
               (when (circular-register object)
                 (structure object (function walk)))))
      (walk :root object))
    (wrap object)
    ;; (com.informatimago.common-lisp.cesarum.utility:print-hashtable (car *circular-references*))
    ))



#-(and)
'(("NSArray"
   (lambda (nsarray)
       (when (circular-register nsarray)
         (dotimes (i [nsarray count])
           (walk [nsarray objectAtIndex:i]))))
   (lambda (nsarray)
       (let ((index (circular-reference nsarray)))
         (if (and index (cdr index))
           (wrap-reference (car index))
           (progn
             (if index
               (wrap-referenced-object (car index) "NSArray"
                                       (dotimes (i [nsarray count])
                                         (wrap  [nsarray objectAtIndex:i])))
               (wrap-unreferenced-object "NSArray"
                                         (dotimes (i [nsarray count])
                                           (wrap  [nsarray objectAtIndex:i]))))))))))


;;------------------------------------------------------------



#-(and)
(progn
  [[NSApplication sharedApplication] mainMenu]
  [NSMenu menuBarHeight]
  [NSMenu setMenuBarVisible:nil]
  [NSMenu setMenuBarVisible:YES]
  [[[NSApplication sharedApplication] mainMenu] itemAtIndex:0]
  
  [[[[NSApplication sharedApplication] mainMenu] itemAtIndex:0] setTitle:@"Clozure CL"]
  
  [[[[[[NSApplication sharedApplication] mainMenu] itemAtIndex:0] submenu] itemAtIndex:0] setTitle: @"About Clozure CL"]

  )

(defun nsmenuitem-to-lisp (nsmenuitem)
  (list* :title (lisp-string [nsmenuitem title])
        :tag   [nsmenuitem tag]
        :state [nsmenuitem state]
        :key-equivalent (lisp-string [nsmenuitem keyEquivalent])
        :is-enabled [nsmenuitem isEnabled]
        :is-hidden [nsmenuitem isHidden]
        :is-hilighted [nsmenuitem isHighlighted]
        :is-alternate [nsmenuitem isAlternate]
        :is-separator-item [nsmenuitem isSeparatorItem]
        :has-submenu [nsmenuitem hasSubmenu]
        (when [nsmenuitem hasSubmenu]
          (list :submenu (nsmenu-to-lisp [nsmenuitem submenu])))))

(defun nsmenu-to-lisp (nsmenu)
  (list :title (lisp-string [nsmenu title])
        :items (loop
                 :for i :from 0 :below [nsmenu numberOfItems]
                 :collect (nsmenuitem-to-lisp [nsmenu itemAtIndex:i]))))

(setf *print-right-margin* 80
      *print-circle* nil)

(pprint (nsmenu-to-lisp [[NSApplication sharedApplication] mainMenu]))





(\#$ERRAEEVENTNOTHANDLED \#$KEYRETURNIDATTR %appleevent-handlers%
%cancel-button %current-key-handler %default-button %get-cancel-button
%get-current-key-handler %get-default-button %get-font-name
%get-key-handler-list %key-handler-list %queued-reply-handlers%

 *control-q-comtab*  *default-menubar*
*disable-bubbles-on-inactive-windows* *font-name-number-alist*
*foreground* 
*script-font-alist* *temp-rgn* *transfer-modes*



add-new-item allow-returns allow-tabs amount
any-modifier-keys-p app apple-menu appleevent appleevent-or-id

arg arglist args attrs auto-position b back-color
bindings body bottom bottom-border bottomright bubble-attrs c cell
checkedp chr class-options class-precedence-list clauses cleanup-forms
cliprgn close-box-p code codes color color->ff-index color-list
color-mask colored colorp consider-window-method constraint container
coord cpl current-font-codes current-font-view current-h current-v
current-view cursor d d-button default default-application-creator
default-position default-size delta destination-view dialog
dialog-item dim-if-undefined direct-subviews-only disabled doc doc-p
document dont-inval-subviews dovector duration each ed-beep edit-menu
empty-menubar enable-sharp-at-reader--macro enabledp end end-test
ensure-list entry env eql-specializer eql-specializer-object
erase-anonymous-invalidations erase-p erase-rgn err error-p
error-pointer errorp errsym face face-mask ff ff-code ff-mask filename
find-all-characters fixnump flag font font-code font-mask
font-name-from-number font-number-from-name font-values font-view for
form form-p forms fred-shadowing-comtab fun function-name
generic-function-methods get-bubble-attributes
get-window-event-handler gf grafport-write-string grow-icon-p h
handler handlerrefcon hardcopy hook id id-table idle include-invisible
include-invisibles include-windoids initialize-application
initialize-cursor initialize-eval initialize-event initialize-font
initialize-menu initialize-pattern initialize-patterns
initialize-scrap initialize-screen initialize-view initialize-window
invalid-rgn item item-list item-num items key key-handler key-hdlr
keyform keywords-and-forms l lambda-list left left-border len lr
make-menu-item make-process-queue make-view-invalid make-view-valid
maybe-erase mb me menu-item-class menu-item-number menu-select
menu-update-for-modal menubar-add-menu menubar-delete-menu
menubar-menu menubar-menus menus message method-exists-p
method-specializers methods mode mode-mask modes modifiers mousergn
moved-h moved-v ms ms-code ms-mask my-item my-window n name
name-or-lambda-expression new-color new-container new-ff new-function
new-height new-key new-layer new-mark new-menubar-list new-ms new-name
new-rgn new-size new-title new-width new-window newstyle niy
non-window-method-exists-p num object object-name old-container old-ff
old-ff-code old-ms old-ms-code old-position old-sc-pos old-window on
op option options options-and-methods other-points output-stream
overwrite-p owner p paramlist parent pattern ph place places point
point-list point1 point2 pos pos-h pos-v process prompt protected-form
psize pt pv queue reason refcon region regions-overlap-p reply
reselect-windows reset-style-p reset-stype-p result ret return-id
revert rgn rgn-vars right right-border s save save-as save-copy-as sb
scrap-type scrap-value scroll-visibly self set-bubble-attributes
set-part-color-loop set-rect-region set-view-container-slot
sharp-at-dispatch-reader-macro show-on-resume-p siblings size
size-mask sl sleep-ticks sleeptime slots source-view spec sr st
standard-generic-function-p start startup string-match-p stuff style
subchar subview subview-type subview-var superclasses supers sym
sys-beep tag tags-or-forms temp test test-form the-desc theappleevent
thing title top topleft typespec ul update-edit-menu-items
update-function update-menu-items updater v val valid value value-p
values-form value]* var var-init-steps varlist vars varsym view-alist
view-allocate-clip-region view-clip-region view-contains-p
view-is-invalid-p view-origin view-preferred-size view-rgn views
vindex visiblep visrgn vlengh vlength vsubview-type vsubviews vvector
vview w what where win wind window-border-width
window-bottom-border-width window-bring-to-front window-class
window-menu-item window-preferred-screen-bounds
window-right-border-width window-title-height
window. windows-menu-menu-item with-temp-rgns wob x xfer-mode-arg
xfer-mode-to-name [symbol)


SET-PART-COLOR-LOOP

(in-package :pw)

(defparameter *slime-trace-output* *trace-output*)
(setf *trace-output*
      (open "/home/pjb/patchwork-trace.txt" :direction :output :if-does-not-exist :create :if-exists :supersede))
(finish-output *trace-output*)
(setf)

(close *step-trace-output*)

(setf *step-trace-output* (open "/home/pjb/patchwork-trace.txt"
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede
                                #+ccl :sharing #+ccl :external)
      *step-package* (find-package :pw)
      *step-mode* :trace)

(setf *step-trace-output* (open "/home/pjb/patchwork-trace.txt"
                                :direction :output
                                :if-does-not-exist :create
                                :if-exists :supersede
                                #+ccl :sharing #+ccl :external)
      *step-package* (find-package :pw)
      *step-mode* :trace)


(setf *step-package* (find-package :pw)
      *step-max-trace-depth* 4
      *step-mode* :trace)


(progn
  (princ
   (with-output-to-string (*step-trace-output*)
     (let ((*readtable* *readtable-patchwork*))
       (step (load #P"/Users/pjb/Patches/addition et multiplication" :verbose t :external-format :mac-roman)
             :trace)))) 
  (values))


(in-package :ui)
(available-members-of-font-family "Times")
(("Times-Roman" "Regular" #<wrapper #<mclgui-referenced #(1 . t)=#<ns-number 5 (#x503)> #x302003C133FD> #x302003C133BD> #<wrapper #<ns-number 0 (#xC3)> #x302003C11C8D>)
 ("Times-Italic" "Italic" #<wrapper #<mclgui-reference #1# #x302003C0829D> #x302003C0825D> #<wrapper #<ns-number 1 (#x1C3)> #x302003C06B1D>)
 ("Times-Bold" "Bold" #<wrapper #<mclgui-referenced #(2 . t)=#<ns-number 9 (#x903)> #x302003C0340D> #x302003C033CD> #<wrapper #<ns-number 2 (#x2C3)> #x302003C01C8D>)
 ("Times-BoldItalic" "Bold Italic" #<wrapper #<mclgui-reference #2# #x302003C5DDCD> #x302003C5DD8D> #<wrapper #<ns-number 3 (#x3C3)> #x302003C5C64D>))

(ccl:%get-cstring [(handle (fourth (first (available-members-of-font-family "Times")))) objCType])
"q"
[(handle (fourth (first (available-members-of-font-family "Times")))) longLongValue]
(class-of (handle (fourth (first (available-members-of-font-family "Times")))))
#<objc:objc-class ns:ns-number (#x7FFF7C42EEF8)>
#<ns-number 0 (#xC3)>
0
0
#<ns-number 0 (#xC3)>
#<wrapper #<ns-number 0 (#xC3)> #x3020039D1B8D>
(("Times-Roman" "Regular" #<wrapper #<mclgui-referenced <MclguiReferenced: 0x57bdb0> (#x57BDB0)> #x302003AF337D> #<wrapper #<ns-number 0 (#xC3)> #x302003AF1C4D>)
 ("Times-Italic" "Italic" #<wrapper #<mclgui-reference <MclguiReference: 0x8119e10> (#x8119E10)> #x302003AE821D> #<wrapper #<ns-number 1 (#x1C3)> #x302003AE6ADD>)
 ("Times-Bold" "Bold" #<wrapper #<mclgui-referenced <MclguiReferenced: 0x579dc0> (#x579DC0)> #x302003AE338D> #<wrapper #<ns-number 2 (#x2C3)> #x302003AE1C4D>)
 ("Times-BoldItalic" "Bold Italic" #<wrapper #<mclgui-reference <MclguiReference: 0x8124310> (#x8124310)> #x302003B1DD8D> #<wrapper #<ns-number 3 (#x3C3)> #x302003B1C64D>))


#|

ui> (nsfont-from-codes 0 0)
#<ns-font "LucidaGrande 12.00 pt. P [] (0x5c2e50) fobj=0x11a8a40, spc=3.80" (#x5C2E50)>
:srccopy
0
(:plain)
ui> (defparameter *font-data* [NSArchiver archivedDataWithRootObject:(nsfont-from-codes 0 0)])
*font-data*
ui> *font-data*)
; Evaluation aborted on #<ccl::simple-reader-error #x302004C5B86D>. ;
ui> *font-data*
#<ns-mutable-data <040b7374 7265616d 74797065 6481e803 84014084 8484064e 53466f6e 741e8484 084e534f 626a6563 74008584 01692484 055b3336 635d0600 00001a00 0000fffe 4c007500 63006900 64006100 47007200 61006e00 64006500 00008401 660c8401 63009801 98009800 86> (#x119B3A0)>
ui> [NSUnarchiver unarchiveObjectWithData:*font-data*]
#<ns-font "LucidaGrande 12.00 pt. P [] (0x5c2e50) fobj=0x11a8a40, spc=3.80" (#x5C2E50)>
|#

