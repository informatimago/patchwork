;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               menu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     OpenStep
;;;;DESCRIPTION
;;;;
;;;;    This implements the MCL menu API.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created (with some API stuff copied from from
;;;;                     mcl l1-menus.lisp).
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

(in-package "MCLGUI")



;;;---------------------------------------------------------------------
;;; Menubar
;;;---------------------------------------------------------------------

(defclass menubar (colored)
  ((menus :initform '()
          :initarg :menus
          :accessor menubar-menus)))


(defmethod color-parts ((thing menubar))
  '(:menubar :default-item-title :default-menu-background :default-menu-title))


(defvar *menubar*         nil
  "
The value of the *menubar* variable is the single instance of the
menubar class.
")


(defvar *default-menubar* nil
  "
The variable *DEFAULT-MENUBAR* contains a list of the menus that are
installed when you first start Macintosh Common Lisp.  You may use
SET-MENUBAR to restore the original menus after installing your own
set of menus.

Note that *DEFAULT-MENUBAR* is simply a list of the menus present when
Macintosh Common Lisp starts up.  It does not contain any code for
initializing these menus. If you destructively change the startup
menus, then *DEFAULT-MENUBAR* will contain the changed menus. Calling
\(SET-MENUBAR *DEFAULT-MENUBAR*) will not undo those modifications.
")


(defvar *apple-menu*      nil
  "
The variable *APPLE-MENU* contains the Apple menu from the initial
menubar.

NOTE: This is a shadow placeholder.  OpenStep doesn't have an \"Apple
Menu\", and Cocoa manages its own Apple menu outside of the reach of
the applications.
")


(defvar *file-menu*       nil
  "
The variable *FILE-MENU* contains the File menu from the initial
menubar.
")


(defvar *edit-menu*       nil
  "
The variable *EDIT-MENU* contains the Edit menu from the initial
menubar.
")


(defvar *lisp-menu*       nil
  "
The variable *LISP-MENU* contains the Lisp menu from the initial
menubar.
")


(defvar *tool-menu*       nil
  "
The variable *TOOL-MENU* contains the Tool menu from the initial
menubar.
")


(defvar *window-menu*       nil
  "
The variable *WINDOW-MENU* contains the Window menu from the initial
menubar.
")


(defvar *menubar-frozen*  nil
  "
The *MENUBAR-FROZEN* variable is typically bound to t while several
menu changes are made.  Once the changes are complete, a call to
DRAW-MENUBAR-IF draws the new menubar all at once.  This mechanism can
prevent undue flickering of the menubar.

If the value of this variable is true, no menubar redrawing will
occur.

If the value of this variable is NIL, the menubar will be redrawn.

If you use *MENUBAR-FROZEN*, it is up to you to later call
DRAW-MENUBAR-IF.  The menubar is not redrawn automatically.
")



(defgeneric empty-menubar (menubar)
  (:method ((mb menubar))
    (dolist (menu (menubar-menus mb) mb)
      (unless (eq *apple-menu* menu)
        (menu-deinstall menu)))))

(defgeneric menubar-add-menu (menubar menu)
  (:method ((mb menubar) menu)
    (setf (menubar-menu mb) (nconc (menubar-menu mb) (list menu)))
    menu))

(defgeneric  menubar-delete-menu (menubar menu)
  (:method ((mb menubar) menu)
    (unless (eq *apple-menu* menu)
      (setf (menubar-menu mb) (delete menu (menubar-menu mb))))
    menu))



(defun menubar ()
  "
RETURN:         A list of the menus currently installed in the *MENUBAR*.
"
  (when *menubar*
    (copy-list (menubar-menus *menubar*))))


(defun set-menubar (new-menubar-list)
  "
DO:             The set-menubar function installs a new set of menus
                in the current menubar.  First the MENU-DEINSTALL
                function is applied to each installed menu except the
                Apple menu, and then the MENU-INSTALL function is
                applied to each menu in NEW-MENUBAR-LIST. The
                NEW-MENUBAR-LIST may be empty, in which case the
                menubar is simply cleared.

RETURN:         NEW-MENUBAR-LIST.

NOTE:           You can never remove the Apple menu. Even if you call
                (SET-MENUBAR NIL), the Apple menu remains in the
                menubar.
"
  (let ((*menubar-frozen* t))
    (empty-menubar *menubar*)
    (mapc (function menu-install) new-menubar-list))
  (draw-menubar-if)
  new-menubar-list)


(defun find-menu (name)
  "
RETURN:         The first menu in the menubar that has string as its
                title.  If no matching menu is found, it returns NIL.
"
  (find name  (menubar-menus *menubar*)
        :key (function menu-title) :test (function string-equal)))



(defun draw-menubar-if ()
  "
DO:             Redraw the menubar if the value of *MENUBAR-FROZEN* is
                NIL.  If the value of *MENUBAR-FROZEN* is not nil, no
                action is taken.
"
  (unless *menubar-frozen*
    (niy draw-menubar-if)))



;;;---------------------------------------------------------------------
;;; Menus
;;;---------------------------------------------------------------------


(defclass menu-element (colored)
  ((color-list      :initform '()
                    :initarg :menu-item-colors
                    :initarg :menu-colors)
   (owner           :initform nil
                    :reader   menu-item-owner
                    :reader   menu-owner)
   (title           :initform "Untitled"
                    :initarg :menu-item-title
                    :reader   menu-item-title
                    :initarg :menu-title
                    :reader   menu-title
                    :documentation "The menu or menu-item title.")
   (enabledp        :initform t
                    :initarg :enabledp
                    :reader   menu-item-enabled-p
                    :reader   menu-enabled-p
                    :documentation "Whether the menu is enabled.")
   (style           :initform nil
                    :reader   menu-item-style
                    :reader   menu-style
                    :documentation "The font style in which the menu appears.
Styles are :plain, :bold, :italic, :shadow, :outline, :underline,
:condense, and :extend.  The keyword :plain indicates the absence of
other styles.")
   (update-function :initform nil
                    :initarg  :update-function
                    :accessor menu-update-function
                    :accessor menu-item-update-function
                    :documentation "The function that is run when the menu is updated.")
   (help-spec       :initform nil
                    :initarg  :help-spec
                    :accessor help-spec)
   (checkedp        :initform nil)))



(defmethod print-object ((self menu-element) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~S" (slot-value self 'title)))
  self)


(defclass menu (menu-element)
  ((item-list   :initform '())
   (menu-font   :initform nil
                :initarg :menu-font)))


(defmethod initialize-instance :after ((menu menu) &key (menu-items ()) &allow-other-keys)
  (when menu-items
    (apply (function add-menu-items) menu menu-items)))


(defmethod color-parts ((thing menu))
  '(:menu-title :menu-background :default-menu-item-title))

(defgeneric menu-items (menu &optional menu-item-class)
  (:documentation "
RETURN:         A list of the menu items installed in the menu.  The
                menu items are listed in the order in which they
                appear in the menu.

MENU-ITEM-CLASS:
                The class from which the returned menu items inherit.
                The default value is menu-element.  Only those menu
                items that inherit from menu-item-class are included
                in the list that is returned.
")
  (:method ((menu menu) &optional (menu-item-class 'menu-element))
    
    (copy-seq (remove-if-not (lambda (item) (typep item menu-item-class))
                             (slot-value menu 'item-list)))))

(defgeneric menu-font (menu)
  ;; (:documentation )
  (:method ((menu menu))
    (or (slot-value menu 'menu-font)
        (and (menu-owner menu)
             (menu-font (menu-owner menu))))))


(defgeneric set-menu-title (menu new-title)
  (:documentation "
DO:             Set the menu title to NEW-TITLE.  If the menu is
                installed, the change in title is immediately
                reflected in the menubar.

RETURN:         NEW-TITLE
")
  (:method ((menu menu) new-title)

    (if (menu-owner menu)
        (set-menu-item-title menu new-title)
        (progn
          (setf (slot-value menu 'title) (copy-seq new-title))
          (niy set-menu-title)))
    new-title))


(defgeneric menu-install (menu)
  (:documentation "
DO:             Add the menu to the menubar at the rightmost position.
RETURN:         T.
")
  (:method ((me menu-element))
    t)
  (:method ((menu menu))
    (unless (slot-value menu 'owner)
      (menubar-add-menu *menubar* menu))
    (niy menu-install)
    t))


(defgeneric menu-deinstall (menu)
  (:documentation "
DO:             Remove a menu from the menubar.
RETURN:         NIL.
")
  (:method ((me menu-element))
    nil)
  (:method ((menu menu))
    (dolist (item (slot-value menu 'item-list))
      (when (typep item 'menu)
        (menu-deinstall item)))
    (menubar-delete-menu *menubar* menu)
    (niy menu-deinstall)
    nil))


(defgeneric menu-installed-p (menu)
  (:documentation "
RETURN:         Whether the menu is installed.
")
  (:method ((menu menu))
    (niy menu-installed-p)
    nil))




(defgeneric menu-enable (menu)
  (:documentation "
DO:             Enable a menu, making it possible to choose its
                items.  This function has no effect if the menu is
                already enabled.  Menus can be enabled and disabled at
                any time.  The effects are visible only when the menu
                is installed in the current menubar.
")
  (:method ((menu menu))
    (unless (menu-enabled-p menu)
      (setf (slot-value menu 'enabledp) t)
      ;; (let ((owner (slot-value menu 'owner)))
      ;;   (if owner
      ;;       ))
      (niy menu-enable))
    menu))


(defgeneric menu-disable (menu)
  (:documentation "
DO:             Disable a menu.  Its items may still be viewed, but
                they cannot be chosen.  The menu and its items appear
                dimmed.  This function has no effect if the menu is
                already disabled.  Menus can be enabled and disabled
                at any time.  The effects are visible only when the
                menu is installed in the current menubar.
")
  (:method ((menu menu))
    (when (menu-enabled-p menu)
      (setf (slot-value menu 'enabledp) nil)
      (niy menu-disable))
    menu))


(defgeneric menu-update (menu)
  (:documentation "
The MENU-UPDATE generic function is called whenever the user clicks in
the menubar or presses a command-key equivalent. The menu-update
method for menus calls the menu’s menu-update-function on menu if it
has one; otherwise it calls menu-item-update on each item in the
menu. This facility is provided so that menus and menu items can be
adjusted to the current program context before they are
displayed. (For example, an item may be checked or unchecked, enabled
or disabled, added, removed, or reordered).

You can specialize menu-update, but you normally do not need to call
it. (It is called by the MCL run-time system.)
")
  (:method ((menu menu))
    (let ((updater (menu-update-function menu)))
      (if updater
          (funcall updater menu)
          (mapc (function menu-item-update)
                (slot-value menu 'item-list))))
    menu))



;;;---------------------------------------------------------------------
;;; MENU-ITEM
;;;---------------------------------------------------------------------


(defclass menu-item (menu-element)
  ((checkedp              :initform nil
                          :initarg :menu-item-checked
                          :reader   menu-item-check-mark
                          :documentation "The character currently used as a check mark beside
the menu item, or NIL if the item is not currently checked.")
   (command-key           :initform nil
                          :initarg :command-key
                          :reader   command-key
                          :documentation "The keyboard equivalent of the menu item.
If there is no keyboard equivalent, then nil.")
   (menu-item-action      :initarg :menu-item-action
                          :accessor menu-item-action-function
                          :documentation "The function that is called when the menu item is selected.")
   (menu-item-icon-num    :initform nil
                          :initarg :icon-num
                          :reader   menu-item-icon-num
                          :writer   (setf menu-item-icon-num-slot))
   ;; This slot eventually contains a list of handle ostype and id so
   ;; if handle goes dead after save-application it can be
   ;; reconstructed, in some cases anyway
   (menu-item-icon-handle :initform nil
                          :initarg  :icon-handle
                          :writer   (setf menu-item-icon-handle-slot)
                          :reader   menu-item-icon-handle)
   (menu-item-icon-type   :initform nil
                          :initarg :icon-type
                          :reader   menu-item-icon-type
                          :writer   (setf menu-item-icon-type-slot))
   (menu-item-script      :initform nil
                          :initarg :menu-item-script
                          :reader   menu-item-script
                          :writer   (setf menu-item-script-slot))))

(defmethod initialize-instance :after ((menu-item menu-item)
                                       &key disabled &allow-other-keys)
  (setf (slot-value menu-item 'enabledp) (not disabled)))


(defmethod color-parts ((thing menu-item))
  '(:item-title :item-key :item-mark))


(defgeneric menu-item-action (item)
  (:documentation "
The MENU-ITEM-ACTION generic function is called whenever the user
chooses the menu item or presses the keyboard equivalent.  The method
defined on MENU-ITEM calls the function that is the value of MENU-ITEM-ACTION
of MENU-ITEM.
")
  (:method ((menu menu))
    nil)
  (:method ((menu-item menu-item))
    (let ((action (menu-item-action-function menu-item)))
      (when action (funcall action)))))


(defgeneric set-menu-item-action-function (menu-item new-function)
  (:documentation "
DO:             Set the menu-item-action-function of menu-item to NEW-FUNCTION.
RETURN:         NEW-FUNCTION.
")
  (:method ((menu-item menu-item) new-function)
    (setf (menu-item-action-function menu-item) new-function)))


(defgeneric set-menu-item-title (item new-title)
  (:documentation "
DO:             Set the menu item title to NEW-TITLE.  If the menu
                item is installed, the change in title is immediately
                reflected in the menu.  If the title is \"-\" then the
                menu item is a separator, an unselectable dotted line.
 
RETURN:         NEW-TITLE
")
  (:method ((item menu-element) new-title)
    (let ((owner (menu-item-owner item)))
      (setf (slot-value item 'title) (copy-seq new-title))
      (when owner
        (when (string= new-title "-")
          (menu-item-disable item)))
      (niy set-menu-item-title))
    new-title))


(defgeneric menu-item-disable (item)
  (:documentation "
DO:             Disable the menu ITEM so that it cannot be chosen.
                The function has no effect if the menu item is already
                disabled.
")
  (:method ((item menu-element))
    (when (menu-item-enabled-p menu)
      (setf (slot-value item 'enabledp) nil)
      (niy menu-item-disable)))
  (:method ((menu menu))
    (menu-disable menu)))


(defgeneric menu-item-enable (item)
  (:documentation "
DO:             Enable the menu ITEM so that it cannot be chosen.
                The function has no effect if the menu item is already
                enabled.
")
  (:method ((item menu-element))
    (unless (menu-item-enabled-p menu)
      (setf (slot-value item 'enabledp) t)
      (niy menu-item-enable)))
  (:method ((menu menu))
    (menu-enable menu)))


(defgeneric set-menu-item-enabled-p (item flag)
  (:documentation "
DO:             Enable or disable the menu ITEM depending on the FLAG.
RETURN:         FLAG.
")
  (:method ((item menu-element) flag)
    (if flag
        (menu-item-enable  item)
        (menu-item-disable item))
    flag))


(defgeneric set-command-key (item new-key)
  (:documentation "
DO:             Set the keyboard equivalent of the menu item to
                NEW-KEY, or to nothing if NEW-KEY is NIL.
NEW-KEY:        NIL, or a character, or a list (modifier character).
                modifier can be :shift, :option (or :meta), or :control.
")
  (:method  ((item menu-element) new-key)
    (let ((mod (when (consp new-key) (first  new-key)))
          (chr (when (consp new-key) (second new-key))))
      (setf (slot-value item 'command-key) new-key)
      (niy set-command-key mod chr))))



(defparameter *not-check-mark*        (code-char #x0237B) " 9083  ⍻  \"NOT_CHECK_MARK\"")
(defparameter *ballot-box-with-check* (code-char #x02611) " 9745  ☑  \"BALLOT_BOX_WITH_CHECK\"")
(defparameter *check-mark*            (code-char #x02713) "10003  ✓  \"CHECK_MARK\"")
(defparameter *heavy-check-mark*      (code-char #x02714) "10004  ✔  \"HEAVY_CHECK_MARK\"")

(defgeneric set-menu-item-check-mark (item new-mark)
  (:documentation "
DO:             Set the character to be used as a check mark beside
                the menu item.  If NEW-MARK is NIL, no check mark
                appears next to the command.  If NEW-MARK is T, then a
                standard check-mark symbol (✓) appears beside the
                command.  If it is a character or the UNICODE value of
                a character, then the corresponding character appears
                next to the menu

RETURN:         NEW-MARK.
")
  (:method ((item menu-element) new-mark)
    (check-type new-mark (or character integer (member t nil)))
    (setq new-mark
          (cond ((characterp new-mark) new-mark)
                ((integerp new-mark)   (code-char new-mark))
                ((eq t new-mark)       *check-mark*)
                ((null new-mark)       nil)))
    (unless (eql new-mark (menu-item-check-mark item))
      (setf (slot-value item 'checkedp) new-mark)
      (niy set-menu-item-check-mark))))


(defgeneric set-menu-item-style (item newstyle)
  (:documentation "
DO:             Set the font style in which the menu item appears.

NEW-STYLES:     A keyword or list of keywords. Allowable keywords are
                :plain, :bold, :italic, :shadow, :outline, :underline,
                :condense, and :extend. The keyword :plain indicates
                the absence of other styles.
")
  (:method ((item menu-element) newstyle)
    (setf (slot-value item 'style) newstyle)
    (niy set-menu-item-style item newstyle)))


(defgeneric menu-item-update (item)
  (:documentation "
The generic function menu-item-update is called when a user clicks a
menu if the menu does not have its own menu-update-function. In this
case, menu-item-update is called on each menu item in the menu.  The
user normally does not need to call this function; it is called
indirectly by the MCL event system.
")
  (:method ((item menu-element))
    (let ((updater (menu-item-update-function item)))
      (when updater
        (funcall updater item)
        t)))
  (:method ((menu menu))
    (menu-update menu)))


(defgeneric set-menu-item-update-function (menu-item new-function)
  (:documentation "
DO:             Set the function used to update the MENU-ITEM.
")
  (:method ((menu-item menu-element) new-function)
    (setf (menu-item-update-function menu-item) new-function)))



(define-condition menu-already-installed-error (error)
  ((menu :initarg :menu :reader menu-already-installed))
  (:report (lambda (err stream)
             (format stream "Menu ~S is already installed."
                     (menu-already-installed err)))))


(define-condition menu-item-not-owned-error (error)
  ((menu :initarg :menu :reader menu-item-not-owned-error-menu)
   (item :initarg :item :reader menu-item-not-owned))
  (:report (lambda (err stream)
             (format stream "Menu item ~S is not a menu item of ~S."
                     (menu-item-not-owned err)
                     (menu-item-not-owned-error-menu err)))))


(defgeneric add-menu-items (menu &rest menu-items)
  (:documentation "
DO:             Append menu-items to the menu.  The new items are
                added to the bottom of the menu in the order
                specified.

RETURN:         NIL.
")
  (:method ((menu menu) &rest menu-items)
    (let ((menu-items (mapcar (lambda (item)
                                (check-type item menu-element)
                                (when (typep item 'menu)
                                  (when (menu-installed-p item)
                                    (error 'menu-already-installed-error :menu item)))
                                item)
                              menu-items)))
      (loop
        :for item :in menu-items
        :do (progn
              (setf (slot-value menu 'item-list) (nconc (slot-value menu 'item-list) (list item))
                    (slot-value item 'owner) menu)
              (when (and (stringp (slot-value item 'title))
                         (string= (slot-value item 'title) "-"))
                (setf (slot-value item 'enabledp) nil))
              (when (typep item 'menu)
                (when (menu-installed-p menu)
                  (menu-install item)))
              (set-part-color-loop item (slot-value item 'color-list))
              (niy add-menu-items))))))


(defgeneric remove-menu-items (menu &rest menu-items)
  (:documentation "
DO:             Remove menu-items from the menu. The removed
                menu-items may be reinstalled later or installed in
                other menus. It is not an error to attempt to remove
                an item that is not in the menu.

RETURN:         NIL.
")
  (:method ((menu menu) &rest menu-items)
    (dolist (item menu-items)
      (when item
        (let ((owner (menu-item-owner item)))
          (when owner
            (unless (eq owner menu)
              (error 'menu-item-not-owned-error :menu menu :item item))
            (setf (slot-value item 'owner) nil)
            (when (typep item 'menu)
              (let ((*menubar-frozen* t))
                (menu-deinstall item)))
            (setf (slot-value menu 'item-list) (delete item (slot-value menu 'item-list)))))))))


(defgeneric find-menu-item (menu title)
  (:documentation "
RETURN:         The menu item with given title or NIL if none.
")
  (:method ((menu menu) title)
    (find title (slot-value menu 'item-list)
          :test (function string-equal) :key (function menu-item-title))))



;;;---------------------------------------------------------------------
;;;
;;; Miscellaneous
;;;


;; Used by window.lisp:

(defun make-menu-item (title &optional action &rest stuff &key &allow-other-keys)
  (apply (function make-instance) 'menu-item
         :menu-item-title title
         :menu-item-action action
         stuff))



;; Used by initialize.lisp:

(defun add-new-item (menu title &optional action
                          &rest rest &key (class 'menu-item) &allow-other-keys)
  (remf rest :class)
  (let ((item (apply (function make-instance)
                     class
                     :menu-item-title title
                     :menu-item-action action
                     rest)))
    (add-menu-items menu item)
    item))




(defun update-edit-menu-items (menu)
  "
This is the menu-item-update-function for the items in the Edit menu.
"
  (let ((on nil)
        (items  (slot-value menu 'item-list)))
    (when items
      (dolist (item items)
        (menu-item-update item)
        (when (menu-item-enabled-p item)
          (setf on t)))
      (if on
          (menu-enable  menu)
          (menu-disable menu)))))


(defmethod menu-update-for-modal ((menu menu) &optional what)
  (let ((updater (menu-update-function menu)))
    (if updater
        (funcall updater menu)
        (case what
          (:disable (menu-disable menu))
          (:enable  (menu-enable  menu))))))


 (defmethod menu-select ((menu menu) num)
   ;; Use ELT because errs out if list too short...
  (menu-item-action (elt (slot-value menu 'item-list) (1- num))))



;;;Menu-item objects

(defgeneric menu-item-number (item)
  (:method ((item menu-element))
    (let ((owner (slot-value item 'owner)))
      (when owner
        (position item (slot-value owner 'item-list))))))




(defclass apple-menu (menu)
  ()
  (:default-initargs :menu-title "Apple"))

(defmethod menu-deinstall ((menu apple-menu))
  ;;The apple menu cannot be removed by the user (it causes multifinder
  ;;to overwrite random memory).
  ;;*menubar-frozen* is true only for system code, it is used when adding
  ;;menu-items to the apple menu (seems not to confuse multifinder).
  nil)




(defun update-windows-menu (menu)
  ;; if menu items are in the right order just leave them there
  ;; conses less and  faster X 1.6 when order-ok, a bit slower when not ok
  (if *modal-dialog-on-top* 
      (menu-disable menu)
      (progn
        (menu-enable menu)
        (niy update-window-menu menu)
        #-(and)
        (without-interrupts
            (let* ((nwins     (length *window-object-alist*))
                   (new-items (make-list nwins))
                   (items     (cddr (slot-value menu 'item-list)))
                   (nitems    0)
                   (order-ok  t))
              (let ((new-items new-items)
                    (items     items))
                (do-wptrs wptr
                  (let* ((w (window-object wptr)))
                    (when (and w (display-in-windows-menu w))
                      (let ((item (window-menu-item w)))
                        (when item
                          (if (window-shown-p w)
                              (set-menu-item-style item :plain)
                              (progn (set-menu-item-style item :italic)
                                     (menu-item-enable item)))
                          (rplaca new-items item)
                          (setf new-items (cdr new-items))
                          (incf nitems)
                          (when (and order-ok (neq item (car items)))
                            (setf order-ok nil))
                          (setf items (cdr items))))))))
              (when (or (not order-ok)(neq nitems (length items))(command-key-p))
                (dolist (item (slot-value menu 'item-list))
                  (setf (slot-value item 'owner) nil)
                                        ; always delete item 1 (they get "renumbered" !)
                  (#_DeleteMenuItem menu-handle 1))
                (setf (slot-value menu 'item-list) nil) 
                (when t                ;(osx-p)
                  (add-menu-items menu *bring-windows-front-item*)
                  (add-menu-items menu (make-instance 'menu-item :menu-item-title "-")))
                (when (command-key-p)
                  ;; 2003-10-08TA - if command key is down then windows sorted alphabetically
                  (let ((copy nil))
                    (dolist (x new-items)
                      (when x (push x copy)))
                    (setf new-items (sort copy #'string-lessp :key #'ccl::menu-title))))
                (dolist (item new-items)
                  (when item ; windoid's & da-window's have no menu-item's                     
                    (add-menu-items menu item)
                    (unless (slot-value item 'enabledp)
                      (menu-item-disable item))))))))))







(defun initialize/menu ()
  (setf *apple-menu*   (make-instance 'apple-menu :title "Apple")
        ;; We should get the menus from the actual Openstep menus:
        *file-menu*    (make-instance 'menu :title "File")
        *edit-menu*    (make-instance 'menu :title "Edit")
        *lisp-menu*    (make-instance 'menu :title "Lisp")
        *tool-menu*    (make-instance 'menu :title "Tools")
        *window-menu*  (make-instance 'menu :menu-title "Windows"
                                      :update-function 'update-windows-menu
                                      :help-spec '(values 1500 (1501 1 2 1 3)))  
        *menubar*      (make-instance 'menubar
                           :menus (list *apple-menu*
                                        *file-menu*
                                        *edit-menu*
                                        *lisp-menu*
                                        *tool-menu*
                                        *window-menu*)))
  (niy initialize-menu))


;;;; THE END ;;;;
