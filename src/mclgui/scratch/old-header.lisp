;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               menu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     OpenStep
;;;;DESCRIPTION
;;;;
;;;;    This file comes from mcl l1-menus.lisp, with all mcl/ccl and
;;;;    MacOS specific stuff removed.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;    akh
;;;;    alice
;;;;    as
;;;;    bill
;;;;    gb    Gary Byers <GaryByers79@gmail.com>
;;;;    gz
;;;;    jaj
;;;;    slh
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Added this header, reformated change log.
;;;;                     Removed any mcl/ccl or MacOS specific stuff.
;;;;    ---- some more
;;;;                     Menus can have unicode titles.  Replace
;;;;                     (#_SetItemMark mh n (%code-char (slot-value item
;;;;                     'menu-id))) with (#_setmenuitemhierarchicalid  mh
;;;;                     n (slot-value item 'menu-id)) so hierarchical
;;;;                     menu can have check-mark - from Takehiko Abe.
;;;;                     Don't do the cfstring business for
;;;;                     font-menu-items.  Deal with initial #\- in
;;;;                     menu-item-title.  Install-menu-item ditto.
;;;;                     Set-menu-item-title does the right thing with
;;;;                     extended-string and non 7bit-ascii .
;;;;                     Menu-item-action ((item windows-menu-menu-item))
;;;;                     - woi - user reported wptr may be gone??
;;;;    ---- 5.1 final
;;;;                     Lose view-font initarg for menu - always
;;;;                     caused an error anyway.  No more
;;;;                     instance-initialize-mixin here thank you,
;;;;                     make mark tapia unhappy?  Now at least
;;;;                     (make-instance 'menu :junk t) will error as
;;;;                     it should.  Allow encoded-string in
;;;;                     instance-initialize of menu-item.
;;;;    ---- 5.1b1
;;;;                     menu may have a menu-font.  menu-update fixes
;;;;                     for ift - actually real problem was in ift.
;;;;                     remove some garbage in
;;;;                     set-menu-item-title-with-cfstr.  worry about
;;;;                     unicode menu items.
;;;;    ---- 5.0final
;;;;                     menu-item-action ((item windows-menu-menu-item))
;;;;                     more likely to dtrt for collapsed windows on OSX.
;;;;                     set-hide-command-key on *lisp-startup-functions*
;;;;                     is conditionalized.
;;;;    ---- 5.0b4
;;;;                     selecting a collapsed window from the windows
;;;;                     menu uncollapses. Thank you.  add
;;;;                     set-hide-command-key and add it to
;;;;                     *lisp-startup-functions*
;;;;    ---- 4.4b5
;;;;                     instance-initialize :after ((menu-item menu-item)
;;;;                     - no more require-type command-key
;;;;    ---- 4.4b4
;;;;    2002-02-11 akh   some stuff about keeping menu-item-icon-type in sync with menu-item-icon-handle
;;;;    ---- 4.4b2
;;;;    2001-07-20 akh   fix set-menu-item-icon-handle and
;;;;                     set-menu-item-script for non carbon - we need
;;;;                     AppearanceLib
;;;;    2001-06-06 akh   see osx-p in menu-item-enable
;;;;    2001-05-05 akh   set-command-key accepts e.g. (:shift #\H)
;;;;                     carbon-compat
;;;;    1999-09-19 akh   added some  stuff for menu-item-icon-handle,
;;;;                     menu-item-icon-type and menu-item-script
;;;;    ---- 4.3f1c1
;;;;    1999-04-22 akh   update-menu-items no longer enables the menu,
;;;;                     update-edit-menu-items is different.  Another tweak
;;;;                     to remove-menu-items, also revert to forgiving a
;;;;                     NIL menu-item
;;;;    ---- 4.3b1
;;;;    1998-08-05 akh   most of set-menubar is without-interrupts - seems to fix a CLIM problem
;;;;    1998-06-09 akh   set-menu-item-title messes with initial #\- just like install-menu-item
;;;;    1998-04-11 akh   install-menu-item passes the item-number to various set-xx fns
;;;;    1998-03-31 akh   minor tweak to remove-menu-items
;;;;    ---- 4.2
;;;;    1997-10-05 akh   see below
;;;;    1997-08-25 akh   optimizations for long menus
;;;;    1997-07-28 akh   set-menu-item-check-mark - do nothing if same cause menu-item-number is slow for lots of items
;;;;    1997-07-04 akh   see below
;;;;    1997-06-02 akh   see below
;;;;    1997-04-17 bill  fix-menu-color-bug does something only if *fix-menu-color-bug* is true.
;;;;                     The default is NIL.
;;;;                     This speeds up set-menubar.
;;;;    1997-04-15 bill  (method menu-install (menu)) calls initialize-menubar-color
;;;;    1996-09-04 akh   conditionalize use of hash table for 3.1 vs 4.0
;;;;    ---- 4.1b2
;;;;    1996-06-03 bill  (method menu-update :around (t)) prevents reentry for a single menu.
;;;;                     This stops Edit menu problems from repeating.
;;;;    1996-05-20 akh   remove some goofy underlining
;;;;    ---- MCL-PPC 3.9
;;;;    1996-03-26 gb    lowmem accessors.
;;;;    1995-11-29 bill  New trap names to avoid emulator.
;;;;    1995-11-09 akh   attach-menu - assure that menus intended to be disabled are in fact disabled
;;;;    1995-04-20 slh   update-menus-for-modal folded into update-menus
;;;;                     menu-update different
;;;;    1995-03-02 akh   changed menu-update to enable if any items enabled????
;;;;    1995-02-09 akh   install-menu-item sets style also
;;;;    1995-02-07 akh   probably no change
;;;;    1995-01-30 akh   dont remember
;;;;    1995-01-30 alice add-menu-items checks if installed-p - from patch
;;;;    1994-12-26 alice make fix-menu-color-bug and set-menubar work
;;;;                     when building from scratch
;;;;                     update-menus-for-modal was brain dead tho it
;;;;                     happened to work in the usual case
;;;;                     menu-enable/disable and set-menu-title pass the
;;;;                     menu to draw-menubar-if draw-menubar-if takes
;;;;                     optional menu arg - draws only when in menubar
;;;;                     (pop-up-menus dont have owners either)
;;;;    1994-12-23 akh   fix update-menus-for-modal again
;;;;    1994-12-29 akh   merge with d13
;;;;    1994-06-29 bill  set-menubar works around a system 7.1.1 bug in menu colors.
;;;;    1993-10-12 alice menu-item-action ((item windows-menu-menu-item))
;;;;                     - control brings class front wards
;;;;    ---- 3.0d13
;;;;    1993-05-04 alice 'character -> 'base-char, make-menu-item second arg is optional
;;;;    1993-05-04 bill  *menu-id-object-table* replaces *menu-id-object-alist*
;;;;                     Faster algorithm for allocate-menu-id
;;;;    1993-04-30 bill  in window-menu-item methods: front-window ->
;;;;                     get-window-event-handler bootstrapping version
;;;;                     on get-window-event-handler
;;;;    ---- 2.1d4
;;;;    1993-02-03 alice instance-initialize for menu is primary instead
;;;;                     of :after so item-list is set  before
;;;;                     view-default-size is called by the after method
;;;;                     on simple-view  (pop-ups care)
;;;;    1992-12-10 bill  menu-item-icon-num slot for menu-item's.
;;;;                     initarg is :icon-num, read by
;;;;                     menu-item-icon-num, set by
;;;;                     set-menu-item-icon-num Thanx to Steve Mitchell
;;;;                     for the initial version of the icon code.
;;;;    1992-07-30 alice change update-menu-items so menu state is truly
;;;;                     fn of state of items
;;;;    ---- 2.0
;;;;    1991-10-30 bill  remove "-iv" on the end of slot names
;;;;    1991-10-16 bill  Selecting a hierarchical menu instead of one of its items
;;;;                     no longer causes an error.
;;;;    1991-10-15 bill  menu-items no longer conses if it doesn't need to.
;;;;    1991-09-25 alice more menu-update fiddling
;;;;    ---- 2.0b3
;;;;    1991-09-05 alice another menu-update tweak
;;;;    1991-09-04 alice menu-update and menu-item-update tweaks
;;;;    1991-08-27 alice menu-update - enable item then call menu-item-update
;;;;    1991-08-23 alice menu-update - let updater decide
;;;;    1991-08-24 gb    use new trap syntax.
;;;;    1991-07-11 bill  leading "-" in menu-item title becomes n-dash so the damn
;;;;                     ROM won't interpret it as a seperator line.
;;;;    1991-08-12 alice menu-disable - (when enabled ...), menu-enable too - looks better, goes faster
;;;;    1991-08-12 alice dim-if-undefined look at handler if any
;;;;    1991-06-10 bill  install-menu-item no longer causes all menu-items below a
;;;;                     menu-item with a title of "" to display as "xxx".
;;;;    1991-06-07 bill  help-spec for menu-elements.
;;;;    ---- 2.0b2
;;;;    1991-01-28 bill  add set-menu-item-action-function
;;;;    1991-01-18 bill  set-menu-item-update-function no longer a :writer (wrong arg order)
;;;;    1991-01-04 bill  instance-initialize for menu-item take menu-item-title as well
;;;;                     as menu-title.
;;;;    1990-12-11 bill  (setf menu-item-update-function) -> set-menu-item-update-function
;;;;    1990-10-03 bill  %class-cpl -> %inited-class-cpl
;;;;    1990-08-27 bill  install-menu-item: wrong sense on test of (slot-value item 'checkedp)
;;;;    1990-07-23 bill  proper return value for set-menu-item-check-mark
;;;;    1990-07-05 bill  menu-item-update-function & menu-update-function
;;;;    1990-06-18 bill  call window-ensure-on-screen if windows-menu-menu-item is
;;;;                     selected with the shift key down.
;;;;    1990-05-30 gb    Use print-unreadable-object.
;;;;    1990-05-04 bill  _DelMCEntries only if *color-available*
;;;;    1990-03-26 bill  Use "_InsMenuItem instead of _AppendMenu so that adds to the
;;;;                     apple-menu will work.  No longer need add-menu-items specialization
;;;;                     for apple-menu.
;;;;    1990-03-20 bill  initialize-instance => instance-initialize.
;;;;    1990-03-17 bill  add readers to menu-element and menu-item classes.
;;;;    1990-03-13 bill  menu-owner => menu-item-owner
;;;;    1990-02-27 bill  Add menu-owner method.
;;;;    1989-12-27 gz    apple-menu-class -> apple-menu.  Remove obsolete #-bccl conditionals.
;;;;    1989-12-15 bill  set-command-key: fix error on setting to NIL.
;;;;    1989-10-11 as    MENU-ITEMS defaults class to MENU-ELEMENT
;;;;    1989-10-03 gz    with-menu-detached returns
;;;;    1989-09-27 gb    simple-string -> ensure-simple-string.
;;;;    1989-09-16 bill  Removed the last vestiges of object-lisp windows.
;;;;    1989-09-31 bill  menu-item-action & dim-if-undefined: make work for CLOS windows.
;;;;    1989-08-25 bill  dim-if-undefined: Alwas dim unless the window is an object.
;;;;                     This needs to be modified to work with CLOS windows.
;;;;    1989-07-28 gz    Use :default-initargs for apple-menu-class.
;;;;    1989-07-20 gz    closified.
;;;;    1989-03-18 as    from 1.3: uninstalled menus are gc-able
;;;;                               flushed dispose-menu, menus
;;;;                               color support
;;;;    1989-03-17 gz    CLOS window-select syntax.
;;;;    1989-03-02 as    removed window-selection-menu-item
;;;;                     added windows-menu-menu-item
;;;;    1988-11-19 gb    dispatch on functionp or symbolp.
;;;;    1988-10-27 gb    char-int -> %char-code.
;;;;    1988-09-19 as    (unless *menubar-frozen* (_drawmenubar)) -->> (draw-menubar-if)
;;;;    1988-08-21 gz    declarations
;;;;    1988-08-06 gb    make (menu-install *apple-menu-class*) check macptrp-ness.
;;;;                     stack-cons.
;;;;    1988-06-28 jaj   re-init menup to nil in add-menu-items
;;;;    1988-06-06 jaj   added *window-selection-menu-item* fns
;;;;    1988-05-19 as    brought over from Beany
;;;;    1988-04-21 jaj   more changes for apple menu (cannot be deinstalled except
;;;;                     by system code), double install is okay.
;;;;    1988-04-20 jaj   set-menubar doesn't remove apple-menu (for multifinder)
;;;;                     added menu-deinstall for apple-menu
;;;;    1988-04-10 jaj   did solo defobfuns
;;;;    1988-04-06 as    added definitions for *apple-menu-class*
;;;;                     hierarchical menus dump properly
;;;;    1988-03-31 as    moved with-menu-detached to l1-macros
;;;;                     removed add-item, remove-item
;;;;    1988-03-30 as    replaced call to with-menubar-frozen with let
;;;;                     changed with-detached-menu to not use detach-menu
;;;;                     punted detach-menu, domenu
;;;;    1988-03-30 gz    New macptr scheme. Flushed pre-1.0 edit history.
;;;;    1988-03-02 gz    Eliminate compiler warnings
;;;;BUGS
;;;;LEGAL
;;;;    LGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    Copyright Digitool, Inc.        1995 - 1999
;;;;    Copyright Apple Computer, Inc.  1989 - 1994
;;;;    Copyright Coral Software Corp.  1986 - 1988
;;;;
;;;;    This library is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Lesser General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 3 of the License, or (at your option) any later
;;;;    version.
;;;;
;;;;    This library is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU Lesser General Public License for more
;;;;    details.
;;;;
;;;;    You should have received a copy of the  GNU Lesser General
;;;;    Public License along with this library.
;;;;    If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
