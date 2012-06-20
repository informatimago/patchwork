(in-package "MCLGUI")

(defparameter *application-handle* nil)
(defparameter *test-menu-title-handle* nil)
(defparameter *test-menu-handle* nil)
(defparameter *test-menu-item-handle* nil)


(defun test ()
  
  (setf *application-handle*
        (or *application-handle*
            [NSApplication sharedApplication]))

  (setf *test-menu-title-handle*
        (or *test-menu-title-handle*
            [[NSMenuItem alloc] initWithTitle:@"Test"
             action:*null*
             keyEquivalent:@"T"]))

  (setf *test-menu-handle*
        (or *test-menu-handle*
            [[NSMenu alloc] initWithTitle:@"Test"]))
  [*test-menu-title-handle* setSubmenu:*test-menu-handle*]
  
  (setf *test-menu-item-handle*
        (or *test-menu-item-handle*
            [[NSMenuItem alloc] initWithTitle:@"Test Item"
             action:*null*
             keyEquivalent:@"T"]))
  [*test-menu-handle* addItem:*test-menu-item-handle*]
  
  [[*application-handle* mainMenu] addItem:*test-menu-title-handle*]
  
  [*test-menu-item-handle* setKeyEquivalent: @"t"]
  [*test-menu-item-handle* setState:1]
  
  )
[*test-menu-title-handle* setTitle: '@"New Test"]
'@"New Test"
#<ns-constant-string "New Test" (#x27B0C80)>
