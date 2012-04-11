;;;; -*- mode:lisp; coding:utf-8 -*-
(in-package "C-TABLE-WINDOW-H")

(defun make-lst-ed-help-window ()
  (let (scroller)
    (setf *lst-ed-box-help-window*
          (make-instance 'pw::C-pw-help-window :window-title "List-editor help" :GROW-ICON-P t
                         :view-position (make-point 50 25) :view-size (make-point 550 455) :close-box-p nil))
    (setq scroller 
          (make-instance 'pw::C-pw-help-window-view :view-size
                         (make-point (- 550 8) (- 455 16)) 
                         :view-container *lst-ed-box-help-window* :v-scrollp t :h-scrollp nil 
                         :track-thumb-p t))
    (add-subviews scroller
                  (make-instance 'static-text-dialog-item :view-position (make-point 5 5)
                                 :view-font '("monaco" 9 :srcor)
                                 :dialog-item-text 
                                 
                                 "lst-ed Editor Keyboard Commands

Upon opening, the editor presents a small two-line, two-column table. 
The commands to edit the table are the following: 

To edit each cell, click twice on the cell, type the desired values and then hit return.

To add cells (lines or columns), it is necessary to first select a cell 
(i.e., position the cursor on the cell and click once).
After selecting the cell one has access to the following commands:
      To add a cell before the current cell:   hit '->'
      To add a cell after the current cell:   hit '<-'
      To add a cell above:   hit Up-arrow
      To add a cell below:   hit Down-arrow

      To cut a cell, select it and hit BACK-SPACE ('<-' above the return key)

      To add a column in front:   SHIFT '->'
      To add a column in back:   SHIFT '<-'
      To add a line above:   SHIFT Up-arrow
      To add a line below:   SHIFT Down-arrow

The addition of cells, lines, or columns causes a cell to be opened, which must be edited. 
Type the values desired and then type Return immediately. 
The evaluation of this module returns a list of lists where each sublist corresponds to a column. 
The first elements of each column are the list headings. 
Entering a list of lists in the module formats the table anew. 
It is possible to edit either numbers or symbols but it is not possible to edit parentheses! 
Observation 2: It is possible to save the module, independently of the patch, 
by choosing the Save option in the front menu. 
To open the front menu, move the cursor to the A and click once. 

"))))
