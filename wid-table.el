;;; wid-table.el --- table and cell widgets

;; Copyright (C) 2008, 2009  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20080830
;; Updated: 20090313
;; Version: 0.0.2a
;; Homepage: https://github.com/tarsius/wid-table
;; Keywords: extensions, widgets

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Display tables using the `widget' libary.

;; This was written as support library for library `cus-keymap' but
;; could be useful on it's own.  It's a bit buggy but this shouldn't
;; be to hard to fix.

;; **************************** WARNING ****************************
;; *                                                               *
;; *  This package is somewhat usable but it is not finished and   *
;; *  since I no longer use it this will probably never change.    *
;; *                                                               *
;; *  If you are interested feel free to adopt it - orphans lead   *
;; *  a sad live.                                                  *
;; *                                                               *
;; *****************************************************************

;;; Code:

(require 'cl)
(require 'wid-edit)

(defvar widget-tables 0
  "Number of tables created.
This is used to create unique symbols for each table.")

;;; The `table' Widget.

(define-widget 'table 'default
  "A table."
  :format "%v"
  :create 'widget-table-create
  :delete 'widget-table-delete
  :convert-table 'widget-table-convert
  :value-create 'widget-table-value-create
  :cell-create 'widget-table-cell-create
  :child-create 'widget-table-child-create
  :spacing '(3 "grey60" t)
  :headers nil
  :column-size-default 4
  :column-size-minimum 3
  :column-size-maximum 6)

(defun widget-table-create (widget)
  "Create table WIDGET."
  (widget-apply widget :convert-table)
  (widget-default-create widget)
  (widget-table-spacing-resize widget)
  ;; Prepare buffer for tables:
  (setq truncate-lines t)
  (when (eq buffer-invisibility-spec t)
    (setq buffer-invisibility-spec '(t))))

(defun widget-table-convert (widget)
  "Convert table WIDGET."
  (let* ((value (widget-get widget :value))
	 (headers (widget-get widget :headers))
	 (row-header (if (atom headers) headers (car headers)))
	 (col-header (if (atom headers) headers (cdr headers)))
	 (rows (if col-header
		   (1+ (length value))
		 (length value)))
	 (cols (if row-header
		   (1+ (length (aref value 0)))
		 (length (aref value 0))))
	 (size (make-vector cols nil)))
    (when row-header
      (aset size 0 0))
    (widget-put widget :rows rows)
    (widget-put widget :columns cols)
    (widget-put widget :cells (make-vector rows nil))
    (widget-put widget :column-size size)
    (widget-put widget :column-hidden (make-vector cols nil))
    (widget-put widget :id (incf widget-tables))))

(defun widget-table-value-create (widget)
  "Create value of table WIDGET."
  (let ((inhibit-point-motion-hooks t)
	(rows (widget-get widget :rows))
	(cols (widget-get widget :columns))
	(size (widget-get widget :column-size))
	(cells (widget-get widget :cells))
	(indent (widget-get widget :indent))
	row-sym col-sym)
    (dotimes (row rows)
      (aset cells row (make-vector cols nil))
      (dotimes (col cols)
	(unless (aref size col)
	  (aset size col (max 1
			      (or (widget-column-size-minimum widget col) 0)
			      (or (widget-column-size-default widget col) 0))))
	(let ((cell (widget-apply widget :cell-create row col)))
	  (aset size col (min (max (or (aref size col) 0)
				   (widget-apply cell :field-size))
			      (widget-column-size-maximum widget col)))
	  (aset (aref cells row) col cell))))))

(defun widget-table-cell-create (widget row col)
  "Create and return cell in table WIDGET at position ROW/COL."
  (let* ((headers (widget-get widget :headers))
	 (row-header (if (atom headers) headers (car headers)))
	 (col-header (if (atom headers) headers (cdr headers))))
    (cond ((and row-header col-header
		(= col 0) (= row 0))
	   (widget-create-child-and-convert
	    widget 'table-header
	    :position (cons row col)
	    :value " "))
	  ((and col-header (= row 0))
	   (widget-create-child-and-convert
	    widget 'column-header
	    :position (cons row col)
	    :value col))
	  ((and row-header (= col 0))
	   (widget-create-child-and-convert
	    widget 'row-header
	    :position (cons row col)
	    :value row))
	  (t
	   (widget-apply widget :child-create row col
			 row-header col-header)))))

(defun widget-table-child-create (widget row col row-header col-header)
  "Create and return child cell in table WIDGET at position ROW/COL."
  (let ((children (widget-get widget :children))
	(cell (widget-create-child-and-convert
	       widget 'editable-cell
	       :position (cons row col)
	       :value (aref (aref (widget-get widget :value)
				  (if col-header (1- row) row))
			    (if row-header (1- col) col)))))
    (widget-put widget :children (cons cell children))
    cell))

(defun widget-table-spacing-symbol (widget id)
  (intern (format "widget-table%d-spacing%d" (widget-get widget :id) id)))

(defun widget-table-spacing-resize (widget)
  (let* ((spacing (widget-get widget :spacing))
	 (space (car spacing))
	 (width 0))
    (put (widget-table-spacing-symbol widget 1) 'display
	 `(space :height (,space)
		 :align-to
		 ,(+ (loop for size across (widget-get widget :column-size)
			   for hide across (widget-get widget :column-hidden)
			   unless hide sum size and do (incf width))
		     (or (widget-get widget :indent) 0))))
    (put (widget-table-spacing-symbol widget 2) 'display
	 `(space :height (,space)
		 :width (,(* space (if (caddr spacing)
				       (1+ width)
				     (1- width))))))))

(defun widget-table-delete (widget)
  (let ((cells (widget-get widget :cells)))
    (dotimes (row (widget-get widget :rows))
      (dotimes (col (widget-get widget :columns))
	(widget-delete (aref (aref cells col) row))))))

(defun widget-table-unhide (widget)
  (widget-rows-unhide widget)
  (widget-columns-unhide widget))

;;; Columns in the `table' Widget.

(defun widget-column-cells (table column)
  "Return an ordered list of all cells in COLUMN of TABLE."
  (let ((cells (widget-get table :cells))
	column-cells)
    (dotimes (row (widget-get table :rows))
      (cons (aref (aref cells row) column) column-cells))
    (nreverse column-cells)))

(defun widget-column-symbol (table column)
  (intern (format "widget-table%d-column%d" (widget-get table :id) column)))

(defun widget-column-symbol-column (symbol)
  (let ((string (prin1-to-string symbol)))
    (string-to-number (substring string (string-match "[0-9]*$" string)))))

(defun widget-column-size (table column)
  (aref (widget-get table :column-size) column))

(defun widget-column-size-maximum (table column)
  (let ((maximum (widget-get table :column-size-maximum)))
    (if (consp maximum)
	(aref maximum column)
      maximum)))

(defun widget-column-size-minimum (table column)
  (let ((minimum (widget-get table :column-size-minimum)))
    (if (consp minimum)
	(aref minimum column)
      minimum)))

(defun widget-column-size-default (table column)
  (let ((default (widget-get table :column-size-default)))
    (if (consp default)
	(aref default column)
      default)))

(defun widget-column-size-optimum (table column)
  (let ((optimum (widget-column-size-minimum table column))
	(cells (widget-get table :cells)))
    (dotimes (row (widget-get table :rows))
      (setq optimum
	    (max optimum
		 (widget-apply (aref (aref cells row) column) :value-size))))
    (or optimum (widget-column-size-maximum table column))))

(defun widget-column-resize (table column size)
  (aset (widget-get table :column-size) column size)
  (widget-table-spacing-resize table)
  (let ((cells (widget-get table :cells)))
    (dotimes (row (widget-get table :rows))
      (widget-field-resize (aref (aref cells row) column) size))))

(defun widget-column-maximize (table column)
  (widget-column-resize table column
			(widget-column-size-maximum table column)))

(defun widget-column-optimize (table column)
  (widget-column-resize table column
			(widget-column-size-optimum table column)))

(defun widget-column-minimize (table column)
  (widget-column-resize table column
			(widget-column-size-minimum table column)))

(defun widget-column-hide (table column)
  (add-to-invisibility-spec (widget-column-symbol table column))
  (aset (widget-get table :column-hidden) column t)
  (widget-table-spacing-resize table)
  ;; Kludge.  Column still visible otherwise.
  (redraw-frame (window-frame (selected-window))))

(defun widget-column-unhide (table column)
  (remove-from-invisibility-spec (widget-column-symbol table column))
  (aset (widget-get table :column-hidden) column nil)
  (widget-table-spacing-resize table))

(defun widget-columns-unhide (table)
  (dolist (symbol buffer-invisibility-spec)
    (when (string-match "^widget-table[0-9]+-column[0-9]+$"
			(prin1-to-string symbol))
      (widget-column-unhide table (widget-column-symbol-column symbol)))))

;;; Rows in the `table' Widget.

(defun widget-row-cells (table row)
  "Return an ordered list of all cells in ROW of TABLE."
  (append (aref table row)))

(defun widget-row-symbol (table row)
  (intern (format "widget-table%d-row%d" (widget-get table :id) row)))

(defalias 'widget-row-symbol-row 'widget-column-symbol-column)

(defun widget-row-hide (table row)
  (add-to-invisibility-spec (widget-row-symbol table row))
  ;; Kludge.  Row still visible otherwise.
  (redraw-frame (window-frame (selected-window)))
  ;; Kludge.  See widget-cell-format-handler.
  (let ((inhibit-read-only t)
	(from (widget-get (aref (aref (widget-get table :cells)
				      row) 0) :from)))
    (remove-list-of-text-properties from (1+ from) '(display))))

(defun widget-row-unhide (table row)
  (remove-from-invisibility-spec (widget-row-symbol table row))
  ;; Kludge.  See widget-cell-format-handler.
  (let ((inhibit-read-only t)
	(from (widget-get (aref (aref (widget-get table :cells)
				      row) 0) :from)))
    (put-text-property from (1+ from) 'display 
		       `(space :width (,(car (widget-get table :spacing)))))))

(defun widget-rows-unhide (table)
  (dolist (symbol buffer-invisibility-spec)
    (when (string-match "^widget-table[0-9]+-row[0-9]+$"
			(prin1-to-string symbol))
      (widget-row-unhide table (widget-row-symbol-row symbol)))))

(defun widget-row-spacing-entered (old new)
  (when (eq (get-text-property new 'point-entered)
 	    'widget-row-spacing-entered)
    (if (< old new)
	(goto-char (next-single-property-change new 'point-entered))
      (goto-char (1- (previous-single-property-change new 'point-entered))))))

(defsubst widget-row-spacing-create ()
  (with-no-warnings ; Only used in `widget-cell-create'.
    (insert (propertize
	     (concat
	      (when indent 
		(propertize (make-string indent ?\s)
			    'display `(space :height (,size)
					     :align-to ,indent)))
	      (propertize "*" 'face face
			  'category (widget-table-spacing-symbol table 1))
	      (propertize "*" 'face face
			  'category (widget-table-spacing-symbol table 2))
	      (propertize "\n" 'line-height t))
	     'invisible invisible
	     'point-entered 'widget-row-spacing-entered))))

;;; The `cell' Widget.

(define-widget 'cell 'field
  "Basic widget most non-editable other cell widgets are derived from.
Editable cells typically are derived from `editable-cell' widget."
  :create 'widget-cell-create
  :value-get 'widget-value-value-get
  :field-specify 'widget-specify-cell
  :field-fixed t
  :field-editable nil)

(defun widget-cell-symbols (widget)
  (let ((table (widget-get widget :parent))
	(position (widget-get widget :position)))
    (list (widget-row-symbol table (car position))
	  (widget-column-symbol table (cdr position)))))

(defun widget-cell-create (widget)
  (widget-specify-insert
   (let* ((table (widget-get widget :parent))
	  (rows (widget-get table :rows))
	  (cols (widget-get table :columns))
	  (pos (widget-get widget :position))
	  (row (car pos))
	  (col (cdr pos))
	  (spacing (widget-get table :spacing))
	  (size (car spacing))
	  (face (list :background (cadr spacing)))
	  (edge (caddr spacing))
	  (indent (widget-get table :indent))
	  (invisible (list (widget-row-symbol table (car pos)))))
     (when (= col 0)
       ;; Extra row spacing.
       (when (and edge (= row 0))
	 (widget-row-spacing-create))
       ;; Indent.
       (when indent
	 (insert (propertize (make-string indent ?\s)
			     'invisible invisible
			     'point-entered 'widget-row-spacing-entered)))
       ;; Extra column spacing.
       (when edge
	 (insert (propertize "*" 'face face
			     'display `(space :width (,size))
			     'invisible invisible
			     'point-entered 'widget-row-spacing-entered))))
     ;; Value.
     (widget-apply widget :value-create)
     ;; Column spacing.
     (when (or edge (< col (1- cols)))
       (insert (propertize "*" 'face face
			   'display `(space :width (,size))
			   'invisible (widget-cell-symbols widget))))
     (when (= col (1- cols))
       (if edge
	   (insert (propertize "\n" 'invisible invisible
			       'point-entered 'widget-row-spacing-entered))
	 (insert (propertize "\n" 'invisible invisible)))
       ;; Row spacing.
       (when (or edge (< row (1- rows)))
	 (widget-row-spacing-create))))
   (let ((from (point-min-marker))
	 (to (point-max-marker)))
     (set-marker-insertion-type from t)
     (set-marker-insertion-type to nil)
     (widget-put widget :from from)
     (widget-put widget :to to)))
  (widget-clear-undo))

(defun widget-specify-cell (widget from to)
  "Specify cell WIDGET between FROM and TO."
  (widget-put widget :size
	      (widget-column-size (widget-get widget :parent)
				  (cdr (widget-get widget :position))))
  (widget-specify-field widget from to)
  (overlay-put (widget-get widget :field-overlay)
	       'invisible (widget-cell-symbols widget)))

;;; The `item-cell' Widget.

(define-widget 'item-cell 'cell
  "An item cell.")

;;; The `push-cell' Widget.

(defface widget-push-cell '((t :background "gray30"))
  "Face used for push-cells."
  :group 'widget-faces)

(define-widget 'push-cell 'cell
  "A pushable cell."
  :value-face 'widget-push-cell
  :field-pushable t)

;;; The `table-header' Widget.

(define-widget 'table-header 'push-cell
  "The table header."
  :action 'widget-table-header-action)

(defvar widget-table-header-menu
  '(("Unhide everything"  . widget-table-unhide)
    ("Unhide all columns" . widget-columns-unhide)
    ("Unhide all rows"    . widget-rows-unhide)))

(defun widget-table-header-action (widget &optional event)
  (let ((answer (widget-choose "Operate on table"
			       widget-table-header-menu
			       event)))
    (when answer
      (funcall answer (widget-get widget :parent)))))

;;; The `row-header' Widget.

(define-widget 'row-header 'table-header
  "A row header in a table."
  :action 'widget-row-header-action
  :value-to-internal (lambda (widget value)
		       (prin1-to-string value)))

(defvar widget-row-header-menu
  '(("Hide row" . widget-row-hide)))

(defun widget-row-header-action (widget &optional event)
  (let ((answer (widget-choose "Operate on row"
			       widget-row-header-menu
			       event)))
    (when answer
      (funcall answer (widget-get widget :parent)
	       (car (widget-get widget :position))))))

;;; The `column-header' Widget.

(define-widget 'column-header 'table-header
  "A column header in a table."
  :action 'widget-column-header-action
  :value-to-internal (lambda (widget value)
		       (prin1-to-string value)))

(defvar widget-column-header-menu
  '(("Hide column"     . widget-column-hide)
    ("Optimize column" . widget-column-optimize)
    ("Minimize column" . widget-column-minimize)
    ("Maximize column" . widget-column-maximize)))

(defun widget-column-header-action (widget &optional event)
  (let ((answer (widget-choose "Operate on column"
			       widget-column-header-menu
			       event)))
    (when answer
      (funcall answer (widget-get widget :parent)
	       (cdr (widget-get widget :position))))))

;;; The `editable-cell' Widget.

(define-widget 'editable-cell 'editable-field
  "An editable cell."
  :create 'widget-cell-create
  :value-size 'widget-cell-size
  :field-specify 'widget-specify-cell
  :field-size 'widget-editable-cell-size
  :field-fixed t
  :field-editable t)

(defun widget-editable-cell-size (widget)
  "Return the size of the cell WIDGET."
  3)
;;;   (- (widget-field-end widget)
;;;      (widget-field-start widget)))

;;; The `sexp-cell' Widget.

(define-widget 'sexp-cell 'editable-cell
  "An arbitrary Lisp expression cell."
  :value nil
  :validate 'widget-sexp-validate
  :match (lambda (widget value) t)
  :value-to-internal (lambda (widget value) (prin1-to-string value))
  :value-to-external (lambda (widget value) (read value))
  :prompt-history 'widget-sexp-prompt-value-history
  :prompt-value 'widget-sexp-prompt-value)

(provide 'wid-table)
;;; wid-table.el ends here
