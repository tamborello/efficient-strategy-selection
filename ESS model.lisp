;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Frank Tamborello
;;; Copyright   : (c) 2013 Frank Tamborello, Cogscent, LLC
;;; Address     : Cogscent, LLC
;;;		: PMB 7431
;;;		: 2711 Centerville Rd, Ste 120
;;;		: Wilmington DE 19808
;;;		: United States
;;;		: frank.tamborello@cogscent.com
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser General Public
;;; License: the GNU Lesser General Public License as published by the
;;; Free Software Foundation (either version 2.1 of the License, 
;;; or, at your option, any later version),
;;; and the Franz, Inc Lisp-specific preamble.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the Lisp Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;; and see Franz, Inc.'s preamble to the GNU Lesser General Public License,
;;; http://opensource.franz.com/preamble.html.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : ESS model.lisp
;;; Revision     : 1
;;; 
;;; Description : A model of behavior change with regard to efficient strategy 
;;;		selection.
;;;		Developed with ACT-R6 (r1227).
;;;
;;; 
;;; Bugs        : 
;;;
;;;
;;; To do       : 		
;;;
;;;
;;;
;;;
;;; 
;;; ----- History -----
;;;	2013.02.23	fpt	r1
;;;		: 1. Inception
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load ACT-R
;; (load "/Users/frank/Documents/ESS/Manuscript/r3/Goal Competition Model/actr6/load-act-r-6.lisp")

(defparameter *methods* '(icon))



(defvar *vis-locs*)

(defvar *exp* "The task environment.")

(defparameter 
    *data-column-headings* 
  "run	trial	kind	got	SCL")
; run#, trial#, trial-type/task-id, procedure step# (zero-indexed), the model's 
; action, got step# - expected step#, the
; model's action was/was not correct, Step Completion
; Latency




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Task Environment: Office Productivity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :virtual-experiment-window)


(defclass exp-window (virtual-event-exp-window)
  ((n-of-k :accessor n-of-k :initarg :n-of-k :initform nil)))

;; the task's virtual window
(defclass word-processor (procedure-window trial)
  ((widgets :accessor widgets :initarg :widgets :initform nil)
   (mouse-pos 
    :accessor mouse-pos 
    :initarg :mouse-pos 
    :initform (vector (act-r-random 1920) (act-r-random 1200)))))



(defclass wp-action (proc-action)
  ((trial-kind :accessor trial-kind :initarg :trial-kind :initform nil)
   (got-num :accessor got-num :initarg :got-num :initform nil)
   (mthd :accessor mthd :initarg :mthd :initform nil)))


;; widgets of the task window
(defclass widget ()
  ((nick-name :accessor nick-name :initarg :nick-name :initform nil)
   (vwindow :accessor vwindow :initarg :vwindow :initform nil)
   (vis-loc :accessor vis-loc :initarg :vis-loc :initform nil)
   (vis-obj :accessor vis-obj :initarg :vis-obj :initform nil)))



(defun make-trials (num kind wind)
  (let (accum)
  (dotimes (i num accum)
          (push 
           (make-instance 
               'word-processor 
             :out-path (data-file wind)
             :task-id kind
             :trial-kind kind)
                accum))))

(defun build-basic-trials (wind)
  (let ((stim-lst nil)
        (n (n-of-k wind))) ; n per kind
    (dotimes (k (length n) (nreverse (flatten stim-lst)))
      (when (> (svref n k) 0)
        (push
         (make-trials (svref n k) k wind)
         stim-lst)))))

(defun do-stimgen (wind)
  (list 
     (let ((stim-lst (build-basic-trials wind)))
       (make-instance 'trial-block
         :size (length stim-lst)
         :trial-lst stim-lst))))

(defmethod initialize-instance :after ((wind virtual-event-exp-window) &key)
  (setf (base-path wind) "/Users/frank/Documents/ESS/Manuscript/r3/Goal Competition Model/"
        (block-lst wind) (do-stimgen wind)
        (nblocks wind) 1
        (write-type wind) :SS))



(defmethod make-data-path ((wind exp-window) type)
  (when (base-path wind)
    (make-pathname :directory (base-path wind)
                   :name
                   (format nil "+model-data")
                   :type type)))

(defmethod setup-trial :after ((wind virtual-event-exp-window) (trl trial))
  (install-device trl)
  (set-cursor-position-fct (vector (act-r-random 1920) (act-r-random 1200)))
  (proc-display :clear t)
  (goal-focus copy) 
;; intervention
  (when (eq (task-id trl) 1)
    ; boost copy-kic's BLA
    (push (mp-time-ms) (chunk-reference-list 'copy-kic))
    (schedule-event-relative 0 (lambda () (finish-trial wind)))
    ; put copy-kic in imaginal buffer chunk
#|    (schedule-set-buffer-chunk 
     'imaginal 
     (car
      (define-chunks-fct
          '((isa word-process
                 command copy-kic))))
     0 
     :module 'imaginal) |#
    ; reward a production that boosts kic's BLA by simply rehearsing it
    ; - when retrieval is free & empty, request retrieval of kic
    ; - eventually this production gets compiled into one that matches need
    ; to issue the command with requesting the motor movement
    )
;; post-intervention
#|  (when (eq (task-id trl) 2)
    ; clear the imaginal buffer
    (schedule-clear-buffer 'imaginal 0 :module 'imaginal)) |#
  (start-timing (timer (current-trial *exp*))))





;; methods for working with widgets
(defgeneric widget-named (procedure-window name)
  (:documentation "Widget-named returns the first widget of virtual-
experiment-window whose name is name."))

(defmethod widget-named ((pw procedure-window) name)
  (labels
      ((wn-helper (lst name)
         (if (null lst) nil
           (let* ((wgt (car lst))
                  (wnn (nick-name wgt)))
             (if
                 (eq name wnn) wgt
               (wn-helper (cdr lst) name))))))
    (wn-helper (widgets pw) name)))

;; methods for detecting where the model clicked

(defgeneric current-widget (device loc)
  (:documentation "Given a device and a location, return a widget containing 
  the location else nil if no widget contains that location."))

(defun inside (loc vl)
  "Takes a display coordinate as a vector and a visual-location chunk name, 
  returns t if the display coordinate is inside the area of the named 
  visual-location chunk."
  (let* ((x1 (chunk-slot-value-fct vl 'screen-x))
         (x2 (+ x1 (chunk-slot-value-fct vl 'width)))
         (y1 (chunk-slot-value-fct vl 'screen-y))
         (y2 (+ y1 (chunk-slot-value-fct vl 'height))))
    (and (>= (svref loc 0) x1)
         (<= (svref loc 0) x2)
         (>= (svref loc 1) y1)
         (<= (svref loc 1) y2))))

(defmethod current-widget ((device procedure-window) (loc vector))
  (labels
      ((current-widget-helper (widgets loc)
         (cond
          ((null widgets) nil)
          ((inside loc (vis-loc (car widgets))) (car widgets))
          (t (current-widget-helper (cdr widgets) loc)))))
  (current-widget-helper (widgets device) loc)))




(defmethod initialize-instance :after ((wind word-processor) &key)
  "Initialize an instance of the word-processor device class. Make the visual-
object chunks, encapsulate them and the visual-location chunks in widgets. Set
the state-vec from the keywords made for the nick-names of the widgets."
    (let
        ((vis-objs
          (define-chunks-fct
              (let ((vo nil))
                (dotimes
                    (i (length *methods*) (nreverse vo))
                  (push
                   `(isa visual-object 
                         screen-pos ,(nth i *vis-locs*)
                         color white)
                   vo))))))
      
      (let ((wgts nil))
        (dotimes (i (length *methods*) (setf (widgets wind) (nreverse wgts)))
          (push
           (make-instance 'widget 
             :vwindow wind 
             :nick-name (intern (string (nth i *methods*)) "KEYWORD")
             :vis-loc (nth i *vis-locs*) 
             :vis-obj (nth i vis-objs))
           wgts))))

;; set the state vector of the procedure-window
  (setf (state-vec wind) #(:copy)))

(defmethod finish-task ((wind procedure-window))
  (write-log wind)
  (finish-trial *exp*))

(defmethod finish-trial ((wind exp-window))
  (let ((curr-block (nth (cblock wind) (block-lst wind))))
    (incf (current-idx curr-block))
    (incf (completed-trials wind))
    (if (= (size curr-block) (current-idx curr-block))
      (finish-block wind curr-block)
      (progn
        (setf (current-trial wind)
              (nth (current-idx curr-block) (trial-lst curr-block)))
        (setup-trial wind (current-trial wind))))))

(defmethod state-check ((wind word-processor) state-name &optional info)
  (declare (ignore info))
    (let ((wp-act (make-instance 'wp-action
                      :latency (round (start-stop-timer (timer wind)))
                      :got state-name
                      :trial-kind (task-id wind))))
        (push wp-act (action-log wind)))
  (finish-task wind))


(defmethod write-pa ((wp-act wp-action) &optional (strm t))
  (let ((out-lst 
         (list 
          (snum *exp*)
          (completed-trials *exp*)
          (trial-kind wp-act)
          (step-num wp-act)
          (got wp-act))))
    (terpri strm)
    (tab-output out-lst strm)
;; Tab-output writes a tab after every item, but reading an empty column into
;; R is sort of annoying,
;; so write the last thing directly into the stream rather than through 
;; tab-output.
    (format strm "~S" (latency wp-act))))

(defmethod write-events ((wind word-processor) &optional (strm t))
    (dolist (p-act (reverse (action-log wind)))
      (write-pa p-act strm)))


;; Either this or delete write-block from virtual-experiment-window.lisp
(defmethod write-block ((wind word-processor) (blk trial-block))
  nil)


;;;; ---------------------------------------------------------------------- ;;;;
;;;;   ACT-R Device Handler Methods
;;;; ---------------------------------------------------------------------- ;;;;

(defmethod build-vis-locs-for ((device procedure-window) vismod)
  (declare (ignore vismod))
  (labels ((wdgt-lst (lst)
             (if (null lst)
               nil
               (cons (vis-loc (car lst)) (wdgt-lst (cdr lst))))))
    (wdgt-lst (widgets device))))

(defmethod vis-loc-to-obj ((device procedure-window) vl)
  "Returns the vis-obj of the widget containing the vis-loc."
  (labels ((get-vis-obj (lst)
             (cond 
               ((null lst) nil)
               ((eq (vis-loc (car lst)) vl) (vis-obj (car lst)))
               (t (get-vis-obj (cdr lst))))))
  (get-vis-obj (widgets device))))
               

(defmethod device-move-cursor-to ((device procedure-window) loc) 
  (setf (mouse-pos device) loc))

(defmethod get-mouse-coordinates ((device procedure-window))
  (mouse-pos device))

(defmethod device-handle-click ((device procedure-window))
  (awhen (current-widget device (get-mouse-coordinates device)) 
         (progn
           (model-output "~%Model clicked ~A.~%" (nick-name it)) 
           (state-check device (nick-name it)))))

(defmethod device-handle-keypress ((device procedure-window) key)
  (declare (ignore key))
  (state-check device :KIC))


;;;; ---------------------------------------------------------------------- ;;;;
;;;;   Testing & Running 
;;;; ---------------------------------------------------------------------- ;;;;
(defun run-model (&optional (n 1000) (k #(49 55 60)))
  "Do n runs of the model through the experiment with k trials of pre-
  intervention, intervention, and post-intervention trials, respectively."
  (dotimes (i n)
    (progn
      (reset)
      (setf *exp* (make-instance 'exp-window :n-of-k k :snum i))
      (format t "~%Run #: ~A~%" (snum *exp*))
      (run-experiment *exp*)
      (with-open-file (strm (out-path (current-trial *exp*))
                            :direction :output 
                            :if-exists nil
                            :if-does-not-exist :create)
        (format strm "~A" *data-column-headings*))
      (run-until-condition (lambda () nil)))))


(defmethod finish-experiment :after ((wind virtual-experiment-window))
  (schedule-break-after-all :details "…the end."))








;;;; ---------------------------------------------------------------------- ;;;;
;;;;   The Model
;;;; ---------------------------------------------------------------------- ;;;;

(defparameter *mas* 1)

; (sdp-fct '((copy-icon :base-level) (copy-kic :base-level)))
; (sdp-fct '((copy-icon :reference-list) (copy-kic :reference-list)))

(clear-all)

(define-model ess

  (sgp-fct 
   (list
    ;; model debugging & running
    :v nil :trace-detail 'medium

    ;; central parameters
    :er t :esc t

    ;; procedural
    :crt nil :cst nil

    ;; declarative
    :ans .3
    :rt -10 
    :bll 0.5 :mas *mas* :ol nil
    :act 'medium

    :imaginal-activation 1

    :do-not-harvest 'visual
    :do-not-harvest 'visual-location
    :do-not-harvest 'imaginal))

  (mapcar 
   'chunk-type-fct 
   '((word-process operator command)
;     (command name key screen-x screen-y color kind)))
     (cmd-mthd cmd)
     ((kic (:include cmd-mthd)) key)
     ((icon (:include cmd-mthd)) screen-x screen-y color kind)))



;; need visual-locations for the procedure-steps and the device
  (setf 
   *vis-locs* 
   (add-dm-fct 
    `((isa visual-location
           screen-x 50
           screen-y 50
           kind rectangle 
           height 20 
           width 50 
           color white
           ))))

  (add-dm-fct 
   (let*
       ((cks 
         `((copy
            isa word-process
            operator retrieve-cm
            command copy)
           (copy-kic isa kic
                     cmd copy
                     key c)
           (copy-icon isa icon
                      cmd copy
                      screen-x 50
                      screen-y 50
                      color white
;                      kind rectangle
                      ))))
     cks))
  


;; Model's a priori biases
  (let ((kic-n 7500)
        (kic-interval 5)
        (icon-n 10000)
        (icon-interval 5)
        (kic-ref-lst nil)
        (icon-ref-lst nil))

    (sdp-fct 
     `((copy-kic :creation-time ,(* -1 kic-interval kic-n))
       (copy-kic :reference-list ,(dotimes (i kic-n (cdr (nreverse kic-ref-lst)))
; (format t "kic-n: ~A~%kic-interval: ~A~%kic-ref-lst: ~A~%" kic-n kic-interval kic-ref-lst)
                                    (push (* -1 i kic-interval) kic-ref-lst)))
       (copy-icon :creation-time ,(* -1 icon-interval icon-n))
       (copy-icon :reference-list ,(dotimes (i icon-n (nreverse icon-ref-lst))
                                     (push (* -1 i icon-interval) icon-ref-lst))))))
    

  
  (start-hand-at-mouse)

  


;;;
;; Productions
;;;
(p retrieve-command-method
   =goal>
	isa word-process
        operator retrieve-cm
        command =c
   ?retrieval>
	state free
==>
   =goal>
	operator retrieving-cm
   +retrieval>
	isa cmd-mthd
        cmd =c
)

;; Retrieved kic method, so do it
(p retrieved-kic-cmd-mthd
   =goal>
	isa word-process
	operator retrieving-cm
   =retrieval>
	isa kic
        key =k
==>
   +manual>
	isa press-key
        key =k
   =goal>
	operator acting)



;; Retrieved icon method, so click it
(p find-step-from-retrieved-icon-cmd-mthd
   =goal>
	isa word-process
	operator retrieving-cm
   =retrieval>
	isa icon
        screen-x =x
        screen-y =y
        color =c
;        kind =k
==>
   =goal>
	operator finding
   +visual-location> 
	isa visual-location
        screen-x =x
        screen-y =y
        color =c
;        kind =k
   =retrieval>)

;; Move
(p move-visual-attention-and-cursor
   "There's an icon, move visual-attention and the cursor to it."
   =goal>
	isa word-process
   	operator finding
   =retrieval>
   	isa icon
        screen-x =x
        screen-y =y
        color =c
;        kind =k
   =visual-location>
   	isa visual-location
        screen-x =x
        screen-y =y
        color =c
;        kind =k
   ?visual>
   	state free
   ?manual>
   	state free
==>
   =goal>
   	operator act
   +visual>
   	isa move-attention
   	screen-pos =visual-location
   +manual>
   	isa move-cursor
   	loc =visual-location)


;; Click the icon
(p click-mouse
   =goal>
   	isa word-process
   	operator act
   =visual> 
	isa visual-object
        color white
   ?manual>
   	state free
==>
   =goal>
   	operator acting
   +manual> 
	isa click-mouse)






)




