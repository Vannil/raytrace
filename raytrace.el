;;; raytrace.el --- Generate and visualize 3D images -*- lexical-binding: t -*-

;; Copyright (C) 2016 Alessio Vanni

;; Author: Alessio Vanni <vannilla@firemail.cc>
;; Created: February 2016
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: ((xelb "0.1"))
;; URL: https://github.com/Vannil/raytrace.el

;; This file is not part of GNU Emacs.

;; Raytrace is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Raytrace is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'xcb)
(require 'xcb-keysyms)
(require 'color)

(defvar raytrace-connection nil
  "Connection with the X server.")

;; The ray-tracing algorithm is taken from
;; Paul Graham's book "ANSI Common Lisp"
;; It was changed a bit to better suit Emacs Lisp
;; rather than Common Lisp.

(defconst raytrace-camera (list 0.0 0.0 200.0)
  "The camera position in the space.")

(defun raytrace-vector-length (x y z)
  "Return the length of a vector given its X, Y, and Z coordinates."
  (sqrt (+ (* x x) (* y y) (* z z))))

(defun raytrace-unit-vector (x y z)
  "Return a vector of distance 1 with coordinates X, Y, and Z.
The vector is returned as a list of three elements."
  (let ((d (raytrace-vector-length x y z)))
    (mapcar #'float (list (/ x d) (/ y d) (/ z d)))))

(defun raytrace-distance (p1 p2)
  "Return the distance between point P1 and point P2.
P1 and P2 are lists of three elements."
  (if (and p1 p2
	   (listp p1) (listp p2)
	   (eql (length p1) 3) (eql (length p2) 3))
      (raytrace-vector-length (- (nth 0 p1) (nth 0 p2))
			      (- (nth 1 p1) (nth 1 p2))
			      (- (nth 2 p1) (nth 2 p2)))
    (error "[Raytrace] Wrong arguments")))

(defun raytrace-smallest-root (a b c)
  "Return the smallest number for which Ax²+Bx+C = 0."
  (unless (zerop a)
    (setf a (float a)
	  b (float b)
	  c (float c))
    (/ (- c) b)
    (let ((delta (- (* b b) (* 4 a c))))
      (unless (minusp delta)
	(let ((dsqrt (sqrt delta)))
	  (min (/ (+ (- b) dsqrt) (* 2 a))
	       (/ (- (- b) dsqrt) (* 2 a))))))))

(defun raytrace-intersect (surface camera ray)
  (let ((c (car surface))
	(r (cadr surface))
	(xr (nth 0 ray))
	(yr (nth 1 ray))
	(zr (nth 2 ray))
	(xc (nth 0 camera))
	(yc (nth 1 camera))
	(zc (nth 2 camera)))
    (let ((n (raytrace-smallest-root (+ (* xr xr) (* yr yr) (* zr zr))
				     (* 2 (+ (* (- xc (nth 0 c)) xr)
					     (* (- yc (nth 1 c)) yr)
					     (* (- zc (nth 2 c)) zr)))
				     (- (+ (expt (- xc (nth 0 c)) 2)
					   (expt (- yc (nth 1 c)) 2)
					   (expt (- zc (nth 2 c)) 2))
					(* r r)))))
      (if n
	  (list (+ xc (* n xr))
		(+ yc (* n yr))
		(+ zc (* n zr)))))))

(defun raytrace-normal (surface intersect)
  (let ((c (car surface)))
    (raytrace-unit-vector (- (nth 0 c) (nth 0 intersect))
			  (- (nth 1 c) (nth 1 intersect))
			  (- (nth 2 c) (nth 2 intersect)))))

(defun raytrace-first-hit (camera ray world)
  (let (surface
	hit
	dist)
    (dolist (s world)
      (let ((h (raytrace-intersect s camera ray)))
	(when h
	  (let ((d (raytrace-distance h camera)))
	    (when (or (null dist) (< d dist))
	      (setq surface s)
	      (setq hit h)
	      (setq distance d))))))
    (delq nil (cons surface hit))))

(defun raytrace-lambert (surface intersect ray)
  (let ((normal (raytrace-normal surface intersect)))
    (max 0 (+ (* (nth 0 ray) (nth 0 normal))
	      (* (nth 1 ray) (nth 1 normal))
	      (* (nth 2 ray) (nth 2 normal))))))

(defun raytrace-send-ray (camera ray world)
  (let ((s (raytrace-first-hit camera ray world)))
    (if s
	(raytrace-lambert (car s) (cdr s) ray)
      0)))

(defun raytrace-color-at (x y world)
  (let ((vect (raytrace-unit-vector (- (float x) (nth 0 raytrace-camera))
				    (- (float y) (nth 1 raytrace-camera))
				    (- (float 0) (nth 2 raytrace-camera)))))
    (raytrace-send-ray raytrace-camera vect world)))

;; Copied algorithm ends here

(defun raytrace--color-value-to-number (r g b)
  (let ((color (color-rgb-to-hex r g b)))
    (string-to-number (substring color 1) 16)))

(defun raytrace--open-window (id w h root)
  (let ((display (oref root root))
	(visual (oref root root-visual))
	(name (generate-new-buffer-name "RayTrace")))
    (xcb:+request raytrace-connection
	(make-instance xcb:CreateWindow
		       :depth xcb:WindowClass:CopyFromParent
		       :wid id
		       :parent display
		       :x 0 :y 0
		       :width w
		       :height h
		       :border-width 1
		       :class xcb:WindowClass:InputOutput
		       :visual visual
		       :value-mask (logior xcb:CW:BackPixel
					   xcb:CW:EventMask)
		       :background-pixel (raytrace--color-value-to-number
					  0.0 0.0 0.0)
		       :event-mask (logior xcb:EventMask:Exposure
					   xcb:EventMask:StructureNotify
					   xcb:EventMask:KeyPress)))
    (xcb:+request raytrace-connection
	(make-instance xcb:ChangeProperty
		       :mode xcb:PropMode:Replace
		       :window id
		       :property xcb:Atom:WM_NAME
		       :type xcb:Atom:STRING
		       :format 8
		       :data-len (length name)
		       :data name))
    (xcb:+request raytrace-connection
	(make-instance xcb:MapWindow
		       :window id))))

(defun raytrace--open-gc (id window)
  (xcb:+request raytrace-connection
      (make-instance xcb:CreateGC
		     :cid id
		     :drawable window
		     :value-mask xcb:GC:Foreground
		     :foreground (raytrace--color-value-to-number
				  0.0 0.0 0.0))))

(defun raytrace-close (data fake)
  (let ((ev (make-instance xcb:KeyPress)))
    (xcb:unmarshal ev data)
    (with-slots (detail event state) ev
      (let ((key (xcb:keysyms:keycode->keysym raytrace-connection
					      detail state)))
	(when (or (eql key ?q) (eql key ?Q))
	  (xcb:+request raytrace-connection
	      (make-instance xcb:SendEvent
			     :propagate 0
			     :destination event
			     :event-mask xcb:EventMask:StructureNotify
			     :event (list
				     (xcb:marshal
				      (make-instance xcb:DestroyNotify
						     :event event
						     :window event)
				      raytrace-connection))))
	  (xcb:flush raytrace-connection))))))

(defun raytrace-draw (list window gc)
  (lambda (a b)
    (let ((pt (pop list)))
      (unless (null pt)
	(message (format "%S" pt))
	(let ((x (nth 0 pt))
	      (y (nth 1 pt))
	      (v (nth 2 pt)))
	  (xcb:+request raytrace-connection
	      (make-instance
	       xcb:ChangeGC
	       :gc gc
	       :value-mask xcb:GC:Foreground
	       :foreground (raytrace--color-value-to-number
			    v v v)))
	  (xcb:+request raytrace-connection
	      (make-instance
	       xcb:PolyPoint
	       :coordinate-mode xcb:CoordMode:Origin
	       :drawable window
	       :gc gc
	       :points (list
			(make-instance xcb:POINT
				       :x x
				       :y y)))))))
    (xcb:flush raytrace-connection)))

(defun raytrace-tracer (name world)
  (let ((l ())
	(w 150)
	(h 150))
    (message "Tracing...")
    (dotimes (y h)
      (dotimes (x w)
	(let ((v (raytrace-color-at x y world)))
	  (unless (zerop v)
	    (push (list x y v) l)))))
    (message "Tracing...done")
;;    (setq l (reverse l))
    (unless raytrace-connection
      (message "Connecting...")
      (setq raytrace-connection (xcb:connect-to-socket))
      (xcb:keysyms:init raytrace-connection)
      (message "Connecting...done"))
    (let ((setup (xcb:get-setup raytrace-connection))
	  (window (xcb:generate-id raytrace-connection))
	  (gc (xcb:generate-id raytrace-connection)))
      (let ((screen (car (oref setup roots))))
	(raytrace--open-window window w h screen)
	(raytrace--open-gc gc window))
      ;; There is probably a better way to pass values around
      ;; without using global variables.
      ;; Until then, closures.
      (xcb:+event raytrace-connection xcb:DestroyNotify
		  (lambda (a b)
		    (message "Closing...")
		    (xcb:+request raytrace-connection
			(make-instance xcb:FreeGC
				       :gc gc))
		    (xcb:+request raytrace-connection
			(make-instance xcb:DestroyWindow
				       :window window))
		    (xcb:flush raytrace-connection)))
      (xcb:+event raytrace-connection xcb:Expose (raytrace-draw l window gc))
      (xcb:+event raytrace-connection xcb:KeyPress #'raytrace-close))
    (xcb:flush raytrace-connection)))

(defun raytrace-disconnect-all ()
  (when raytrace-connection
    (xcb:disconnect raytrace-connection))
  (setq raytrace-connection nil))

(defun raytrace-sphere (x y z r)
  (list (mapcar #'float (list x y z)) (float r)))

(raytrace-tracer "test"
		 (list (raytrace-sphere 12 10 12 10)
		       (raytrace-sphere 13 38 24 15)))

(raytrace-disconnect-all)


;;; raytrace.el ends here
