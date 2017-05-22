;;; memanalize.el --- 
;; 
;; Filename: memanalize.el
;; Description: plot graph for emacs memory usage
;; Author: Matthias
;; 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 17.05.2017 initial commit 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defsubst get-mem-conses (mi)
  (let ((data (nth 0 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-symbols (mi)
  (let ((data (nth 1 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-misc (mi)
  (let ((data (nth 2 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-string-header (mi)
  (let ((data (nth 3 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-string-bytes (mi)
  (let ((data (nth 4 mi)))
    (/ (* (nth 1 data) (nth 2 data)) (* 1024 1024.0))))

(defsubst get-mem-vector-header (mi)
  (let ((data (nth 5 mi)))
    (/ (* (nth 1 data) (nth 2 data)) (* 1024 1024.0))))

(defsubst get-mem-vector-slots (mi)
  (let ((data (nth 6 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-floats (mi)
  (let ((data (nth 7 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-intervals (mi)
  (let ((data (nth 8 mi)))
    (/ (* (nth 1 data) (+ (nth 2 data) (nth 3 data))) (* 1024 1024.0))))

(defsubst get-mem-buffers (mi)
  (let ((data (nth 9 mi)))
    (/ (* (nth 1 data) (nth 2 data)) (* 1024 1024.0))))

(defun collector (filename)
  "Write memory data into file with FILENAME."
  (let ((mi (garbage-collect)))
    (with-temp-buffer
      (insert 
       (format "%f %f %f %f %f %f %f %f %f %f %f\r\n"
               (float-time)
               (get-mem-conses mi)
               (get-mem-symbols mi)
               (get-mem-misc mi)
               (get-mem-string-header mi)
               (get-mem-string-bytes mi)
               (get-mem-vector-header mi)
               (get-mem-vector-slots mi)
               (get-mem-floats mi)
               (get-mem-intervals mi)
               (get-mem-buffers mi)))
      (let ((message-log-max nil))
        (append-to-file (point-min) (point-max) filename)))))

(defvar collector-timer nil)

(defun start-collection (filename interval)
  (interactive "FEnter filename:\nMEnter interval: ")
  (setq collector-filename filename
        collector-timer (run-at-time
                         2
                         (string-to-number interval)
                         'collector filename)))

(defun stop-collection ()
  (interactive)
  (when (timerp collector-timer)
    (cancel-timer collector-timer)))

(defun plot-image-from-data (datafile imagefile)
  (interactive "FEnter data-filename: \nFEnter image-filename:")
  (let ((gnuplot (start-process "gnuplot" "*gnuplot*" "gnuplot")))
    (process-send-string gnuplot "set term png\n")
    (process-send-string gnuplot (format "set output \"%s\"\n" imagefile))
    (process-send-string gnuplot "set grid\n")
    (process-send-string gnuplot "set title \"Emacs memory consumption by category\"\n")
    (process-send-string gnuplot "set xlabel \"interval\"\n")
    (process-send-string gnuplot "set autoscale\n")
    (process-send-string gnuplot "set ylabel \"2^{20} bytes\"\n")
    (process-send-string gnuplot (format "plot \"%s\" using 2 title \"cons cells\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 3 title \"symbols\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 4 title \"\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 5 title \"string header\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 6 title \"string bytes\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 7 title \"vector header\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 8 title \"vector slots\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 9 title \"floats\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 10 title \"intervals\" with lines" datafile))
    (process-send-string gnuplot (format ", \"%s\" using 11 title \"buffers\" with lines\n" datafile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; memanalize.el ends here
