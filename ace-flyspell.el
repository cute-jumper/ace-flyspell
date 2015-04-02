;;; ace-flyspell.el --- Jump to and correct spelling errors using ace-jump-mode and flyspell    -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; URL: https://github.com/cute-jumper/ace-flyspell
;; Version: 0.1
;; Package-Requires: ((ace-jump-mode "2.0"))
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'ace-jump-mode)
(require 'flyspell)

(defgroup ace-flyspell nil
  "Jump to and correct spelling errors using ace-jump-mode and flyspell"
  :group 'flyspell)

(defface ace-flyspell-background
  '((t (:box t :bold t)))
  "face for ace-flyspell")

(defvar ace-flyspell--ov (let ((ov (make-overlay 1 1 nil nil t)))
                       (overlay-put ov 'face 'ace-flyspell-background)
                       ov))

(defvar ace-flyspell--original-point 1)

(defvar ace-flyspell--original-length 0)

;; Two convinient macros from `ace-link.el', which is a pacakge written by 
;; Oleh Krehel <ohwoeowho@gmail.com>.
;; Original code URL: https://github.com/abo-abo/ace-link
;; I modified the macro name to conform the naming convention
(defmacro ace-flyspell--flet (binding &rest body)
  "Temporarily override BINDING and execute BODY."
  (declare (indent 1))
  (let* ((name (car binding))
         (old (cl-gensym (symbol-name name))))
    `(let ((,old (symbol-function ',name)))
       (unwind-protect
            (progn
              (fset ',name (lambda ,@(cdr binding)))
              ,@body)
         (fset ',name ,old)))))

(defmacro ace-flyspell--generic (candidates &rest follower)
  "Ace jump to CANDIDATES using FOLLOWER."
  (declare (indent 1))
  `(ace-flyspell--flet (ace-jump-search-candidate
              (str va-list)
              (mapcar (lambda (x)
                        (make-aj-position
                         :offset (1- x)
                         :visual-area (car va-list)))
                      ,candidates))
     (setq ace-jump-mode-end-hook
           (list (lambda ()
                   (setq ace-jump-mode-end-hook)
                   ,@follower)))
     (let ((ace-jump-mode-scope 'window))
       (ace-jump-do ""))))
;; End macros from `ace-link.el'

(defun ace-flyspell--collect-candidates ()
  (save-excursion      
    (save-restriction
      (narrow-to-region (window-start) (window-end (selected-window) t))
      (let ((pos (point-min))
            (pos-max (point-max))
            (pos-list nil)
            (word t))
        (goto-char pos)
        (while (and word (< pos pos-max))
          (setq word (flyspell-get-word t))
          (when word
            (setq pos (nth 1 word))
            (let* ((ovs (overlays-at pos))
                   (r (ace-flyspell--has-flyspell-overlay-p ovs)))              
              (when r
                (push pos pos-list)))
            (setq pos (1+ (nth 2 word)))
            (goto-char pos)))
        (nreverse pos-list)))))

(defun ace-flyspell--has-flyspell-overlay-p (ovs)
  (let ((r nil))
    (while (and (not r) (consp ovs))
      (if (flyspell-overlay-p (car ovs))
          (setq r t)
        (setq ovs (cdr ovs))))
    r))

(defun ace-flyspell--add-overlay (beg end)
  (move-overlay ace-flyspell--ov beg end (current-buffer)))

(defun ace-flyspell-help ()
  (message "Press . to do flyspell-auto-correct-word"))

(defun ace-flyspell--auto-correct-word ()
  (interactive)
  (flyspell-auto-correct-word)
  (ace-flyspell-help))

(defun ace-flyspell--reset ()
  (interactive)
  (setq overriding-local-map nil)
  (remove-hook 'mouse-leave-buffer-hook 'ace-flyspell--reset)
  (remove-hook 'kbd-macro-termination-hook 'ace-flyspell--reset)
  (remove-hook 'minibuffer-setup-hook 'ace-flyspell--reset)
  (let ((ov-start (overlay-start ace-flyspell--ov))
        (ov-end (overlay-end ace-flyspell--ov)))
    (if (and ace-flyspell--ov ov-start ov-end)
        (if (> ov-start ace-flyspell--original-point)
            (goto-char ace-flyspell--original-point)
          (goto-char (+ ace-flyspell--original-point (- (- ov-end ov-start)
                                                    ace-flyspell--original-length))))
      (goto-char ace-flyspell--original-point)))
  (delete-overlay ace-flyspell--ov))

;;;###autoload
(defun ace-flyspell-correct-word ()
  (interactive)
  (setq ace-flyspell--original-point (point))
  (ace-flyspell--generic
      (ace-flyspell--collect-candidates)
    (forward-char)
    (let* ((word-length (length (save-excursion (car (flyspell-get-word))))))
      (setq ace-flyspell--original-length word-length)
      (ace-flyspell--add-overlay (point) (+ (point) word-length)))
    (setq overriding-local-map
          (let ((map (make-sparse-keymap)))
            (define-key map (kbd ".") 'ace-flyspell--auto-correct-word)
            (define-key map [t] 'ace-flyspell--reset)
            map))
    (add-hook 'mouse-leave-buffer-hook 'ace-flyspell--reset)
    (add-hook 'kbd-macro-termination-hook 'ace-flyspell--reset)
    (add-hook 'minibuffer-setup-hook 'ace-flyspell--reset)
    (ace-flyspell-help)))

;;;###autoload
(defun ace-flyspell-jump-word ()
  (interactive)
  (ace-flyspell--generic
      (ace-flyspell--collect-candidates)
    (forward-char)))

;;;###autoload
(defun ace-flyspell-dwim ()
  (interactive)
  (if (or (and (eq flyspell-auto-correct-pos (point))
               (consp flyspell-auto-correct-region))
          (not (flyspell-word)))
      (flyspell-auto-correct-word)
    (ace-flyspell)))

(define-key flyspell-mode-map (kbd "C-.") 'ace-flyspell-dwim)

(provide 'ace-flyspell)
;;; ace-flyspell.el ends here
