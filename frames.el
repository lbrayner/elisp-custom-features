(define-derived-mode frames-mode special-mode "Frames")

(defun frames ()
  "Visual representation of frames similar to Vim's :tabs command."
  (interactive)
  (cl-labels
      ((get-previous-buffer (buffer)
                            (with-current-buffer buffer
                              (save-selected-window
                                (select-window (get-buffer-window buffer 0))
                                (let ((previous (previous-buffer)))
                                  (next-buffer) previous))))
       (frame-face (selected-frame frame text)
                   (let* ((selected "orange red")
                          (other "dodger blue")
                          (color (if (eq frame selected-frame)
                                     selected other)))
                     (propertize text 'face `(:foreground ,color))))
       (window-frame (window text)
                     (let ((name (buffer-name (window-buffer window))))
                       (if (string= name "*Frames*")
                           (propertize text 'face '(:foreground "yellow"))
                         text)))
       (show-window (other-window-exists-p window)
                    (let* ((buffer (window-buffer window))
                           (name (buffer-name buffer))
                           (window-name
                            (cond ((not other-window-exists-p) name)
                                  ((not (string= name "*Frames*")) name)
                                  (t (buffer-name (get-previous-buffer buffer)))))
                           (text (format "\t%s" window-name)))
                      (window-frame window text)))
       (show-frame (count selected-frame frame)
                   (frame-face selected-frame frame
                               (format "%d %s"
                                       count
                                       (frame-parameter frame 'name))))
       (represent-windows
        (representation other-window-exists-p windows)
        (if (not (eq windows nil))
            (let* ((window (car windows))
                   (shown-window (show-window other-window-exists-p window)))
              (represent-windows
               (if (string= "" representation)
                   shown-window
                 (concat representation "\n" shown-window))
               other-window-exists-p
               (cdr windows)))
          representation))
       (represent-frames
        (count representation other-window-exists-p selected-frame frames)
        (if (not (eq frames nil))
            (let ((frame (car frames)))
              (with-selected-frame frame
                (let* ((represented-windows (represent-windows
                                             "" other-window-exists-p
                                             (window-list frame)))
                       (represented-frame (concat
                                           (show-frame count selected-frame frame)
                                           "\n" represented-windows)))
                  (represent-frames
                   (1+ count)
                   (if (string= "" representation)
                       represented-frame
                     (concat representation "\n" represented-frame))
                   other-window-exists-p
                   selected-frame
                   (cdr frames)))))
          representation))
       (frames-buffer ()
                      (let ((selected-frame (selected-frame))
                            (other-window-exists-p
                             (not (eq (selected-window) (next-window))))
                            (buffer (get-buffer-create "*Frames*")))
                        (display-buffer buffer)
                        (with-current-buffer buffer
                          (save-selected-window
                            (select-window (get-buffer-window buffer 0))
                            (or (derived-mode-p 'frames-mode)
                                (frames-mode))
                            (setq buffer-read-only nil)
                            (erase-buffer)
                            (insert (represent-frames
                                     1 "" other-window-exists-p
                                     selected-frame (frame-list)))
                            (setq buffer-read-only t))))))
    (frames-buffer)))

(provide 'frames)
