(defun frames ()
  "Visual representation of frames similar to Vim's :tabs command."
  (interactive)
  (cl-labels
      ((frame-face (frame text)
                   (let* ((selected "orange red")
                          (other "dodger blue")
                          (color (if (eq frame (selected-frame))
                                     selected other)))
                     (propertize text 'face `(:foreground ,color))))
       (show-window (window)
                    (format "\t%s"
                            (buffer-name (window-buffer window))))
       (show-frame (count frame)
                   (frame-face frame
                               (format "%d %s"
                                       count
                                       (frame-parameter frame 'name))))
       (represent-windows
        (representation windows)
        (if (not (eq windows nil))
            (let* ((window (car windows))
                   (shown-window (show-window window)))
              (represent-windows
               (if (string= "" representation)
                   shown-window
                 (concat representation "\n" shown-window))
               (cdr windows)))
          representation))
       (represent-frames
        (count representation frames)
        (if (not (eq frames nil))
            (let* ((frame (car frames))
                   (represented-windows (represent-windows
                                         "" (window-list frame)))
                   (represented-frame (concat
                                       (show-frame count frame)
                                       "\n" represented-windows)))
              (represent-frames
               (1+ count)
               (if (string= "" representation)
                   represented-frame
                 (concat representation "\n" represented-frame))
               (cdr frames)))
          representation))
       (frames-buffer ()
                      (let ((buffer (get-buffer-create "*Frames*"))
                            (represented-frames (represent-frames 1 "" (frame-list))))
                        (display-buffer buffer)
                        (with-current-buffer buffer
                          (save-selected-window
                            (select-window (get-buffer-window buffer 0))
                            (setq buffer-read-only nil)
                            (erase-buffer)
                            (insert represented-frames)
                            (setq buffer-read-only t))))))
    (frames-buffer)))

(provide 'frames)
