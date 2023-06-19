(provide 'headphone)

(defconst headphone-bluetooth-profile-a2dp "a2dp_sink")
(defconst headphone-bluetooth-profile-hfp "handsfree_head_unit")

(defun switch-headphone-current-profile ()
  "returns (set-card-profile <card_id> <profile>)"
  (split-string
   (shell-command-to-string "pacmd dump | grep bluez | grep profile")))

(defun switch-headphone-profile ()
  (interactive)
  (let* ((current (switch-headphone-current-profile))
         (card_id (cadr current))
         (profile (caddr current))
         (next_profile (if (string= profile headphone-bluetooth-profile-a2dp)
                           headphone-bluetooth-profile-hfp
                           headphone-bluetooth-profile-a2dp)))
    (shell-command (format "pacmd set-card-profile %s %s"
                           card_id
                           next_profile))
    (message "Switched to %s" next_profile)))

;;; headphone.el ends here
