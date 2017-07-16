;;; package --- Summary
;;; Commentary:
;;; Code:

(defun serika-f/shn/split (file-1 file-2 &optional output-format)
  "Split MUSIC-FILE to files with PLAYLIST-FILE."
  (when serika-shn--executable
    (let* ((music-file (if (string-match ".cue$" file-1) file-2 file-1))
           (playlist-file (if (string-match ".cue$" file-1) file-1 file-2))
           (cmd))
      (setq cmd (format (concat serika-shn--executable
                                " split -o %s -t %s -f \"%s\" \"%s\"")
                        (or output-format "flac")
                        serika-shn--file-format
                        playlist-file
                        music-file))
      (serika-f/eshell/execute cmd t))))
