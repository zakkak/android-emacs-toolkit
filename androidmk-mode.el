;;; androidmk-mode.el --- android.mk mode 

;; Copyright (C) 2011 zxy
;; Author: zxy <gcoordinate@gmail.com>
;; Maintainer: zxy <gcoordinate@gmail.com>
;; Created: May 2011
;; Version: 0.1.1 

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Installation instructions
;; --------------------------------
;; window->preference->general->workspace check Refresh automatically
;; you have to defined ndk-script-path
;;

(defun win-android-start-log ()
  "android-start-log for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "lgs.sh \n")))
(defun lin-android-start-log ()
  "android-start-log for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/lgs.sh \n")))
(defun android-start-log () 
  "Open shell to show android log"
  (interactive)
  (shell "*android-log*")
  (if (eq system-type 'windows-nt)
      (win-android-start-log)
    (lin-android-start-log))
  (end-of-buffer)
  (other-window -1))

(defun win-android-open-dir ()
  "android-open-dir for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "mhp.sh \n")))
(defun lin-android-open-dir ()
  "android-open-dir for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/mhp.sh \n")))
(defun android-open-dir (name dir)
  "Open shell to prepare for run shell script"
  (interactive "sProject name: \nDDirectory: ")
  (let ((default-directory dir))(shell name))
  (if (eq system-type 'windows-nt)
      (win-android-open-dir)
    (lin-android-open-dir)))

(defun win-androidndk-build ()
  "androidndk-build for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh \n"))
  (comint-send-string (current-buffer) (concat "adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun lin-androidndk-build ()
  "androidndk-build for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun androidndk-build (proName)
  "Build and run project with android ndk"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (shell "*android-build*")
  (if (eq system-type 'windows-nt)
      (win-androidndk-build)
    (lin-androidndk-build))
  (end-of-buffer)
  (other-window -1))

(defun win-androidndk-rebuild ()
  "androidndk-rebuild for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh r\n"))
  (comint-send-string (current-buffer) (concat "adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun lin-androidndk-rebuild ()
  "androidndk-rebuild for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh r\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun androidndk-rebuild (proName)
  "Rebuild and run project with android ndk"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (shell "*android-build*")
  (if (eq system-type 'windows-nt)
      (win-androidndk-rebuild)
    (lin-androidndk-rebuild))
  (end-of-buffer)
  (other-window -1))

(defun win-androidsdk-build ()
  "androidsdk-build for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh b '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "api.sh '"sdk-project-path"/bin'\n")))
(defun lin-androidsdk-build ()
  "androidsdk-build for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "source ndbr.sh b '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/api.sh '"sdk-project-path"/bin'\n")))
(defun androidsdk-build (proName)
  "Build ndk project and install apk in sdk-project-path"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (shell "*android-build*")
  (if (eq system-type 'windows-nt)
      (win-androidsdk-build)
    (lin-androidsdk-build))
  (end-of-buffer)
  (other-window -1))

(defun win-androidsdk-rebuild ()
  "androidsdk-rebuild for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh r '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "api.sh '"sdk-project-path"/bin'\n")))
(defun lin-androidsdk-rebuild ()
  "androidsdk-rebuild for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh r '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/api.sh '"sdk-project-path"/bin'\n")))
(defun androidsdk-rebuild (proName)
  "Rebuild ndk project and install apk in sdk-project-path"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (shell "*android-build*")
  (if (eq system-type 'windows-nt)
      (win-androidsdk-rebuild)
    (lin-androidsdk-rebuild))
  (end-of-buffer)
  (other-window -1))

(defun android-list-targets ()
  "list all targets on android"
  (interactive)
  (shell "*android-info*")
  (if (eq system-type 'windows-nt)
      (comint-send-string (current-buffer) (concat "'"sdk-root-path"/tools/android.bat' list targets \n"))
    (comint-send-string (current-buffer) (concat "'"sdk-root-path"/tools/android.bat' list targets \n")))
  (end-of-buffer)
  (other-window -1))

(defun android-list-avds ()
  "list all avds on android"
  (interactive)
  (shell "*android-info*")
  (if (eq system-type 'windows-nt)
      (comint-send-string (current-buffer) (concat "'"sdk-root-path"/tools/android.bat' list avd \n"))
    (comint-send-string (current-buffer) (concat "'"sdk-root-path"/tools/android.bat' list avd \n")))
  (end-of-buffer)
  (other-window -1))

(defun android-launch-avd (avdName)
  "launch avd with name"
  (interactive "savd name:")
  (shell avdName)
  (if (eq system-type 'windows-nt)
      (comint-send-string (current-buffer) (concat "'"sdk-root-path"/tools/emulator.exe' -avd "avdName"\n"))
    (comint-send-string (current-buffer) (concat "'"sdk-root-path"/tools/emulator.exe' -avd "avdName"\n")))
  (end-of-buffer)
  (other-window -1))


;; the command to comment/uncomment text
(defun androidmk-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
     (comment-dwim arg)))

;; keywords for syntax coloring
(setq myKeywords
 `(
   ( ,(regexp-opt '("Sin" "Cos" "Sum") 'word) . font-lock-function-name-face)
   ( ,(regexp-opt '("Pi" "Infinity") 'word) . font-lock-constant-face)
  )
)

;; define the major mode.
(define-derived-mode androidmk-mode makefile-mode "androidmk"
"androidmk-mode is a major mode for editing language androidmk."
  ;;(setq font-lock-defaults '(myKeywords))

  ;; modify the keymap
  ;;(define-key androidmk-mode-map [remap comment-dwim] 'androidmk-comment-dwim)

  ;; perl style comment: 
  ;;(modify-syntax-entry ?# "< b" androidmk-mode-syntax-table)
  ;;(modify-syntax-entry ?\n "> b" androidmk-mode-syntax-table)
)

(defun init-shell-command ()
  "init shell command, you have to run once"
  (interactive)
  ;;(let (shell password))
  (if (eq system-type 'windows-nt)
      (setq my-system-type "windows")
    (shell "*init-shell-command*")
    (comint-send-string (current-buffer) (concat "sudo \n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-root-path"/ndk-build.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/adr.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/api.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/apu.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/init.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/lgs.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/mhp.sh\n"))
    (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/ndbr.sh\n"))))

(defun cygwin-settings()
  (setenv "PATH" (concat "'"cygwin-root-path"/bin;'" (getenv "PATH")))
  ;;(define c (string-append cygwin-root-path "/bin/bash.exe"))
  ;;(setq shell-file-name "C:/cygwin/bin/bash.exe")
  (setq shell-file-name  cygwin-root-path"/bin/bash.exe")
  (setq explicit-shell-file-name shell-file-name)
  ;;(setq explicit-shell-file-name "c:/cygwin/bin/bash.exe")
  (setq shell-file-name "'"cygwin-root-path"/bin/bash.exe'"))
  ;;(setq shell-file-name "c:/cygwin/bin/bash.exe"))
;;(if (eq system-type 'windows-nt) 
;;    (cygwin-settings))

(add-to-list 'auto-mode-alist '("\\.mk\\'" . androidmk-mode))
(provide 'androidmk-mode)
