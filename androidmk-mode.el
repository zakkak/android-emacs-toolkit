;;; androidmk-mode.el --- android.mk mode 

;; Copyright (C) 2011 zxy
;; Author: zxy <gcoordinate@gmail.com>
;; Maintainer: zxy <gcoordinate@gmail.com>
;; Created: May 2011
;; Version: 1.2

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

(message (concat "Loading " load-file-name))

;; public var
(defvar ndk-script-path
  (concat(file-name-directory (or load-file-name buffer-file-name))"ndkscript")
  "*Script for android ndk")
;;(message ndk-script-path)

(defvar ndk-template-directory
  (concat(file-name-directory (or load-file-name buffer-file-name))"jni_template")
  "*Project template for android ndk")

;; ==================================================================
;; androidmk-mode utility and feature defuns
;; ==================================================================

(defun android-temp-buffer ()
  (if (not (eq nil (get-buffer "*android-temp*")))
      (kill-buffer "*android-temp*"))
  (shell "*android-temp*"))

(defun android-build-buffer ()
  (if (not (eq nil (get-buffer "*android-build*")))
      (kill-buffer "*android-build*"))
  (shell "*android-build*"))

(defun android-string-android ()
  (if (eq system-type 'windows-nt)
      (concat "'"sdk-root-path"/tools/android.bat'")
    (concat "'"sdk-root-path"/tools/android'")))

(defun android-string-emulator ()
  (if (eq system-type 'windows-nt)
      (concat "'"sdk-root-path"/tools/emulator.exe'")
    (concat "'"sdk-root-path"/tools/emulator'")))

(defun android-list-target ()
  (let* ((command (concat (android-string-android)" list targets"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "id: \\(.*\\)" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "no Android Virtual Devices found"))))

(defun android-list-avd ()
  (let* ((command (concat (android-string-android)" list avds"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "Name: \\(.*\\)" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "no Android Virtual Devices found"))))

(defun android-string-avd ()
  (setq result (android-list-target))
  (setq tmpnum 0)
  (setq tmpreadstring "[android] Virtual Device( ")
  (while (< tmpnum (length result))
    (setq tmpreadstring
          (concat tmpreadstring(nth tmpnum result)"; "))
    (setq tmpnum (+ tmpnum 1)))
  (concat tmpreadstring"): "))

;; ==================================================================

(defun lin-init-command ()
  "init shell command, you have to run once"
  (interactive)
  (android-temp-buffer)
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-root-path"/ndk-build\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/adr.sh\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/api.sh\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/apu.sh\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/init.sh\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/lgs.sh\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/mhp.sh\n"))
  (comint-send-string (current-buffer) (concat "sudo chmod +x "ndk-script-path"/ndbr.sh\n"))
  (delete-window))

(defun win-init-command ()
  "init shell command, you have to run once"
  (interactive)
  (android-temp-buffer)
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/_adbt.sh "ndk-script-path"/_adbt.sh \n"))
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/adr.sh "ndk-script-path"/adr.sh \n"))
  (comint-send-string 
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/api.sh "ndk-script-path"/api.sh \n"))
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/apu.sh "ndk-script-path"/apu.sh \n"))
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/init.sh "ndk-script-path"/init.sh \n"))
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/lgs.sh "ndk-script-path"/lgs.sh\n"))
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/mhp.sh "ndk-script-path"/mhp.sh \n"))
  (comint-send-string
   (current-buffer)
   (concat "dos2unix "ndk-script-path"/ndbr.sh "ndk-script-path"/ndbr.sh \n"))
  (delete-window))

(if (eq system-type 'windows-nt)
    (win-init-command)
  (lin-init-command))

;; ==================================================================

(defun android-list-targets ()
  "list all targets on android"
  (interactive)
  (android-temp-buffer)
  (comint-send-string (current-buffer) (concat "echo '[android] Listing targets...' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" list targets \n"))
  (end-of-buffer)
  (other-window -1))

(defun android-list-avds ()
  "list all avds on android"
  (interactive)
  (android-temp-buffer)
  (comint-send-string (current-buffer) (concat "echo '[android] Listing avd...' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" list avd \n"))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun android-launch-avd ()
  "launch avd with name"
  (interactive)
  ;; start avd
  (let ((avdName (completing-read "[android] Virtual Device Name: " (android-list-avd))))
    (shell (concat "*android-avd-"avdName"*"))
    (comint-send-string (current-buffer) (concat (android-string-emulator)" -avd "avdName"\n"))
    (end-of-buffer)
    (other-window -1)))

(defun android-create-avd ()
  "Create avd with name"
  (interactive)
  ;; create avd
  (let ((avdName (read-string "[android] Name (default AVD): " nil nil "AVD" nil))
        (avdTarget (read-string (android-string-avd) nil nil "1" nil)))
    (android-temp-buffer)
    (comint-send-string (current-buffer)
                        (format "%s create avd -n %s -t %s \n"
                                (android-string-android)
                                avdName
                                avdTarget))
    (end-of-buffer)))

;; ==================================================================

(defun win-android-start-log ()
  "android-start-log for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "lgs.sh \n")))

(defun lin-android-start-log ()
  "android-start-log for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/lgs.sh \n")))

(defun android-start-log () 
  "Open shell to show android log"
  (interactive)
  (if (not (eq nil (get-buffer "*android-log*")))
      (kill-buffer "*android-log*"))
  (shell "*android-log*")
  (if (eq system-type 'windows-nt)
      (win-android-start-log)
    (lin-android-start-log))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun* android-new-project ()
  "Create new project"
  (interactive)
  (if (equal nil buffer-file-name)
      (setq tmpprojectpath "~/")
    (setq tmpprojectpath (file-name-directory buffer-file-name)))
  (setq result (android-list-target))
  (setq tmpnum 0)
  (setq tmpreadstring "[android] Virtual Device( ")
  (while (< tmpnum (length result))
    (setq tmpreadstring
          (concat tmpreadstring(nth tmpnum result)"; "))
    (setq tmpnum (+ tmpnum 1)))
  (setq tmpreadstring (concat tmpreadstring"): "))
  ;; read information from mini buffer
  (let ((projectpath (read-string "[android] Project path: "
                                  tmpprojectpath nil tmpprojectpath nil))
        (projectname (read-string "[android] Project name (default androidproject): "
                                  nil nil "androidproject" nil))
        (targetid (read-string tmpreadstring nil nil "1" nil))
        (package (read-string "[android] Package name (default org.example): "
                              nil nil "org.example" nil))
        (activity (read-string "[android] Activity name (default TestActivity): "
                              nil nil "TestActivity" nil)))
    (setq projectpath (concat projectpath"/"projectname))
    (message "[android] Building android project at %s." projectpath)
    (if (and (file-exists-p projectpath) (file-exists-p (concat projectpath"/build.xml")))
        (error "Project exist!"))
    (when (or (not (file-exists-p projectpath))
            (and (file-exists-p projectpath)
                 (y-or-n-p (format "[android] Folder %s exist! Overwrit?" projectpath))))
      (when (y-or-n-p "[android] New ndk folder?")
        (unless (file-exists-p projectpath)
          (dired-create-directory projectpath))
        (copy-directory ndk-template-directory (concat projectpath"/jni"))
        (find-file (concat projectpath"/jni/android.mk"))
        (beginning-of-buffer))
      (android-temp-buffer)
      (comint-send-string (current-buffer)
                          (format "%s create project -n %s -t %s -p %s -k %s -a %s \n"
                                  (android-string-android)
                                  projectname
                                  targetid
                                  projectpath
                                  package
                                  activity))
      (end-of-buffer)
      (other-window -1)
      )))

;; ==================================================================

(defun win-androidndk-build ()
  "androidndk-build for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh \n"))
  (comint-send-string (current-buffer) (concat "adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun lin-androidndk-build ()
  "androidndk-build for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun androidndk-build (proName)
  "Build and run project with android ndk"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidndk-build)
    (lin-androidndk-build))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-androidndk-rebuild ()
  "androidndk-rebuild for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh r\n"))
  (comint-send-string (current-buffer) (concat "adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun lin-androidndk-rebuild ()
  "androidndk-rebuild for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh r\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/adr.sh '"ndk-project-name"' '"(replace-regexp-in-string "/jni/Android.mk" "" ndk-proPath)"'\n")))

(defun androidndk-rebuild (proName)
  "Rebuild and run project with android ndk"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidndk-rebuild)
    (lin-androidndk-rebuild))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-androidsdk-build ()
  "androidsdk-build for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh b '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "api.sh '"sdk-project-path"/bin'\n")))

(defun lin-androidsdk-build ()
  "androidsdk-build for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source ndbr.sh b '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/api.sh '"sdk-project-path"/bin'\n")))

(defun androidsdk-build (proName)
  "Build ndk project and install apk in sdk-project-path"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidsdk-build)
    (lin-androidsdk-build))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-androidsdk-rebuild ()
  "androidsdk-rebuild for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh r '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "api.sh '"sdk-project-path"/bin'\n")))

(defun lin-androidsdk-rebuild ()
  "androidsdk-rebuild for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh r '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/apu.sh '"sdk-project-package"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/api.sh '"sdk-project-path"/bin'\n")))

(defun androidsdk-rebuild (proName)
  "Rebuild ndk project and install apk in sdk-project-path"
  (interactive "*p")
  (setq ndk-proPath (buffer-file-name))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidsdk-rebuild)
    (lin-androidsdk-rebuild))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-android-open-dir ()
  "android-open-dir for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "mhp.sh \n")))

(defun lin-android-open-dir ()
  "android-open-dir for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"ndk-root-path"' '"sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/mhp.sh \n")))

(defun android-open-dir (name dir)
  "Open shell to prepare for run shell script"
  (interactive "sProject name: \nDDirectory: ")
  (let ((default-directory dir))(shell name))
  (if (eq system-type 'windows-nt)
      (win-android-open-dir)
    (lin-android-open-dir)))

;; ==================================================================
;; end of android-mode utility and feature defuns
;; ==================================================================

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

(add-to-list 'auto-mode-alist '("\\.mk\\'" . androidmk-mode))

(provide 'androidmk-mode)

;;; androidmk-mode.el ends here
