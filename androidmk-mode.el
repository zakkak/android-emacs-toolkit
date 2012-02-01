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
(defvar android-default-package "com.package"
  "*Android new project default package")

(defvar android-ndk-root-path ""
  "*Android ndk root path")
(message "[android] %s"android-ndk-root-path)

(defvar android-sdk-root-path ""
  "*Android sdk root path")
(message "[android] %s"android-sdk-root-path)

;; private var
(defvar toolkit-path
  (file-name-directory (or load-file-name buffer-file-name))
  "*Android emacs toolkit path")

(defvar ndk-script-path
  (concat toolkit-path"ndkscript")
  ;;   (if (eq system-type 'windows-nt)
  ;;       (concat "/cygdrive/"(replace-regexp-in-string ":" "" toolkit-path)"ndkscript")
  ;;     (concat toolkit-path"ndkscript"))
  "*Script for android ndk")

(defvar ndk-template-directory
  (concat toolkit-path"jni_template")
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
      (concat "'"android-sdk-root-path"/tools/android.bat'")
    (concat "'"android-sdk-root-path"/tools/android'")))

(defun android-string-emulator ()
  (if (eq system-type 'windows-nt)
      (concat "'"android-sdk-root-path"/tools/emulator.exe'")
    (concat "'"android-sdk-root-path"/tools/emulator'")))

(defun android-list-target ()
  (message "[android] List android target...")
  (let* ((command (concat (android-string-android)" list targets"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "id: \\(.*\\)" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "[android] no Android Virtual Devices found"))))

(defun android-list-avd ()
  (message "[android] List android avd...")
  (let* ((command (concat (android-string-android)" list avds"))
         (output (shell-command-to-string command))
         (result nil)
         (offset 0))
    (while (string-match "Name: \\(.*\\)" output offset)
      (setq result (cons (match-string 1 output) result))
      (setq offset (match-end 0)))
    (if result
        (reverse result)
      (error "[android] no Android Virtual Devices found"))))

(defun android-string-avd ()
  (setq result (android-list-target))
  (setq tmpnum 0)
  (setq tmpreadstring "[android] Virtual Device( ")
  (while (< tmpnum (length result))
    (setq tmpreadstring
          (concat tmpreadstring(nth tmpnum result)"; "))
    (setq tmpnum (+ tmpnum 1)))
  (concat tmpreadstring"): "))

(defun android-project-path ()
  "get sdk project path"
  (if (equal nil buffer-file-name)
      (error "[android] buffer-file-name is nil!"))
  (setq tmpprojectpath (file-name-directory (or load-file-name buffer-file-name)))
  (if (file-exists-p (concat tmpprojectpath "build.xml"))
      (concat tmpprojectpath)
    (progn
      (setq tmpprojectpath (replace-regexp-in-string "/jni.*" "" tmpprojectpath))
      (if (file-exists-p (concat tmpprojectpath "/build.xml"))
          (concat tmpprojectpath)
        (progn
          (setq tmpprojectpath (replace-regexp-in-string "/src.*" "" tmpprojectpath))
          (if (file-exists-p (concat tmpprojectpath "/build.xml"))
              (concat tmpprojectpath)
            (error "[android] %s isn't in android project!" buffer-file-name)
            ))
        ))
    ))

(defun android-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun android-read-lines (filePath) 
  "Return a list of lines of a file at at FPATH." 
  (with-temp-buffer 
    (insert-file-contents filePath) 
    (split-string (buffer-string) "\n" t)))

(defun android-parse-name ()
  "Parse project name."
  (let ((output (android-string-from-file (concat (android-project-path)"/build.xml")))
        (offset 0))
    (if (string-match "<project name=\"\\(.*\\)\" " output offset)
        (match-string 1 output)
      (error "[android] Cannot parse project name"))))

(defun android-parse-package ()
  "Parse package name." 
  (let ((output (android-string-from-file (concat (android-project-path)"/AndroidManifest.xml")))
        (offset 0))
    (if (string-match "package=\"\\(.*\\)\"" output offset)
        (match-string 1 output)
      (error "[android] Cannot parse project package"))))

(defun android-parse-activity ()
  "Parse activity name." 
  (let ((output (android-string-from-file (concat (android-project-path)"/AndroidManifest.xml")))
        (offset 0))
    (if (string-match "<activity android:name=\"\\(.*\\)\"" output offset)
        (match-string 1 output)
      (error "[android] Cannot parse project activity"))))

;; ==================================================================

(defun lin-init-command ()
  "init shell command, you have to run once"
  (interactive)
  (android-temp-buffer)
  (comint-send-string (current-buffer) (concat "sudo chmod +x "android-ndk-root-path"/ndk-build\n"))
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
  (comint-send-string (current-buffer) (concat "echo '[android] List targets...' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" list targets \n"))
  (end-of-buffer)
  (other-window -1))

(defun android-list-avds ()
  "list all avds on android"
  (interactive)
  (android-temp-buffer)
  (comint-send-string (current-buffer) (concat "echo '[android] List avd...' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" list avd \n"))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun android-launch-avd ()
  "launch avd with name"
  (interactive)
  (let ((avdName (completing-read "[android] Virtual Device Name(Key 'up' to chose): " (android-list-avd))))
    ;; (call-process (android-string-emulator) nil t nil (concat " -avd "avdName"\n"))
    ;; (shell-command (concat (android-string-emulator)" -avd "avdName"\n"))
    ;; (message (shell-command-to-string (concat (android-string-emulator)" -avd "avdName"\n")))
    (setq bufname (concat "*android-temp-"avdName"*"))
    (if (not (eq nil (get-buffer bufname)))
        (error "[android] %s has been launched." avdName))
    (shell bufname)
    (comint-send-string (current-buffer) (concat (android-string-emulator)" -avd "avdName"\n"))
    (end-of-buffer)
    (delete-window)))

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
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "lgs.sh \n")))

(defun lin-android-start-log ()
  "android-start-log for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
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

(defun android-new-project ()
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
                                  nil nil "androidproject" nil)))
    (setq projectpath (concat projectpath"/"projectname))
    (if (and (file-exists-p projectpath) (file-exists-p (concat projectpath"/build.xml")))
        (error "[android] Project exist!"))
    (let ((targetid (read-string tmpreadstring nil nil "1" nil))
          (package (read-string (concat "[android] Package name: ")
                                (concat android-default-package".") nil (concat android-default-package".test") nil))
          (activity (read-string "[android] Activity name (default TestActivity): "
                                 nil nil "TestActivity" nil)))
      (message "[android] Building android project at %s." projectpath)
      ;; Build sdk
      (let ((output (shell-command-to-string (format "%s create project -n %s -t %s -p %s -k %s -a %s \n"
                                                     (android-string-android)
                                                     projectname
                                                     targetid
                                                     projectpath
                                                     package
                                                     activity)))
            (offset 0))
        (when (string-match "Error:\\(.*\\)" output offset)
          (error "[android] %s"(match-string 1 output)))
        (find-file (format "%s/src/%s/%s.java"
                           projectpath
                           (replace-regexp-in-string "\\." "/" package)
                           activity))
        ;; Build ndk
        (when (y-or-n-p "[android] New ndk folder?")
          (unless (file-exists-p projectpath)
            (dired-create-directory projectpath))
          (copy-directory ndk-template-directory (concat projectpath"/jni"))
          ;; (find-file (concat projectpath"/jni/android.mk"))
          (beginning-of-buffer))
        ))))

;; ==================================================================

(defun win-androidndk-build ()
  "androidndk-build for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh \n"))
  (comint-send-string (current-buffer) (concat "adr.sh '"sdk-project-name"' '"sdk-project-path"'\n")))

(defun lin-androidndk-build ()
  "androidndk-build for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/adr.sh '"sdk-project-name"' '"sdk-project-path"'\n")))

(defun androidndk-build ()
  "Build and run project with android ndk"
  (interactive)
  (setq sdk-project-path (android-project-path))
  (setq sdk-project-name (android-parse-name))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidndk-build)
    (lin-androidndk-build))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-androidndk-rebuild ()
  "androidndk-rebuild for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh r\n"))
  (comint-send-string (current-buffer) (concat "adr.sh '"sdk-project-name"' '"sdk-project-path"'\n")))

(defun lin-androidndk-rebuild ()
  "androidndk-rebuild for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh r\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/adr.sh '"sdk-project-name"' '"sdk-project-path"'\n")))

(defun androidndk-rebuild ()
  "Rebuild and run project with android ndk"
  (interactive)
  (setq sdk-project-path (android-project-path))
  (setq sdk-project-name (android-parse-name))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidndk-rebuild)
    (lin-androidndk-rebuild))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-androidsdk-build ()
  "androidsdk-build for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" update project --target 1 --path '"sdk-project-path"' \n"))
  (comint-send-string (current-buffer) (concat "ant -buildfile '"sdk-project-path"/build.xml' ""debug"" \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh b '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "apu.sh '"sdk-package-name"'\n"))
  (comint-send-string (current-buffer) (concat "api.sh '"sdk-project-path"/bin' '"sdk-package-name"' '"sdk-activity-name"'\n")))

(defun lin-androidsdk-build ()
  "androidsdk-build for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" update project --target 1 --path '"sdk-project-path"' \n"))
  (comint-send-string (current-buffer) (concat "ant -buildfile '"sdk-project-path"/build.xml' ""debug"" \n"))
  (comint-send-string (current-buffer) (concat "source ndbr.sh b '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/apu.sh '"sdk-package-name"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/api.sh '"sdk-project-path"/bin' '"sdk-package-name"' '"sdk-activity-name"'\n")))

(defun androidsdk-build ()
  "Build ndk project and install apk in sdk-project-path"
  (interactive)
  (setq sdk-project-path (android-project-path))
  (setq sdk-package-name (android-parse-package))
  (setq sdk-activity-name (android-parse-activity))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidsdk-build)
    (lin-androidsdk-build))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-androidsdk-rebuild ()
  "androidsdk-rebuild for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" update project --target 1 --path '"sdk-project-path"' \n"))
  (comint-send-string (current-buffer) (concat "ant -buildfile '"sdk-project-path"/build.xml' ""clean"" \n"))
  (comint-send-string (current-buffer) (concat "ant -buildfile '"sdk-project-path"/build.xml' ""debug"" \n"))
  (comint-send-string (current-buffer) (concat "ndbr.sh r '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "apu.sh '"sdk-package-name"'\n"))
  (comint-send-string (current-buffer) (concat "api.sh '"sdk-project-path"/bin' '"sdk-package-name"' '"sdk-activity-name"'\n")))

(defun lin-androidsdk-rebuild ()
  "androidsdk-rebuild for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat (android-string-android)" update project --target 1 --path '"sdk-project-path"' \n"))
  (comint-send-string (current-buffer) (concat "ant -buildfile '"sdk-project-path"/build.xml' ""clean"" \n"))
  (comint-send-string (current-buffer) (concat "ant -buildfile '"sdk-project-path"/build.xml' ""debug"" \n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/ndbr.sh r '"sdk-project-path"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/apu.sh '"sdk-package-name"'\n"))
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/api.sh '"sdk-project-path"/bin' '"sdk-package-name"' '"sdk-activity-name"'\n")))

(defun androidsdk-rebuild ()
  "Rebuild ndk project and install apk in sdk-project-path"
  (interactive)
  (setq sdk-project-path (android-project-path))
  (setq sdk-package-name (android-parse-package))
  (setq sdk-activity-name (android-parse-activity))
  (android-build-buffer)
  (if (eq system-type 'windows-nt)
      (win-androidsdk-rebuild)
    (lin-androidsdk-rebuild))
  (end-of-buffer)
  (other-window -1))

;; ==================================================================

(defun win-android-open-dir ()
  "android-open-dir for windows"
  (comint-send-string (current-buffer) (concat ". "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
  (comint-send-string (current-buffer) (concat "mhp.sh \n")))

(defun lin-android-open-dir ()
  "android-open-dir for linux"
  (comint-send-string (current-buffer) (concat "source "ndk-script-path"/init.sh '"ndk-script-path"' '"android-ndk-root-path"' '"android-sdk-root-path"' \n"))
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
