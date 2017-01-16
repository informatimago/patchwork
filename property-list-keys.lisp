;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               property-type-keys.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Creates a CFBundleDocumentType dictionary.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-03-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "PATCHWORK.BUILDER")

(defparameter *core-foundation-keys*
  '((:|CFAppleHelpAnchor| string)
    (:|CFBundleAllowMixedLocalizations| boolean)
    (:|CFBundleDevelopmentRegion| string)
    (:|CFBundleDisplayName| string)
    (:|CFBundleDocumentTypes| (vector dictionary))
    (:|CFBundleExecutable| string)
    (:|CFBundleHelpBookFolder| string)
    (:|CFBundleHelpBookName| string)
    (:|CFBundleIconFile| string)
    (:|CFBundleIconFiles| (vector string))
    (:|CFBundleIcons| dictionary)
    (:|CFBundleIdentifier| string)
    (:|CFBundleInfoDictionaryVersion| string)
    (:|CFBundleLocalizations| vector)
    (:|CFBundleName| string)
    (:|CFBundlePackageType| string)
    (:|CFBundleShortVersionString| string)
    (:|CFBundleSignature| string)
    (:|CFBundleURLTypes| (vector dictionary))
    (:|CFBundleVersion| string)
    (:|CFPlugInDynamicRegistration| string)
    (:|CFPlugInDynamicRegistrationFunction| string)
    (:|CFPlugInFactories| dictionary)
    (:|CFPlugInTypes| dictionary)
    (:|CFPlugInUnloadFunction| string)))

(defparameter *launch-service-keys*
  '((:|LSApplicationCategoryType| string)
    (:|LSArchitecturePriority| (vector string))
    (:|LSBackgroundOnly| boolean)
    (:|LSEnvironment| dictionary)
    (:|LSFileQuarantineEnabled| boolean)
    (:|LSFileQuarantineExcludedPathPatterns| (vector string))
    (:|LSGetAppDiedEvents| boolean)
    (:|LSMinimumSystemVersion| string)
    (:|LSMinimumSystemVersionByArchitecture| dictionary)
    (:|LSMultipleInstancesProhibited| boolean)
    (:|LSRequiresIPhoneOS| boolean)
    (:|LSRequiresNativeExecution| boolean)
    (:|LSUIElement| string)
    (:|LSUIPresentationMode| number)
    (:|LSVisibleInClassic| boolean)
    (:|MinimumOSVersion| string)))

(defparameter *cocoa-keys*
  '((:|GKGameCenterBadgingDisabled| boolean)
    (:|GKShowChallengeBanners| boolean)
    (:|NSAppleScriptEnabled| boolean)
    (:|NSBluetoothPeripheralUsageDescription| string)
    (:|NSCalendarsUsageDescription| string)
    (:|NSCameraUsageDescription| string)
    (:|NSContactsUsageDescription| string)
    (:|NSDockTilePlugIn| string)
    (:|NSHumanReadableCopyright| string)
    (:|NSJavaNeeded| boolean)
    (:|NSJavaPath| (vector string))
    (:|NSJavaRoot| string)
    (:|NSLocationUsageDescription| string)
    (:|NSMainNibFile| string)
    (:|NSMicrophoneUsageDescription| string)
    (:|NSMotionUsageDescription| string)
    (:|NSPersistentStoreTypeKey| string)
    (:|NSPhotoLibraryUsageDescription| string)
    (:|NSPrefPaneIconFile| string)
    (:|NSPrefPaneIconLabel| string)
    (:|NSPrincipalClass| string)
    (:|NSRemindersUsageDescription| string)
    (:|NSServices| (vector dictionary))
    (:|NSSupportsAutomaticTermination| boolean)
    (:|NSSupportsSuddenTermination| boolean)
    (:|NSUbiquitousDisplaySet| string)
    (:|NSUserNotificationAlertStyle| string)
    (:|UTExportedTypeDeclarations| (vector dictionary))
    (:|UTImportedTypeDeclarations| (vector dictionary))))

(defparameter *osx-keys*
  '((:|APInstallerURL| string)
    (:|APFiles| (vector dictionary))
    (:|ATSApplicationFontsPath| string)
    (:|CSResourcesFileMapped| boolean)
    (:|QLSandboxUnsupported| boolean)
    (:|QuartzGLEnable| boolean)))

(defparameter *cf-bundle-document-types*
  '((:|CFBundleTypeExtensions| (vector string))
    (:|CFBundleTypeIconFile| string)
    (:|CFBundleTypeIconFiles| (vector string))
    (:|CFBundleTypeMIMETypes| (vector string))
    (:|CFBundleTypeName| string)
    (:|CFBundleTypeOSTypes| (vector string))
    (:|CFBundleTypeRole| string)
    (:|LSIsAppleDefaultForType| boolean)
    (:|LSItemContentTypes| (vector string))
    (:|LSHandlerRank| string)
    (:|LSTypeIsPackage| boolean)
    (:|NSDocumentClass| string)
    (:|NSExportableAs| (vector string))
    (:|NSExportableTypes| (vector string))))

(defparameter *uti-keys*
  '((:|UTExportedTypeDeclarations| (vector string))
    (:|UTImportedTypeDeclarations| (vector string))
    (:|UTTypeIdentifier| string)
    (:|UTTypeTagSpecification| dictionary)
    (:|UTTypeConformsTo| (vector string))
    (:|UTTypeDescription| string)
    (:|UTTypeIconFile| string)
    (:|UTTypeReferenceURL| string)
    (:|UTTypeVersion| string)

    (:|com.apple.ostype| string)
    (:|public.filename-extension| (vector string))))

(defparameter *all-keys*
  (append *core-foundation-keys*
          *launch-service-keys*
          *cocoa-keys*
          *osx-keys*
          *cf-bundle-document-types*
          *uti-keys*))


(defun array (elements)
  (if (listp elements)
      (make-array (length elements) :initial-contents elements)
      elements))

(defun boolstring (boolean)
  (if boolean "true" "false"))

(defun dictionary (&rest keys-and-values)
  (assert (evenp (length keys-and-values)))
  (let ((table (make-hash-table :test (function equal))))
    (loop
      :for (key value) :on keys-and-values :by (function cddr)
      :do (let ((type (second (assoc key *all-keys* :test (function string-equal)))))
            (if type
                (setf value (validate-type type value))
                (warn "Unknown dictionary key ~S" key))
            (setf (gethash (if (keywordp key)
                               key
                               (intern (string key) "KEYWORD"))
                           table) value)))
    table))

(defun validate-type (type value)
  (if (atom type)
      (case type
        ((string)     (check-type value string) value)
        ((number)     (check-type value number) value)
        ((boolean)    (if value "YES" "NO"))
        ((vector)     (etypecase value
                        (string (error "Expected a vector, got a string ~S" value))
                        (vector value)
                        (list   (coerce value 'vector))))
        ((dictionary) (check-type value hash-table) value)
        (otherwise    (error "Unknown key type ~S" type)))
      (case (first type)
        ((vector)     (let ((value (etypecase value
                                     (vector value)
                                     (list   (coerce value 'vector)))))
                        (map-into value (lambda (element)
                                          (validate-type (second type) element)) value)))
        (otherwise (error "Unknown key type ~S" type)))))

(defun test/validate-type ()
  (assert (equal (ignore-errors (validate-type 'string "Hello")) "Hello"))
  (assert (equal (ignore-errors (validate-type 'string 42)) nil))
  (assert (equal (ignore-errors (validate-type 'number 42)) 42))
  (assert (equal (ignore-errors (validate-type 'number "42")) nil))
  (assert (equalp (ignore-errors (validate-type 'vector #(1 2 3))) #(1 2 3)))
  (assert (equalp (ignore-errors (validate-type 'vector '(1 2 3))) #(1 2 3)))
  (assert (equalp (ignore-errors (validate-type 'vector '())) #()))
  (assert (equalp (ignore-errors (validate-type 'vector "42")) nil))
  (let ((h (make-hash-table)))
    (assert (equalp (ignore-errors (validate-type 'dictionary h)) h)))
  (assert (equalp (ignore-errors (validate-type 'dictionary "42")) nil))
  (assert (equalp (ignore-errors (validate-type 'smurf "42")) nil))
  (assert (equalp (ignore-errors (validate-type '(vector number) #(1 2 3))) #(1 2 3)))
  (assert (equalp (ignore-errors (validate-type '(vector string) #("1" "2" "3"))) #("1" "2" "3")))
  (let ((vh (vector (make-hash-table) (make-hash-table) (make-hash-table))))
    (assert (equalp (ignore-errors (validate-type '(vector dictionary) vh)) vh)))
  (assert (equalp (ignore-errors (validate-type '(vector smurf) #("1" "2" "3"))) nil))
  :success)

(test/validate-type)

;;;; THE END ;;;
