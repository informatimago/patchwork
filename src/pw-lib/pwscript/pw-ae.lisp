;;;; -*- mode:lisp; coding:utf-8 -*-
;;*********************************************
;;*                                           *
;;* AppleEvents <-> Clos Object layer  V 1.02 *
;;*                                           *
;;*     By Philippe  Piernot                  *
;;*        (piernot@ksl.stanford.edu)         *
;;*                                           *
;;* Copyright 1993 by Philippe Piernot.       *
;;* All Rights Reserved.                      *
;;*                                           *
;;* Permission is given to modify and         *
;;* distribute this source code provided that *
;;* the name of the author and this copyright *
;;* notice appear at the top.                 *
;;*                                           *
;;*********************************************

(in-package :cl-user)

(defconstant $kAECoreSuite             :|core|)

(defconstant $keyAEDesiredClass        :|want|)
(defconstant $keyAEContainer           :|from|)
(defconstant $keyAEKeyForm             :|form|)
(defconstant $keyAEKeyData             :|seld|)

(defconstant $typeObjectSpecifier      :|obj |)

(defconstant $formAbsolutePosition     :|indx|)
(defconstant $formRelativePosition     :|rele|)
(defconstant $formTest                 :|test|)
(defconstant $formRange	               :|rang|)
(defconstant $formPropertyID           :|prop|)
(defconstant $formName                 :|name|)

(defun make-aetoclos ()
  (let ((h (make-hash-table)))
    (setf (getHash :|aevt| h) 'AppleEvent)
    (setf (getHash :|cmpd| h) 'ComparisonDescriptor)
    (setf (getHash :|doub| h) 'double-float)
    (setf (getHash :|enum| h) 'Keyword)
    (setf (getHash :|fals| h) 'Null)
    (setf (getHash :|itxt| h) 'String)
    (setf (getHash :|list| h) 'List)
    (setf (getHash :|long| h) 'Bignum)
    (setf (getHash :|magn| h) 'Bignum)
    (setf (getHash :|null| h) 'Null)
    (setf (getHash :|obj | h) 'ObjectSpecifier)
    (setf (getHash :|rang| h) 'RangeDescriptor)
    (setf (getHash :|reco| h) 'Hash-Table)
    (setf (getHash :|shor| h) 'Fixnum)
    (setf (getHash :|sign| h) 'Keyword)
    (setf (getHash :|sing| h) 'Single-Float)
    (setf (getHash :|ssid| h) 'Fixnum)
    (setf (getHash :|TEXT| h) 'String)
    (setf (getHash :|true| h) 'T)
    (setf (getHash :|type| h) 'Keyword)
    h))

(defvar *AEToClos* (make-aetoclos))


(defun make-aetodesc ()
  (let ((h (make-hash-table)))
    (setf (getHash :|aevt| h) 'AppleEvent)
    (setf (getHash :|cmpd| h) 'AERecord)
    (setf (getHash :|list| h) 'AEDescList)
    (setf (getHash :|obj | h) 'AERecord)
    (setf (getHash :|rang| h) 'AERecord)
    (setf (getHash :|reco| h) 'AERecord)
    (setf (getHash :|targ| h) 'AEAddressDesc)
    h))
(defvar *AEToDesc* (make-aetodesc))


;;*******************************************************************
;;*                                                                 *
;;* AEDesc Class                                                    * 
;;*                                                                 *
;;*   nullAEDesc         ()                                         *
;;*   fromClosObject     ((self Symbol) &optional object type size) *
;;*   asClosObject       ((self AEDesc))                            *
;;*   dispose            ((self AEDesc))                            *
;;*   getDescriptorType  ((self AEDesc))                            *
;;*   getDataHandle      ((self AEDesc))                            *
;;*   duplicate          ((self AEDesc))                            *
;;*   coerceTo           ((self AEDesc) type)                       *
;;*   putInAE            ((self AEDesc) dataPtr)                    *
;;*   getAEType          ((self AEDesc))                            *
;;*                                                                 *
;;*******************************************************************


(defclass AEDesc (Standard-Object)
  ((descRecPtr :writer setDescRecPtr
               :reader getDescRecPtr)))

(defmethod initialize-Instance ((self AEDesc) &key 
                                object
                                (type    (getAEType object))
                                (recSize (getAESize object))
                                descRecPtr)
  (unless descRecPtr
    (%stack-Block ((dataPtr recSize))
      (putInAE object dataPtr)
      (setq descRecPtr (make-Record :AEDesc))
      (#_AECreateDesc 
       type 
       dataPtr
       recSize
       descRecPtr)))
  (setDescRecPtr descRecPtr self))

(defun nullAEDesc ()
  (make-Instance
    'AEDesc
    :descRecPtr (let ((descRecPtr (make-Record :aedesc)))
                  (#_AECreateDesc 
                   :|null| 
                   (%null-Ptr)
                   0
                   descRecPtr)
                  descRecPtr)))

(defmethod fromClosObject ((self Symbol) 
                           object &optional 
                           (type (getAEType object))
                           (size (getAESize object)))
  (make-Instance
    self
    :object  object
    :type    type 
    :recSize size))

(defmethod asClosObject ((self AEDesc))
  "Return the AE descriptor as a clos object"
  (fromAEDesc
   (getHash (getDescriptorType self) *AEToClos*)
   self))

(defmethod dispose ((self AEDesc))
  (let ((descRecPtr (getDescRecPtr self)))
    (#_AEDisposeDesc descRecPtr)
    (dispose-Record  descRecPtr)))

(defmethod getDescriptorType ((self AEDesc))
  (rref (getDescRecPtr self) :AEDesc.descriptorType))

(defmethod getDataHandle ((self AEDesc))
  (rref (getDescRecPtr self) :AEDesc.dataHandle))

(defmethod duplicate ((self AEDesc))
  (let ((result (make-Record :AEDesc)))
    (#_AEDuplicateDesc
     (getDescRecPtr self)
     result)
    (make-Instance (class-Name (class-Of self)) :descRecPtr result)))

(defmethod coerceTo ((self AEDesc) type)
  (let ((result (make-Record :AEDesc)))
    (#_AECoerceDesc
     (getDescRecPtr self)
     type
     result)
    (setDescRecPtr result self)
   ;AAA (dispose self)
   self))
  


(defmethod getAEType ((self AEDesc))
  (declare (ignore self))
  :|****|)


;;*************************************************************
;;*                                                           *
;;* AEAddressDesc Class                                       *
;;*                                                           *
;;*   fromPPCBrowser     ((self AEAddressDesc) prompt title)  *
;;*                                                           *
;;*************************************************************


(defclass AEAddressDesc (AEDesc)
  ())

(defmethod initialize-Instance ((self AEAddressDesc) &key
                                object
                                type 
                                recSize
                                descRecPtr)
  (declare (ignore type) (ignore recSize))
  (unless descRecPtr
    (setq descRecPtr (make-Record :AEDesc))
    (case (class-Name (class-Of object))
      (Cons    (apply 'create-Psn-Target    descRecPtr object))
      (Keyword (ui::create-Signature-Target     descRecPtr object))
      (String  (create-Named-Process-Target descRecPtr object))
      (Null    (create-Self-Target          descRecPtr))))
  (setDescRecPtr descRecPtr self))

(defun fromPPCBrowser (&key
                       (prompt "Choose an Application")
                       (title  "Applications"))
  (make-Instance
    'AEAddressDesc
    :descRecPtr (let ((descRecPtr (make-Record :AEAddressDesc)))
                  (choose-AppleEvent-Target 
                   descRecPtr 
                   :prompt prompt 
                   :title  title)
                  descRecPtr)))


;;*****************************************************************
;;*                                                               *
;;* AEDescList Class                                              *
;;*                                                               *
;;*   getSize            ((self AEDescList))                      *
;;*   deleteNthItem      ((self AEDescList) index)                *
;;*   setNthItem         ((self AEDescList) index item)           *
;;*   getNthItem         ((self AEDescList) index &optional type) *
;;*   setNthDesc         ((self AEDescList) index desc)           *
;;*   getNthDesc         ((self AEDescList) index &optional type) *
;;*   getAESizeOfNthDesc ((self AEDescList) index)                *
;;*   getAEType          ((self AEDescList))                      *
;;*                                                               *
;;*****************************************************************


(defclass AEDescList (AEDesc)
  ())

(defmethod initialize-Instance ((self AEDescList) &key
                                object
                                type 
                                recSize
                                descRecPtr)
  (declare (ignore type) (ignore recSize))
  (unless descRecPtr
    (setq descRecPtr (make-Record :AEDescList))
    (#_AECreateList (%null-Ptr) 0 nil descRecPtr)
    (setDescRecPtr descRecPtr self)
    (if object
      (loop for i from 1 to (length object) do
            (setNthItem self i (elt object (1- i))))))
  (setDescRecPtr descRecPtr self))

(defmethod getSize ((self AEDescList))
  (niy getSize self)
  ;; (rlet ((result :Signed-Long))         
  ;;   (#_AECountItems
  ;;    (getDescRecPtr self)
  ;;    result)
  ;;   (%get-Signed-Long result))
  )

(defmethod deleteNthItem ((self AEDescList) index)
  (#_AEDeleteItem
   (getDescRecPtr self)
   index))

(defmethod setNthItem ((self AEDescList) index item)
  (let ((desc (asAEDesc item)))
    (setNthDesc self index desc)
    (dispose desc)))

(defmethod getNthItem ((self AEDescList) index &optional (type :|****|))
  (multiple-Value-Bind (desc keyword)
    (getNthDesc self index type)
    (let ((item (asClosObject desc)))
      (dispose desc)
      (values item keyword))))

(defmethod setNthDesc ((self AEDescList) index desc)
  (#_AEPutDesc 
   (getDescRecPtr self)
   index
   (getDescRecPtr desc)))

(defmethod getNthDesc ((self AEDescList) index &optional (type :|****|))
  (niy getNthDesc self index type)
  ;; (let ((result (make-Record :AEDesc))
  ;;       (descClass nil))
  ;;   (rlet ((keyword :OSType))
  ;;     (#_AEGetNthDesc
  ;;      (getDescRecPtr self)
  ;;      index
  ;;      type
  ;;      keyword
  ;;      result)
  ;;     (setq descClass (getHash (rref result :AEDesc.descriptorType) *AEToDesc*))
  ;;     (unless descClass
  ;;       (setq descClass 'AEDesc))
  ;;     (values
  ;;      (make-Instance descClass :descRecPtr result)
  ;;      (%get-OSType keyword))))
  )

(defmethod getAESizeOfNthDesc ((self AEDescList) index)
  (niy getAESizeOfNthDesc self index)
  ;; (rlet ((size :Signed-Long)
  ;;        (type :OSType))
  ;;   (#_AESizeOfNthItem 
  ;;    (getDescRecPtr self) 
  ;;    index 
  ;;    type
  ;;    size)
  ;;   (values (%get-Signed-Long size) (%get-OSType type)))
  )

(defmethod getAEType ((self AEDescList))
  (declare (ignore self))
  :|list|)


;;*****************************************************************
;;*                                                               *
;;* AERecord Class                                                *
;;*                                                               *
;;*   deleteKeyItem      ((self AERecord) keyword)                *
;;*   setKeyItem         ((self AERecord) keyword item)           *
;;*   getKeyItem         ((self AERecord) keyword &optional type) *
;;*   setKeyDesc         ((self AERecord) keyword desc)           *
;;*   getKeyDesc         ((self AERecord) keyword &optional type) *
;;*   getAESizeOfKeyDesc ((self AERecord) keyword)                *
;;*   getAEType          ((self AERecord))                        *
;;*                                                               *
;;*****************************************************************


(defclass AERecord (AEDescList)
  ())

(defmethod initialize-Instance ((self AERecord) &key
                                object 
                                type 
                                recSize
                                descRecPtr)
  (declare (ignore type) (ignore recSize))
  (unless descRecPtr
    (setq descRecPtr (make-Record :AERecord))
    (#_AECreateList (%null-Ptr) 0 T descRecPtr)
    (setDescRecPtr descRecPtr self)
    (if object
      (maphash 
       (lambda (keyword item)
           (setKeyItem self keyword item))
       object)))
  (setDescRecPtr descRecPtr self))

(defmethod deleteKeyItem ((self AERecord) keyword)
  (#_AEDeleteKeyDesc 
   (getDescRecPtr self) 
   keyword))

(defmethod setKeyItem ((self AERecord) keyword item)
  (let ((desc (asAEDesc item)))
    (setKeyDesc self keyword desc)
    (dispose desc)))

(defmethod getKeyItem ((self AERecord) keyword &optional (type :|****|))
  (let* ((desc (getKeyDesc self keyword type))
         (item (asClosObject desc)))
    (dispose desc)
    item))

(defmethod setKeyDesc ((self AERecord) keyword desc)
  (#_AEPutKeyDesc 
   (getDescRecPtr self)
   keyword
   (getDescRecPtr desc)))

(defmethod getKeyDesc ((self AERecord) keyword &optional (type :|****|))
  (let ((result (make-Record :AEDesc))
        (descClass nil))
    (#_AEGetKeyDesc 
     (getDescRecPtr self)
     keyword
     type
     result)
    (setq descClass (getHash (rref result :AEDesc.descriptorType) *AEToDesc*))
    (unless descClass
        (setq descClass 'AEDesc))
    (make-Instance descClass :descRecPtr result)))

(defmethod getAESizeOfKeyDesc ((self AERecord) keyword)
  (niy getAESizeOfKeyDesc self keyword)
  ;; (rlet ((size :Signed-Long)
  ;;        (type :OSType))
  ;;   (#_AESizeOfNthItem 
  ;;    (getDescRecPtr self) 
  ;;    keyword 
  ;;    type
  ;;    size)
  ;;   (values (%get-Signed-Long size) (%get-OSType type)))
  )

(defmethod getAEType ((self AERecord))
  (declare (ignore self))
  :|reco|)


;;*********************************************************************
;;*                                                                   *
;;* AppleEvent Class                                                  *
;;*                                                                   *
;;*   deleteParam          ((self AppleEvent) keyword)                *
;;*   setParam             ((self AppleEvent) keyword item)           *
;;*   getParam             ((self AppleEvent) keyword &optional type) *
;;*   setParamDesc         ((self AppleEvent) keyword desc)           *
;;*   getParamDesc         ((self AppleEvent) keyword &optional type) *
;;*   getAESizeOfParam     ((self AppleEvent) keyword)                *
;;*   setAttribute         ((self AppleEvent) keyword item)           *
;;*   getAttribute         ((self AppleEvent) keyword &optional type) *
;;*   setAttributeDesc     ((self AppleEvent) keyword desc)           *
;;*   getAttributeDesc     ((self AppleEvent) keyword &optional type) *
;;*   getAESizeOfAttribute ((self AppleEvent) keyword)                *
;;*   getAEType            ((self AppleEvent))                        *
;;*                                                                   *
;;*********************************************************************


(defclass AppleEvent (AERecord)
  ())

(defmethod initialize-instance ((self AppleEvent) &key
                                object type recSize descRecPtr
                                class  id   target
                                (returnId      #$kAutoGenerateReturnID)
                                (transactionId #$kAnyTransactionID))
  (declare (ignore object) (ignore type) (ignore recSize))
  (unless descRecPtr
    (setq descRecPtr (make-Record :AppleEvent))
    (#_AECreateAppleEvent
     class
     id 
     (getDescRecPtr target)
     returnId 
     transactionId
     descRecPtr))
  (setDescRecPtr descRecPtr self))

(defmethod deleteParam ((self AppleEvent) keyword)
  (#_AEDeleteParam 
   (getDescRecPtr self) 
   keyword))

(defmethod setParam ((self AppleEvent) keyword item)
  (let ((desc (asAEDesc item)))
    (setParamDesc self keyword desc)
    (dispose desc)))

(defmethod getParam ((self AppleEvent) keyword &optional (type :|****|))
  (let* ((desc (getParamDesc self keyword type))
         (item (asClosObject desc)))
    (dispose desc)
    item))

(defmethod setParamDesc ((self AppleEvent) keyword desc)
  (#_AEPutParamDesc
   (getDescRecPtr self)
   keyword
   (getDescRecPtr desc)))

(defmethod getParamDesc ((self AppleEvent) keyword &optional (type :|****|))
  (let ((result (make-Record :AEDesc))
        (descClass nil))
    (#_AEGetParamDesc 
     (getDescRecPtr self)
     keyword
     type
     result)
    (setq descClass (getHash (rref result :aedesc.descriptorType) *AEToDesc*))
    (unless descClass
        (setq descClass 'AEDesc))
    (make-Instance descClass :descRecPtr result)))

(defmethod getAESizeOfParam ((self AppleEvent) keyword)
  (niy getAESizeOfParam self keyword)
  ;; (rlet ((size :Signed-Long)
  ;;        (type :OSType))
  ;;   (#_AESizeOfParam 
  ;;    (getDescRecPtr self) 
  ;;    keyword 
  ;;    type
  ;;    size)
  ;;   (values (%get-Signed-Long size) (%get-OSType type)))
  )

(defmethod setAttribute ((self AppleEvent) keyword item)
  (let ((desc (asAEDesc item)))
    (setAttributeDesc self keyword desc)
    (dispose desc)))

(defmethod getAttribute ((self AppleEvent) keyword &optional (type :|****|))
  (let* ((desc (getAttributeDesc self keyword type))
         (item (asClosObject desc)))
    (dispose desc)
    item))

(defmethod setAttributeDesc ((self AppleEvent) keyword desc)
  (#_AEPutAttributeDesc 
   (getDescRecPtr self)
   keyword
   (getDescRecPtr desc)))

(defmethod getAttributeDesc ((self AppleEvent) keyword &optional (type :|****|))
  (let ((result (make-Record :AEDesc))
        (descClass nil))
    (#_AEGetAttributeDesc 
     (getDescRecPtr self)
     keyword
     type
     result)
    (setq descClass (getHash (rref result :AEDesc.descriptorType) *AEToDesc*))
    (unless descClass
        (setq descClass 'AEDesc))
    (make-Instance descClass :descRecPtr result)))

(defmethod getAESizeOfAttribute ((self AppleEvent) keyword)
  (niy getAESizeOfAttribute self keyword)
  ;; (rlet ((size :Signed-Long)
  ;;        (type :OSType))
  ;;   (#_AESizeOfAttribute 
  ;;    (getDescRecPtr self) 
  ;;    keyword 
  ;;    type
  ;;    size)
  ;;   (values (%get-Signed-Long size) (%get-OSType type)))
  )

(defmethod getAEType ((self AppleEvent))
  (declare (ignore self))
  :|aevt|)

(defmethod send ((self AppleEvent) &key
                 (reply-mode :no-reply) 
                 (interact-mode nil)
                 (can-switch-layer nil)
                 (dont-reconnect nil)
                 (want-receipt nil) 
                 (priority #$kAENormalPriority)
                 (timeout #$kAEDefaultTimeout)
                 (idleproc appleevent-idle)
                 filterproc)
  (let ((reply (make-Record :aedesc)))
    (send-appleevent 
     (getDescRecPtr self)
     reply
     :reply-mode reply-mode 
     :interact-mode interact-mode
     :can-switch-layer can-switch-layer
     :dont-reconnect dont-reconnect
     :want-receipt want-receipt
     :priority priority
     :timeout timeout
     :idleproc idleproc
     :filterproc filterproc)
    (make-instance 'AppleEvent :descRecPtr reply)
    ))


;;**************************
;;*                        *
;;* OffsetDescriptor Class *
;;*                        *
;;**************************


(defclass OffsetDescriptor (AERecord)
  ())


;;******************************
;;*                            *
;;* ComparisonDescriptor Class *
;;*                            *
;;******************************


(defclass ComparisonDescriptor (AERecord)
  ())


;;***************************
;;*                         *
;;* LogicalDescriptor Class *
;;*                         *
;;***************************


(defclass LogicalDescriptor (AERecord)
  ())


;;*************************
;;*                       *
;;* RangeDescriptor Class *
;;*                       *
;;*************************


(defclass RangeDescriptor (AERecord)
  ())


;;********************************************************
;;*                                                      *
;;* ObjectSpecifier Class                                *
;;*                                                      *
;;*   fromAEDesc ((self (eql 'ObjectSpecifier)) desc)    *
;;*   asAEDesc   ((self ObjectSpecifier) &optional type) *
;;*   getAEType  ((self ObjectSpecifier))                *
;;*                                                      *
;;********************************************************


(defclass ObjectSpecifier (Standard-Object)
  ((class     :reader getClass
              :writer setClass)
   (container :reader getContainer
              :writer setContainer)
   (form      :reader getForm
              :writer setForm)
   (data      :reader getData
              :writer setData)))

(defmethod initialize-instance ((self ObjectSpecifier) &key
                                class
                                container
                                form
                                data)
  (setClass class self)
  (setContainer container self)
  (setForm form self)
  (setData data self))

(defmethod fromAEDesc ((self (eql 'ObjectSpecifier)) desc)
  (declare (ignore self))
  (setq desc (coerceTo desc :|reco|))
  (make-instance
    'ObjectSpecifier
    :class     (getKeyItem desc $keyAEDesiredClass)
    :container (getKeyItem desc $keyAEContainer)
    :form      (getKeyItem desc $keyAEKeyForm)
    :data      (getKeyItem desc $keyAEKeyData)))

(defmethod asAEDesc ((self ObjectSpecifier) &optional 
                     (type (getAEType self)))
  (let ((rec  (make-instance 'AERecord))
        (cont (getContainer self))
        (form (make-instance 
                'AEDesc 
                :object (getForm self)
                :type   :|enum|)))
    (setKeyItem rec $keyAEDesiredClass (getClass self))
    (if cont
      (setKeyItem rec $keyAEContainer  cont)
      (setKeyDesc rec $keyAEContainer  (nullAEDesc)))
    (setKeyDesc rec $keyAEKeyForm      form)
    (dispose form)
    (setKeyItem rec $keyAEKeyData      (getData self))
    (coerceTo rec type)))

(defmethod getAEType ((self ObjectSpecifier))
  (declare (ignore self))
  :|obj |)


;;**************************************
;;*                                    *
;;* T Class                            *
;;*                                    *
;;*   fromAEDesc ((self (eql T)) desc) *
;;*   asAEDesc   ((self T))            *
;;*   putInAE    ((self T) dataPtr)    *
;;*   getAEType  ((self T))            *
;;*   getAESize  ((self T))            *
;;*                                    *
;;**************************************


(defmethod fromAEDesc ((self (eql T)) desc)
  (declare (ignore self) (ignore desc))
  T)

(defmethod asAEDesc ((self T) &optional 
                     (type (getAEType self)))
  (make-instance 
    'AEDesc 
    :object self 
    :type   type))

(defmethod putInAE ((self T) dataPtr)
  (declare (ignore self))
  (%put-word dataPtr 1))

(defmethod getAEType ((self T))
  (declare (ignore self))
  :|true|)

(defmethod getAESize ((self T))
  (declare (ignore self))
  2)


;;******************************************
;;*                                        *
;;* List Class                             *
;;*                                        *
;;*   fromAEDesc ((self (eql 'List)) desc) *
;;*   asAEDesc   ((self List))             *
;;*   getAEType  ((self List))             *
;;*   getAESize  ((self List))             *
;;*                                        *
;;******************************************


(defmethod fromAEDesc ((self (eql 'List)) descriptor)
  (declare (ignore self))
  (let ((list '())
        (descriptorList (make-instance 'AEDescList :descRecPtr (getdescRecPtr descriptor) )))
    (loop for i from 1 to (getSize descriptorList) do
      (setq list (append list (list (getNthItem descriptorList i)))))
    list))

(defmethod asAEDesc ((self List) &optional 
                     (type (getAEType self)))
  (make-instance 
    'AEDescList
    :object self
    :type   type))

(defmethod getAEType ((self List))
  (declare (ignore self))
  :|list|)

(defmethod getAESize ((self List))     
  (declare (ignore self)))


;;************************************************
;;*                                              *
;;* Hash-Table Class                             *
;;*                                              *
;;*   fromAEDesc ((self (eql 'Hash-Table)) desc) *
;;*   getAEType  ((self Hash-Table))             *
;;*   getAESize  ((self Hash-Table))             *
;;*                                              *
;;************************************************


(defmethod fromAEDesc ((self (eql 'Hash-Table)) aerecord)
  (declare (ignore self))
  (let ((htab  (make-Hash-Table)))
    (loop for i from 1 to (getSize aerecord) do
      (multiple-value-bind (value keyword)
        (getNthItem aerecord i)
        (setf (getHash keyword htab) value)))
    htab))

(defmethod asAEDesc ((self Hash-Table) &optional 
                     (type (getAEType self)))
  (make-instance 
    'AERecord
    :object self
    :type   type))

(defmethod getAEType ((self Hash-Table))
  (declare (ignore self))
  :|reco|)


;;********************************************
;;*                                          *
;;* Fixnum Class                             *
;;*                                          *
;;*   fromAEDesc ((self (eql 'Fixnum)) desc) *
;;*   putInAE    ((self Fixnum) dataPtr)     *
;;*   getAEType  ((self Fixnum))             *
;;*   getAESize  ((self Fixnum))             *
;;*                                          *
;;********************************************


(defmethod fromAEDesc ((self (eql 'Fixnum)) desc)
  (declare (ignore self))
  (%hget-Word (getDataHandle desc)))



(defmethod putInAE ((self Fixnum) dataPtr)
  (%put-Word dataPtr self))

(defmethod getAEType ((self Fixnum))
  (declare (ignore self))
  :|shor|)

(defmethod getAESize ((self Fixnum))
  (declare (ignore self))
  2)


;;********************************************
;;*                                          *
;;* Double-float                             *
;;*                                          *
;;*   fromAEDesc ((self (eql 'double-float)) desc) *
;;*   putInAE    ((self double-float) dataPtr)     *
;;*   getAEType  ((self double-float))             *
;;*   getAESize  ((self double-float))             *
;;*                                          *
;;********************************************

(DEFUN AS-GET-STRING (data)
  (let* ((size (#_GetHandleSize data))
            (text (make-string size)))
     (dotimes (i size)
        (setf (char text i) (code-char (%hget-byte data i))))
     text))

(defmethod fromAEDesc ((self (eql 'double-float)) desc)
  (declare (ignore self))
  (read-from-string 
   (with-aedescs (coerced.desc)
     (let ((err (#_AECoerceDesc (getDescRecPtr desc)
                 #$typechar coerced.desc)))
       (cond
        ((zerop err)
         (as-get-string (rref coerced.desc AEDesc.dataHandle))))))))

(defmethod putInAE ((self double-float) dataPtr)
  (%put-Word dataPtr self))

(defmethod getAEType ((self double-float))
  (declare (ignore self))
  :|doub|)

(defmethod getAESize ((self double-float))
  (declare (ignore self))
  2)


;;********************************************
;;*                                          *
;;* Bignum Class                             *
;;*                                          *
;;*   fromAEDesc ((self (eql 'Bignum)) desc) *
;;*   putInAE    ((self Bignum) dataPtr)     *
;;*   getAEType  ((self Bignum))             *
;;*   getAESize  ((self Bignum))             *
;;*                                          *
;;********************************************


(defmethod fromAEDesc ((self (eql 'Bignum)) desc)
  (declare (ignore self))
  (%hget-Long (getDataHandle desc)))

(defmethod putInAE ((self Bignum) dataPtr)
  (%put-Long dataPtr self))

(defmethod getAEType ((self Bignum))
  (declare (ignore self))
  :|long|)

(defmethod getAESize ((self Bignum))
  (declare (ignore self))
  4)


;;*********************************************
;;*                                           *
;;* Keyword Class                             *
;;*                                           *
;;*   fromAEDesc ((self (eql 'Keyword)) desc) *
;;*   putInAE    ((self Keyword) dataPtr)     *
;;*   getAEType  ((self Keyword))             *
;;*   getAESize  ((self Keyword))             *
;;*                                           *
;;*********************************************


(defmethod fromAEDesc ((self (eql 'Keyword)) desc)
  (declare (ignore self))
  (%get-OSType (%get-Ptr (getDataHandle desc))))

(defmethod putInAE ((self Keyword) dataPtr)
  (%put-OSType dataPtr self))

(defmethod getAEType ((self Keyword))
  (declare (ignore self))
  :|type|)

(defmethod getAESize ((self Keyword))
  4)


;;********************************************
;;*                                          *
;;* String Class                             *
;;*                                          *
;;*   fromAEDesc ((self (eql 'String)) desc) *
;;*   putInAE    ((self String) dataPtr)     *
;;*   getAEType  ((self String))             *
;;*   getAESize  ((self String))             *
;;*                                          *
;;********************************************


(defmethod fromAEDesc ((self (eql 'String)) desc)
  (declare (ignore self))
  (with-dereferenced-handles ((ptr (getDataHandle desc)))
    (ui::%str-from-ptr ptr (#_GetHandleSize (getDataHandle desc)))))

(defmethod putInAE ((self String) dataPtr)
  (%put-CString dataPtr self))


(defmethod getAEType ((self String))
  (declare (ignore self))
  :|TEXT|)


(defmethod getAESize ((self String))
  (length self))
 

;;******************************************
;;*                                        *
;;* Null Class                             *
;;*                                        *
;;*   fromAEDesc ((self (eql 'Null)) desc) *
;;*   putInAE    ((self Null) dataPtr)     *
;;*   getAEType  ((self Null))             *
;;*   getAESize  ((self Null))             *
;;*                                        *
;;******************************************


(defmethod fromAEDesc ((self (eql 'Null)) desc)
  (declare (ignore self) (ignore desc))
  nil)

(defmethod putInAE ((self Null) dataPtr)
  (declare (ignore self))
  (%put-Word dataPtr 0))

(defmethod getAEType ((self Null))
  (declare (ignore self))
  :|fals|)

(defmethod getAESize ((self Null))
  (declare (ignore self))
  2)




(defmethod asAEDesc ((self integer) &optional 
                     (type (getAEType self)))
  (declare (ignore type))
  (let* ((thedesc (make-Record :AEDesc)))
    (%stack-block ((int-ptr 4))
      (%put-long int-ptr self)
      (ae-error (#_AECreateDesc #$typeInteger int-ptr 4 thedesc)))
    (make-instance 'AEDesc :descRecPtr thedesc)))


(defmethod asAEDesc ((self String) &optional 
                     (type (getAEType self)))
  (declare (ignore type))
  (let* ((thedesc (make-Record :AEDesc)))
    (with-cstrs ((cstring self))
      (ae-error (#_AECreateDesc #$typeChar cstring (length self) thedesc)))
    (make-instance 'AEDesc :descRecPtr thedesc)))

(defmethod asAEDesc ((self symbol) &optional 
                     (type (getAEType self)))
 (if (equal self t)
    (let ((thedesc (make-Record :AEDesc)))
      (%stack-block ((bool-ptr 1))
        (%put-byte bool-ptr -1)
        (ae-error (#_AECreateDesc type bool-ptr 1 thedesc)))
      (make-instance 'AEDesc :descRecPtr thedesc))
    (make-instance 'AEDesc :object self :type  type)))

(defmethod asAEDesc ((self character) &optional 
                     (type (getAEType self)))
  (declare (ignore type))
  (asAEDesc (string self)))


(defmethod asAEDesc ((self float) &optional 
                     (type (getAEType self)))
  (declare (ignore type))
  (let ((float-string (format nil "~f" self))
        (thedesc (make-Record :AEDesc)))
    (list->aedesc-by-type #$TypeFloat float-string thedesc)
    (make-instance 'AEDesc :descRecPtr thedesc)))


(defmethod asAEDesc ((self null) &optional 
                                   (type (getAEType self)))
  (let ((thedesc (make-Record :AEDesc)))
    (%stack-block ((bool-ptr 1))
      (%put-byte bool-ptr 0)
      (ae-error (#_AECreateDesc type bool-ptr 1 thedesc)))
    (make-instance 'AEDesc :descRecPtr thedesc)))


(defmethod list->aedesc-by-type ((desctype t) data descriptor)
  (ae-error (#_AECoerceDesc (getdescRecPtr (asAEDesc data )) desctype descriptor))
  descriptor)



(defmethod getKeyItem ((self AEDesc) keyword &optional (type :|****|))
  (let* ((desc (getKeyDesc self keyword type))
         (item (asClosObject desc)))
    (dispose desc)
    item))

(defmethod getKeyDesc ((self AEDesc) keyword &optional (type :|****|))
  (let ((result (make-Record :AEDesc))
        (descClass nil))
    (#_AEGetKeyDesc 
     (getDescRecPtr self)
     keyword
     type
     result)
    (setq descClass (getHash (rref result :AEDesc.descriptorType) *AEToDesc*))
    (unless descClass
        (setq descClass 'AEDesc))
    (make-Instance descClass :descRecPtr result)))
