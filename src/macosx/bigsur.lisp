(in-package "COMMON-LISP-USER")

(require :cocoa)



#-(and)
(ignore
 (list (sel-get-name (objc:@selector "sharedApplication"))
       (sel-get-uid "sharedApplication")
       (sel-is-mapped (objc:@selector "sharedApplication")))
("sharedApplication" #<A Foreign Pointer #x7FFF7BC1FE34> t)
 )








 #|

(dlsym (ccl::shlib.handle *libobjc*) "class_getClassMethod")


(class-get-class-method  [NSApplication class] (objc:@selector "sharedApplication"))
#<ns-date 2001-01-01 00:00:00 +0000 (#x7FFF23B29EE5)>     
(type-of (class-get-class-method  [NSApplication class] (objc:@selector "sharedApplication")))

(ccl:external-call "class_getClassMethod" 
                   :<c>lass (objc:send ns:+ns-application 'class)
                   :<sel> (objc:\@SELECTOR "sharedApplication")
                   :<m>ethod)
--> #<ns-date 2001-01-01 00:00:00 +0000 (#x7FFF23B29EE5)>

(ql:quickload "cffi")
(cffi:defcfun (class-get-class-method "class_getClassMethod") :pointer (id :pointer) (sel :pointer))
(class-get-class-method (objc:objc-message-send ns:+ns-application "class" :<c>lass)
                        (objc:@selector "sharedApplication"))
--> #<ns-date 2001-01-01 00:00:00 +0000 (#x7FFF23B29EE5)>

(Ignore
 (macroexpand-1 '[NSApplication sharedApplication])
 (ccl::with-ns-exceptions-as-errors (ccl:rlet nil (objc:objc-message-send ns:+ns-application "sharedApplication" :id)))
 t
 (#/class_getName [NSApplication class])
 (#/class_getClassMethod [NSApplication class] (objc:@selector "sharedApplication"))
 
 )
;; (load-library "/usr/lib/libobjc-trampolines.dylib")

(apropos "MACPTR")
 arch::error-object-not-macptr                          V  140
  ccl::%consmacptr%                                        
  ccl::%copy-macptr-to-macptr                              
  ccl::%DOUBLE-FLOAT-FROM-MACPTR\!                       F 
  ccl::%fixnum-from-macptr                               F 
  ccl::%fixnum-ref-macptr                                F 
  ccl::%fixnum-set-macptr                                F 
  ccl::%ivector-from-macptr                              F 
  ccl::%macptr->dead-macptr                              F 
  ccl::%macptr-allocation-string                         F 
  ccl::%macptr-domain                                    F 
  ccl::%macptr-foreign                                     
  ccl::%macptr-hash                                      F 
  ccl::%macptr-type                                      F 
  ccl::%macptrptr%                                         
  ccl::%ordinal-type-class-for-macptr                    F 
  ccl::%revive-macptr                                    F 
  ccl::%set-macptr-domain                                F 
  ccl::%set-macptr-type                                  F 
  ccl::%set-new-macptr-value                               
  ccl:%setf-macptr                                       F 
  ccl::%setf-macptr-to-object                            F 
  ccl::%vect-data-to-macptr                              F 
  ccl::%write-macptr-allocation-info                     F 
  ccl::%write-macptr-type-info                           F 
  ccl::*macptr-class*                                   V  #<built-in-class ccl:macptr>
  ccl::copy-foreign-macptr-type                          F 
  ccl::dead-macptr                                         
  ccl::dead-macptr-p                                     F 
  ccl::deref-macptr                                        
  ccl::describe-macptr-allocation-and-address            F 
  ccl::encoded-gpr-macptr                                F 
  ccl::foreign-macptr                                      
  ccl::foreign-macptr-type                                 
  ccl::foreign-macptr-type-alignment                     F 
  ccl::foreign-macptr-type-bits                          F 
  ccl::foreign-macptr-type-class                         F 
  ccl::foreign-macptr-type-ordinal                       F 
  ccl::foreign-macptr-type-p                             F 
  ccl::indexed-gpr-macptr                                F 
  ccl:macptr                                               
  ccl::macptr->fixnum                                    F 
  ccl::macptr->stack                                       
  ccl::macptr-deport-gen-method                          F 
  ccl::macptr-extract-gen-method                         F 
  ccl::macptr-flags                                      F 
  ccl::macptr-foreign-rep-method                         F 
  ccl::macptr-lisp-rep-method                            F 
  ccl::macptr-naturalize-gen-method                      F 
  ccl::macptr-ptr                                          
  ccl::macptr-unparse-method                             F 
  ccl:macptrp                                            F 
  ccl::make-foreign-macptr-type                          F 
  ccl::make-gcable-macptr                                F 
  ccl::nx1-%setf-macptr                                    
  ccl::register-xmacptr-dispose-function                 F 
  ccl::set-%gcable-macptrs%                              F 
  ccl::set-macptr-address                                  
  ccl::set-macptr-flags                                  F 
  ccl::setup-macptr-allocation                             
  ccl::trap-unless-macptr                                  
  ccl::with-area-macptr                                  M 
  ccl:with-macptrs                                       M 
  ccl::write-a-dead-macptr                               F 
  ccl::write-a-macptr                                    F 
  ccl::x862-%consmacptr%                                   
  ccl::x862-%macptrptr%                                    
  ccl::x862-%setf-macptr                                   
  ccl::x862-macptr->heap                                 F 
  ccl::x862-macptr-arg-to-reg                            F 
  ccl::x862-store-macptr                                 F 
     :macptr                                            V  :macptr
x8632::kernel-import-register-xmacptr-dispose-function  V  48
x8632::macptr-header                                    V  799
x8632::macptr.address                                   V  -2
x8632::macptr.address-cell                              V  0
x8632::macptr.domain                                    V  2
x8632::macptr.domain-cell                               V  1
x8632::macptr.element-count                             V  3
x8632::macptr.header                                    V  -6
x8632::macptr.size                                      V  16
x8632::macptr.type                                      V  6
x8632::macptr.type-cell                                 V  2
x8632::subtag-dead-macptr                               V  39
x8632::subtag-macptr                                    V  31
x8632::xmacptr.address                                  V  -2
x8632::xmacptr.address-cell                             V  0
x8632::xmacptr.domain                                   V  2
x8632::xmacptr.domain-cell                              V  1
x8632::xmacptr.element-count                            V  5
x8632::xmacptr.flags                                    V  10
x8632::xmacptr.flags-cell                               V  3
x8632::xmacptr.header                                   V  -6
x8632::xmacptr.link                                     V  14
x8632::xmacptr.link-cell                                V  4
x8632::xmacptr.size                                     V  24
x8632::xmacptr.type                                     V  6
x8632::xmacptr.type-cell                                V  2
x8664::kernel-import-register-xmacptr-dispose-function  V  96
x8664::macptr-header                                    V  794
x8664::macptr.address                                   V  -5
x8664::macptr.address-cell                              V  0
x8664::macptr.domain                                    V  3
x8664::macptr.domain-cell                               V  1
x8664::macptr.element-count                             V  3
x8664::macptr.header                                    V  -13
x8664::macptr.size                                      V  32
x8664::macptr.type                                      V  11
x8664::macptr.type-cell                                 V  2
x8664::subtag-dead-macptr                               V  42
x8664::subtag-macptr                                    V  26
x8664::xmacptr.address                                  V  -5
x8664::xmacptr.address-cell                             V  0
x8664::xmacptr.domain                                   V  3
x8664::xmacptr.domain-cell                              V  1
x8664::xmacptr.element-count                            V  5
x8664::xmacptr.flags                                    V  19
x8664::xmacptr.flags-cell                               V  3
x8664::xmacptr.header                                   V  -13
x8664::xmacptr.link                                     V  27
x8664::xmacptr.link-cell                                V  4
x8664::xmacptr.size                                     V  48
x8664::xmacptr.type                                     V  11
x8664::xmacptr.type-cell                                V  2
|#
