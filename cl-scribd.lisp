;;;; cl-scribd.lisp

(in-package #:cl-scribd)

;; insert credentials here
(defparameter *api-root* "http://api.scribd.com/api")
(defparameter *api-key* "")
(defparameter *api-secret* "") ; for request signing

;; http://www.scribd.com/developers/api?method_name=collections.addDoc
;;
;; This method adds a document to an existing collection.
;; Will return a blank OK response object if successful.
;;
;(build-api-call collections.addDoc (doc_id collection_id))

;; http://www.scribd.com/developers/api?method_name=collections.getList
;;
;; This method retrieves a list of collections for a given user.
;;
;(build-api-call collections.getList ())

;; http://www.scribd.com/developers/api?method_name=collections.removeDoc
;;
;; This method removes a document from an existing collection.
;; Will return a blank OK response object if successful.
;;
;(build-api-call collections.removeDoc (doc_id collection_id))

;; http://www.scribd.com/developers/api?method_name=collections.listDocs
;;
;; This method retrieves a list of documents in a given collection.
;;
;(build-api-call collections.listDocs (collection_id))

;; http://www.scribd.com/developers/api?method_name=docs.upload
;;
;; This method accepts the document file via Multi-Part POST.
;;
;(build-api-call docs.upload (file)

;; http://www.scribd.com/developers/api?method_name=docs.uploadFromUrl
;;
;; This method retrieves the document file from the specified URL.
;;
;(build-api-call docs.uploadFromUrl (url))

;; http://www.scribd.com/developers/api?method_name=docs.getList
;;
;; This method retrieves a list of documents for a given user.
;;
;(build-api-call docs.getList ())

;; http://www.scribd.com/developers/api?method_name=docs.getConversionStatus
;;
;; This method retrieves the conversion status of the document.
;;
;(build-api-call docs.getConversionStatus (doc_id))

;; http://www.scribd.com/developers/api?method_name=docs.getSettings
;;
;; This method retrieves the meta-data for existing documents.
;;
;(build-api-call docs.getSettings (doc_id))

;; http://www.scribd.com/developers/api?method_name=docs.changeSettings
;;
;; This method updates the meta-data for existing documents.
;; Only send arguments for fields you would like to overwrite.
;; Will return a blank response object if succesful
;;
;(build-api-call docs.changeSettings (doc_ids)

;; http://www.scribd.com/developers/api?method_name=docs.getDownloadUrl
;;
;; This method returns a link you can use to download a static version of a document.
;;
;(build-api-call docs.getDownloadUrl (doc_id doc_type))

;; http://www.scribd.com/developers/api?method_name=docs.getStats
;;
;; This method retrieves statistics of the document.
;;
;(build-api-call docs.getStats (doc_id))

;; http://www.scribd.com/developers/api?method_name=docs.delete
;;
;; This method deletes an existing document.
;; Will return a blank OK response object if successful.
;;
;(build-api-call docs.delete (doc_id))

;; http://www.scribd.com/developers/api?method_name=docs.search
;;
;; This method searches for the specified query in the public documents on Scribd.com.
;; Private documents are not searchable.
;;
;(build-api-call docs.search (query))

;; http://www.scribd.com/developers/api?method_name=docs.getCategories
;;
;; This method returns a list of categories or subcategories.
;;
;(build-api-call docs.getCategories ())

;; http://www.scribd.com/developers/api?method_name=docs.featured
;;
;; This method returns a list of featured documents.
;;
;(build-api-call docs.featured ())

;; http://www.scribd.com/developers/api?method_name=docs.browse
;;
;; This method returns a list of documents that meet filter criteria.
;;
;(build-api-call docs.browse ())

;; http://www.scribd.com/developers/api?method_name=docs.uploadThumb
;;
;; This method accepts a document thumbnail file via Multi-Part POST.
;;
;(build-api-call docs.uploadThumb (file doc_id))

;; http://www.scribd.com/developers/api?method_name=thumbnail.get
;;
;; This method retrieves a URL to the thumbnail of a document, in a given size.
;;
;(build-api-call thumbnail.get (doc_id))

;; http://www.scribd.com/developers/api?method_name=user.login
;;
;; This method allows your API application to sign in as an existing Scribd user,
;; executing methods as that user.
;;
;(build-api-call user.login (username password))

;; http://www.scribd.com/developers/api?method_name=user.signup
;;
;; This method allows your API application to signup a new Scribd user. If the signup is successful,
;; your application will be passed back a session key which will allow you to execute methods on behalf of the new user.
;;
;(build-api-call user.signup (username password email))

;; http://www.scribd.com/developers/api?method_name=user.getAutoSigninUrl
;;
;; This method returns a URL that, when visited, will automatically sign in
;; the given user account and then redirect to the URL you provide.
;;
;(build-api-call user.getAutoSigninUrl (next_url))

;; Macro for generating API calls to Scribd methods
;; Required parameters <method> and <api_key> included by default
;;
;; Usage: (build-api-call docs.getList ())
;; 
;; The we can call: (docs.getList)
;;
(defmacro build-api-call (fname (&rest params))
  `(defun ,fname (&key (http-method :get) (verbose nil) (signed nil) ,@params)
     (let ((parameters `(("api_key" . ,*api-key*)
                         ("method" . ,(string-downcase ',fname)))))
       ,@(mapcar
          (lambda (s)
            `(when ,s
               (push (cons `,(string-downcase ',s) ,s) parameters)))
          (reverse params))
       (multiple-value-bind (body status header uri) 
           (drakma:http-request *api-root*
                                :method http-method
                                :parameters (if signed
                                                (build-api-sig parameters)
                                                parameters))
         (let ((result (cxml:parse body (cxml:make-string-sink))))
           (if verbose
               (values result header uri status)
               result))))))
       
;; http://www.scribd.com/developers/api?method_name=Signing
;;
;; To activate signing, go to your account options page and change the "Require API Signature" option.
;;
(defun build-api-sig (params)
  (let* ((sorted-params (sort params #'string< :key 'car))
         (concat (concatenate 'string *api-secret* (format nil "~{~{~a~}~}" (mapcar #'(lambda (x)
                                                                                        `(,(car x) ,(cdr x)))
                                                                                    sorted-params))))
         (md5hash (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :md5 (ironclad:ascii-string-to-byte-array concat)))))
    (append sorted-params `(("api_sig" . ,md5hash)))))