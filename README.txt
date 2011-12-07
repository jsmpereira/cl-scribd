Common Lisp Client for the Scribd API.
http://www.scribd.com/developers

Setup:

Clone into a dir that quicklisp knows about.
(ql:quickload :cl-scribd)
(in-package :cl-scribd)
(setf *api-key* "my-api-key")

Basically consists of one macro for building api calls.
There are examples of most API calls in the code.

Structure: (build-api-call <method-name> <args-list>)

Build the call: (build-api-call docs.getList ())
Use it: (docs.getList)

Something more elaborate:

Build: (build-api-call docs.getDownloadUrl (doc_id doc_type))
Call: (docs.getDownloadUrl :doc_id "60920304" :doc_type "original")

Result:
"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<rsp stat=\"ok\">
  <download_link>
    <![CDATA[http://documents.scribd.com.s3.amazonaws.com/docs/2e5phql3y811zyp2.doc?t=1311652082]]>
  </download_link>
</rsp>"
