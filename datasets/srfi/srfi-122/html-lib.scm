(declare (fixnum) (not safe))

;;; This is an attempt at a complete implementation of HTML 4.0 transitional in Scheme.

;;; All html tags are translated to scheme functions with name <tagname> that takes keyword
;;; arguments for the attributes and an optional rest parameter if the tag has an end-tag.

(define-structure html-form name attributes body start-newline? end-tag-start-newline? end-tag?)

(define-structure html-raw value)

(define-structure attribute name takes-value? value)

(define html:degree (integer->char 176)) ; will be translated by html-display

;;; Display the structure we built, which is a tree of Scheme objects.

(define (html->string x)
  (let ((port (open-output-string)))
    (html-display x port)
    (get-output-string port)))

(define (html-display x . possible-port)

;;; This table has a non-#f entry for every character that is valid in
;;; a standard HTML document.  The entry is what should be displayed when
;;; this character occurs.

  (define character-entity-table
    '#(#\nul
       #f; #\#x1
       #f; #\#x2
       #f; #\#x3
       #f; #\#x4
       #f; #\#x5
       #f; #\#x6
       #f; #\bel
       #f; #\backspace
       #\tab
       #\newline
       #f; #\vt
       #f; #\page
       #\return
       #f; #\#xE
       #f; #\#xF
       #f; #\#x10
       #f; #\#x11
       #f; #\#x12
       #f; #\#x13
       #f; #\#x14
       #f; #\#x15
       #f; #\#x16
       #f; #\#x17
       #f; #\#x18
       #f; #\#x19
       #f; #\#x1A
       #f; #\#x1B
       #f; #\#x1C
       #f; #\#x1D
       #f; #\#x1E
       #f; #\#x1F
       #\space
       #\!
       "&quot;"
       #\#
       #\$
       #\%
       "&amp;"
       #\'
       #\(
       #\)
       #\*
       #\+
       #\,
       #\-
       #\.
       #\/
       #\0
       #\1
       #\2
       #\3
       #\4
       #\5
       #\6
       #\7
       #\8
       #\9
       #\:
       #\;
       "&lt;"
       #\=
       "&gt;"
       #\?
       #\@
       #\A
       #\B
       #\C
       #\D
       #\E
       #\F
       #\G
       #\H
       #\I
       #\J
       #\K
       #\L
       #\M
       #\N
       #\O
       #\P
       #\Q
       #\R
       #\S
       #\T
       #\U
       #\V
       #\W
       #\X
       #\Y
       #\Z
       #\[
       #\\
       #\]
       #\^
       #\_
       #\`
       #\a
       #\b
       #\c
       #\d
       #\e
       #\f
       #\g
       #\h
       #\i
       #\j
       #\k
       #\l
       #\m
       #\n
       #\o
       #\p
       #\q
       #\r
       #\s
       #\t
       #\u
       #\v
       #\w
       #\x
       #\y
       #\z
       #\{
       #\|
       #\}
       #\~
       #f; #\rubout
       #f; "&#128;"
       #f; "&#129;"
       "&#130;"
       "&#131;"
       "&#132;"
       "&#133;"
       "&#134;"
       "&#135;"
       "&#136;"
       "&#137;"
       "&#138;"
       "&#139;"
       "&#140;"
       #f; "&#141;"
       "&#142;"
       #f; "&#143;"
       #f; "&#144;"
       "&#145;"
       "&#146;"
       "&#147;"
       "&#148;"
       "&#149;"
       "&#150;"
       "&#151;"
       "&#152;"
       "&#153;"
       "&#154;"
       "&#155;"
       "&#156;"
       #f; "&#157;"
       "&#158;"
       "&#159;"
       "&#160;"
       "&#161;"
       "&#162;"
       "&#163;"
       "&#164;"
       "&#165;"
       "&#166;"
       "&#167;"
       "&#168;"
       "&#169;"
       "&#170;"
       "&#171;"
       "&#172;"
       "&#173;"
       "&#174;"
       "&#175;"
       "&#176;"
       "&#177;"
       "&#178;"
       "&#179;"
       "&#180;"
       "&#181;"
       "&#182;"
       "&#183;"
       "&#184;"
       "&#185;"
       "&#186;"
       "&#187;"
       "&#188;"
       "&#189;"
       "&#190;"
       "&#191;"
       "&#192;"
       "&#193;"
       "&#194;"
       "&#195;"
       "&#196;"
       "&#197;"
       "&#198;"
       "&#199;"
       "&#200;"
       "&#201;"
       "&#202;"
       "&#203;"
       "&#204;"
       "&#205;"
       "&#206;"
       "&#207;"
       "&#208;"
       "&#209;"
       "&#210;"
       "&#211;"
       "&#212;"
       "&#213;"
       "&#214;"
       "&#215;"
       "&#216;"
       "&#217;"
       "&#218;"
       "&#219;"
       "&#220;"
       "&#221;"
       "&#222;"
       "&#223;"
       "&#224;"
       "&#225;"
       "&#226;"
       "&#227;"
       "&#228;"
       "&#229;"
       "&#230;"
       "&#231;"
       "&#232;"
       "&#233;"
       "&#234;"
       "&#235;"
       "&#236;"
       "&#237;"
       "&#238;"
       "&#239;"
       "&#240;"
       "&#241;"
       "&#242;"
       "&#243;"
       "&#244;"
       "&#245;"
       "&#246;"
       "&#247;"
       "&#248;"
       "&#249;"
       "&#250;"
       "&#251;"
       "&#252;"
       "&#253;"
       "&#254;"
       "&#255;"
       ))
  (let ((port (if (null? possible-port)
		  (current-output-port)
		  (car possible-port)))
	(indentation 0)
	(indentation-whitespace (make-string 100 #\space))
	(old-indentation 0))   ; used with <pre>

    (define (go-to-new-line)
      (newline port)
      (##write-substring indentation-whitespace 0 (min indentation 100) port))

    (define (display-html-form x)
      (if (html-form-start-newline? x)
	  (begin
	    (go-to-new-line)
	    (set! indentation (+ indentation 2))))
      (##write-substring "<" 0 1 port)
      (let ((name (html-form-name x)))
	(##write-substring name 0 (string-length name) port))
      (for-each (lambda (attribute)
		  (##write-substring " " 0 1 port)
		  (let ((name (attribute-name attribute)))
		    (##write-substring name 0 (string-length name) port))
		  (if (attribute-takes-value? attribute)
		      (begin
			(##write-substring "=\"" 0 2 port)
			(protect (attribute-value attribute))
			(##write-substring "\"" 0 1 port))))
		(html-form-attributes x))
      (##write-substring ">" 0 1 port)
      (if (equal? (html-form-name x) "pre")
	  (begin
	    (set! old-indentation indentation)
	    (set! indentation 0)))
      (cond ((html-form-body x)
	     =>
	     protect))
      (if (equal? (html-form-name x) "pre")
	  (set! indentation old-indentation))
      (if (html-form-start-newline? x)
	  (set! indentation (- indentation 2)))
      (if (html-form-end-tag? x)
	  (begin
	    (if (html-form-end-tag-start-newline? x)
		(go-to-new-line))
	    (##write-substring "</" 0 2 port)
	    (let ((name (html-form-name x)))
	      (##write-substring name 0 (string-length name) port))
	    (##write-substring ">" 0 1 port))))
    
    (define (display-protected-string x)
      (let ((n (string-length x)))
	(let loop ((start 0) (end 0))
	  (if (= end n) 
	      (##write-substring x start end port)
	      (let* ((ch (string-ref x end))
		     (index (char->integer ch)))
		(cond ((and (< index 256)
			    (vector-ref character-entity-table index))
		       =>
		       (lambda (character-value)
			 (if (char? character-value)
			     (if (eq? character-value #\newline)
				 (begin
				   (##write-substring x start end port)
				   (go-to-new-line)
				   (loop (+ end 1) (+ end 1)))
				 (loop start (+ end 1)))
			     (begin  ; it's a string
			       (##write-substring x start end port)
			       (##write-substring character-value 0 (string-length character-value) port)
			       (loop (+ end 1) (+ end 1))))))
		      (else
		       (display (string-append "Warning: Character (integer->char "
					       index
					       ") is not a valid HTML 4.0 character entity\n")
				##stderr)
		       (loop start (+ end 1)))))))))

    (define (display-protected-char x)
      (let ((index (char->integer x)))
	(cond ((and (< index 256)
		    (vector-ref character-entity-table index))
	       =>
	       (lambda (character-value)
		 (if (char? character-value)
		     (if (eq? character-value #\newline)
			 (go-to-new-line)
			 (display character-value port))
		     (##write-substring character-value 0 (string-length character-value) port))))
	      (else
	       (display (string-append "Warning: Character (integer->char "
				       index
				       ") is not a valid HTML 4.0 character entity\n")
			##stderr)
	       (display x port)))))

    (define (display-attribute-value x)
      (protect x #f))

    (define (unprotect x)
      (cond ((pair? x) (for-each unprotect x))
	    ((null? x) (void))
	    ((string? x) (##write-substring x 0 (string-length x) port))
	    ((char? x) (write-char x port))
	    ((html-form? x)
	     (error "html-display: There shouldn't be a form here" x))
	    ((html-raw? x)
	     (error "html-display: There shouldn't be a form here" x))
	    (else
	     (display x port))))
    
    (define (protect x #!optional (allow-forms? #t))
      (cond ((pair? x) (for-each protect x))
	    ((html-form? x)
	     (if allow-forms?
		 (display-html-form x)
		 (error "html-display: There shouldn't be a form here" x)))
	    ((null? x) (void))
	    ((string? x) (display-protected-string x))
	    ((char? x) (display-protected-char x))
	    ((html-raw? x)
	     (unprotect (html-raw-value x)))
	    (else
	     (display-protected-string (with-output-to-string '() (lambda () (display x)))))))
     
    (protect x)))

(define (<unprotected> . args)  ; passes through args unprotected; used for html generated by other means.
  (make-html-raw args))

;;; this function parses the args of such a form.

(define (html-parse-args form-name end-tag? attribute-alist single-attribute-alist args)
  (let loop ((args args))
    (cond ((and (not (null? args)) (keyword? (car args)))
	   (let ((key (car args))
		 (args (cdr args)))
	     (cond ((assq key attribute-alist)
		    => (lambda (entry)
			 (let ((attribute (cdr entry)))
			   (cond ((attribute-value attribute) 
				  (error "Keyword used more than once" form-name key))
				 ((null? args) 
				  (error "Keyword must take an argument" form-name key))
				 (else
				  (attribute-value-set! attribute (car args))
				  (loop (cdr args)))))))
		   ((assq key single-attribute-alist)
		    => (lambda (entry)
			 (let ((attribute (cdr entry)))
			   (cond ((attribute-value attribute)
				  (error "Keyword used more than once" form-name key))
				 (else
				  (attribute-value-set! attribute #t)
				  (loop args))))))
		   (else
		    (error "Unrecognized keyword" form-name key)))))
	  ((and (not end-tag?) (not (null? args)))
	   (error "Body found in tag without end-tag" form-name args))
	  (else
	   args)))) ; return


;;; this function builds the form

(define (html-build-form tag-name attribute-alist single-attribute-alist args start-newline? end-tag-start-newline? end-tag?)
  (let ((attributes 
	 (let loop ((alist attribute-alist)
		    (out '()))
	   (if (not (null? alist))
	       (let ((attribute (cdar alist)))
		 (if (attribute-value attribute)
		     (loop (cdr alist)
			   (cons attribute out))
		     (loop (cdr alist)
			   out)))
	       (let loop ((alist single-attribute-alist)
			  (out out))
		 (if (not (null? alist))
		     (let ((attribute (cdar alist)))
		       (if (attribute-value attribute)
			   (loop (cdr alist)
				 (cons attribute
				       out))
			   (loop (cdr alist)
				 out)))
		     out))))))
    (make-html-form tag-name attributes args start-newline? end-tag-start-newline? end-tag?)))

;;; tags are defined with this macro.  It takes a required tag-name as the first argument, and
;;; the following key arguments:
;;; allow-core-attributes?: Takes the generic HTML 4.0 core attributes (default #t)
;;; end-tag?: takes an end tag and a body (default #t)
;;; start-newline?: start a newline with this tag and with its end tag
;;; attributes: the attributes of the tag that take values
;;; single-attributes: the attributes of the tag that don't take a value.

;;; There should also be a required: argument giving required attributes for each tag.

(##define-macro (define-tag . args)
  (let ((internal-define-tag
	 (lambda (tag-name 
		  #!key 
		  (allow-core-attributes? #t)
		  (end-tag? #t)
		  (start-newline? #f)
		  (end-tag-start-newline? #f)
		  (attributes '())
		  (single-attributes '()))
	   (let ((core-attributes '(class 
				    dir 
				    id
				    lang
				    onclick
				    ondblclick
				    onkeydown 
				    onkeypress
				    onkeyup
				    onmousedown
				    onmousemove
				    onmouseout
				    onmouseover
				    onmouseup
				    style
				    title)))
	     ;; the maps here are done at compile time, so they are the system map.

	     (let* ((attributes (if allow-core-attributes? (append attributes core-attributes) attributes))
		    (attribute-strings (map (lambda (x) (symbol->string x)) attributes))
		    (attribute-keywords (map (lambda (x) (string->keyword x)) attribute-strings))
		    (attribute-alist (cons 'list (map (lambda (keyword name)
							`(cons ,keyword (make-attribute ,name #t #f)))
						      attribute-keywords
						      attribute-strings)))
		    (single-attribute-strings (map (lambda (x) (symbol->string x)) single-attributes))
		    (single-attribute-keywords (map (lambda (x) (string->keyword x)) single-attribute-strings))
		    (single-attribute-alist (cons 'list (map (lambda (keyword name)
							       `(cons ,keyword (make-attribute ,name #f #f)))
							     single-attribute-keywords
							     single-attribute-strings)))
		    (form-name (string->symbol (string-append "<" (symbol->string tag-name) ">"))))
	       `(define (,form-name . args)
		  (let ((attribute-alist
			 ,(if (null? attribute-keywords)
			      ''()
			      attribute-alist))
			(single-attribute-alist
			 ,(if (null? single-attribute-keywords)
			      ''()
			      single-attribute-alist)))
		    (let ((args (html-parse-args ,(list 'quote form-name) ,end-tag? attribute-alist single-attribute-alist args)))
		      (html-build-form ,(symbol->string tag-name)
				       attribute-alist
				       single-attribute-alist
				       args
				       ,start-newline?
				       ,end-tag-start-newline?
				       ,end-tag?)))))))))
    
    (apply internal-define-tag args)))

(define-tag a attributes: (accesskey charset coords href hreflang name rel rev shape tabindex target type))

(define-tag abbr)

(define-tag acronym)

(define-tag address)

(define-tag applet allow-core-attributes?: #f attributes: (align alt archive class code codebase height hspace id name object style title vspace width))

(define-tag area end-tag?: #f attributes: (accesskey alt coords href onblur onfocus shape tabindex taborder target) single-attributes: (nohref))

(define-tag b)

(define-tag base allow-core-attributes?: #f end-tag?: #f attributes: (href target))

(define-tag basefont allow-core-attributes?: #f end-tag?: #f attributes: (color face id size))

(define-tag bdo allow-core-attributes?: #f attributes: (class dir id lang style title))

(define-tag big)

(define-tag blockquote start-newline?: #t end-tag-start-newline?: #t attributes: (cite))

(define-tag body start-newline?: #t end-tag-start-newline?: #t attributes: (alink background bgcolor link onload onunload text vlink))

(define-tag br start-newline?: #t allow-core-attributes?: #f end-tag?: #f attributes: (class clear id style title))

(define-tag button start-newline?: #t end-tag?: #f attributes: (accesskey name onblur onfocus type tabindex value) single-attributes: (disabled))

(define-tag caption start-newline?: #t attributes: (align))

(define-tag center start-newline?: #t)

(define-tag cite start-newline?: #t)

(define-tag code)

(define-tag col end-tag?: #f start-newline?: #t attributes: (align char charoff span valign width))

(define-tag colgroup end-tag?: #f start-newline?: #t attributes: (align char charoff span valign width))

(define-tag dd start-newline?: #t)

(define-tag del attributes: (cite datetime))

(define-tag dfn)

(define-tag dir single-attributes: (compact))

(define-tag div attributes: (align))

(define-tag dl start-newline?: #t single-attributes: (compact))

(define-tag dt start-newline?: #t)

(define-tag em)

(define-tag field-set)

(define-tag font start-newline?: #t allow-core-attributes?: #f attributes: (class color dir face id lang size style title))

(define-tag form start-newline?: #t end-tag-start-newline?: #t attributes: (accept-charset action enctype method onreset onsubmit target))

(define-tag frame allow-core-attributes?: #f start-newline?: #t attributes: (class frameborder id longdesc marginheight marginwidth name scrolling src style title) single-attributes: (noresize))

(define-tag frameset start-newline?: #t attributes: (cols onload onunload rows))

(define-tag h1 start-newline?: #t attributes: (align))

(define-tag h2 start-newline?: #t attributes: (align))

(define-tag h3 start-newline?: #t attributes: (align))

(define-tag h4 start-newline?: #t attributes: (align))

(define-tag h5 start-newline?: #t attributes: (align))

(define-tag h6 start-newline?: #t attributes: (align))

(define-tag head allow-core-attributes?: #f start-newline?: #t end-tag-start-newline?: #t attributes: (dir lang profile))

(define-tag hr allow-core-attributes?: #f end-tag?: #f start-newline?: #t attributes: (align class id onclick ondblclick onkeydown onkeypress onkeyup onmousedown onmousemove onmouseout onmouseover onmouseup size style title width) single-attributes: (noshade))

(define-tag html allow-core-attributes?: #f start-newline?: #t end-tag-start-newline?: #t attributes: (dir lang version))

(define-tag i)

(define-tag iframe allow-core-attributes?: #f attributes: (align class frameborder height id longdesc marginheight marginwidth name scrolling src style title width))

(define-tag img end-tag?: #f attributes: (align alt border height hspace longdesc src usemap vspace width) single-attributes: (ismap))

;; we'll define input by hand at end

(define-tag ins attributes: (cite datetime))

(define-tag isindex end-tag?: #f allow-core-attributes?: #f attributes: (class dir id lang prompt style title))

(define-tag kbd)

(define-tag label attributes: (accesskey for onblur onfocus))

(define-tag legend attributes: (accesskey align))

(define-tag li start-newline?: #t attributes: (type value))

(define-tag link start-newline?: #t end-tag?: #f attributes: (charset href hreflang media rel rev type))

(define-tag map attributes: (name))

(define-tag menu single-attributes: (compact))

(define-tag meta end-tag?: #f allow-core-attributes?: #f attributes: (charset content dir http-equiv lang name scheme))

(define-tag noframes)

(define-tag noscript)

(define-tag object 
  end-tag?: #f  start-newline?: #t
  attributes: (align archive border classid codebase codetype data height hspace name standby tabindex type usemap vspace width)
  single-attributes: (declare))

(define-tag ol start-newline?: #t end-tag-start-newline?: #f attributes: (start type) single-attributes: (compact))

(define-tag optgroup attributes: (label) single-attributes: (disabled))

(define-tag option start-newline?: #t attributes: (label value) single-attributes: (disabled selected))

(define-tag p start-newline?: #t attributes: (align))

(define-tag param allow-core-attributes?: #f attributes: (id name type value valuetype))

(define-tag plaintext start-newline?: #t end-tag?: #f)

(define-tag pre start-newline?: #t attributes: (width))

(define-tag q attributes: (cite))

(define-tag s)

(define-tag samp)

(define-tag script start-newline?: #t allow-core-attributes?: #f attributes: (charset language src type) single-attributes: (defer))

(define-tag select start-newline?: #t attributes: (name onblur onchange onfocus size tabindex) single-attributes: (disabled multiple))

(define-tag small)

(define-tag span)

(define-tag strike)

(define-tag strong)

(define-tag style allow-core-attributes?: #f attributes: (dir lang media title type))

(define-tag sub)

(define-tag sup)

(define-tag table start-newline?: #t end-tag-start-newline?: #t attributes: (align bgcolor border cellpadding cellspacing frame rules summary width))

(define-tag td start-newline?: #t attributes: (abbr align axis bgcolor char charoff colspan headers height rowspan scope valign width) single-attributes: (nowrap))

(define-tag textarea attributes: (accesskey cols name onblur onchange onfocus onselect rows tabindex) single-attributes: (disabled readonly))

(define-tag tfoot start-newline?: #t attributes: (align char charoff valign))

(define-tag th attributes: (abbr align axis bgcolor char charoff colspan headers height rowspan scope valign width) single-attributes: (nowrap))

(define-tag thead start-newline?: #t attributes: (align char charoff valign))

(define-tag title start-newline?: #t allow-core-attributes?: #f attributes: (dir lang))

(define-tag tr start-newline?: #t attributes: (align char charolff valign))

(define-tag tt)

(define-tag u)

(define-tag ul start-newline?: #t end-tag-start-newline?: #f attributes: (type) single-attributes: (compact))

(define-tag var)

;;; because the attributes that an input tag accepts depends on its type attribute,
;;; the input tag does not follow the form given above, so we define it by hand here.

(##define-macro (define-input-tag)
  (let ((button-attributes '(type: 
			     accesskey:
			     name:
			     onblur:
			     onfocus:
			     tabindex:
			     value:
			     class: 
			     dir: 
			     id:
			     lang:
			     onclick:
			     ondblclick:
			     onkeydown: 
			     onkeypress:
			     onkeyup:
			     onmousedown:
			     onmousemove:
			     onmouseout:
			     onmouseover:
			     onmouseup:
			     style:
			     title:))
	(button-single-attributes '(disabled:))
	(checkbox-attributes '(type:
			       accesskey:
			       name:
			       tabindex:
			       value:
			       class: 
			       dir: 
			       id:
			       lang:
			       onclick:
			       ondblclick:
			       onkeydown: 
			       onkeypress:
			       onkeyup:
			       onmousedown:
			       onmousemove:
			       onmouseout:
			       onmouseover:
			       onmouseup:
			       style:
			       title:))
	(checkbox-single-attributes '(checked: disabled: readonly:))
	(file-attributes '(type:
			   accesskey:
			   maxlength:
			   name:
			   onblur:
			   onchange:
			   onfocus:
			   size:
			   tabindex:
			   value:
			   class: 
			   dir: 
			   id:
			   lang:
			   onclick:
			   ondblclick:
			   onkeydown: 
			   onkeypress:
			   onkeyup:
			   onmousedown:
			   onmousemove:
			   onmouseout:
			   onmouseover:
			   onmouseup:
			   style:
			   title:))
	(file-single-attributes '(disabled: readonly:))
	(hidden-attributes '(type:
			     name:
			     value:
			     class: 
			     dir: 
			     id:
			     lang:
			     onclick:
			     ondblclick:
			     onkeydown: 
			     onkeypress:
			     onkeyup:
			     onmousedown:
			     onmousemove:
			     onmouseout:
			     onmouseover:
			     onmouseup:
			     style:
			     title:))
	(hidden-single-attributes '())
	(image-attributes '(type:
			    accesskey:
			    align:
			    alt:
			    border:
			    name:
			    src:
			    tabindex:
			    usemap:
			    class: 
			    dir: 
			    id:
			    lang:
			    onclick:
			    ondblclick:
			    onkeydown: 
			    onkeypress:
			    onkeyup:
			    onmousedown:
			    onmousemove:
			    onmouseout:
			    onmouseover:
			    onmouseup:
			    style:
			    title:))
	(image-single-attributes '(disabled:))
	(password-attributes '(type:
			       acccesskey:
			       maxlength:
			       name:
			       onblur:
			       onchange:
			       onfocus:
			       onselect:
			       size:
			       tabindex:
			       value:
			       class: 
			       dir: 
			       id:
			       lang:
			       onclick:
			       ondblclick:
			       onkeydown: 
			       onkeypress:
			       onkeyup:
			       onmousedown:
			       onmousemove:
			       onmouseout:
			       onmouseover:
			       onmouseup:
			       style:
			       title:))
	(password-single-attributes '(disabled: readonly:))
	(radio-attributes '(type:
			    accesskey:
			    name:
			    tabindex:
			    value:
			    class: 
			    dir: 
			    id:
			    lang:
			    onclick:
			    ondblclick:
			    onkeydown: 
			    onkeypress:
			    onkeyup:
			    onmousedown:
			    onmousemove:
			    onmouseout:
			    onmouseover:
			    onmouseup:
			    style:
			    title:))
	(radio-single-attributes '(checked: disabled: readonly:))
	(reset-attributes '(type:
			    accesskey:
			    tabindex:
			    value:
			    class: 
			    dir: 
			    id:
			    lang:
			    onclick:
			    ondblclick:
			    onkeydown: 
			    onkeypress:
			    onkeyup:
			    onmousedown:
			    onmousemove:
			    onmouseout:
			    onmouseover:
			    onmouseup:
			    style:
			    title:))
	(reset-single-attributes '(disabled:))
	(submit-attributes '(type:
			     accesskey:
			     name:
			     tabindex:
			     value:
			     class: 
			     dir: 
			     id:
			     lang:
			     onclick:
			     ondblclick:
			     onkeydown: 
			     onkeypress:
			     onkeyup:
			     onmousedown:
			     onmousemove:
			     onmouseout:
			     onmouseover:
			     onmouseup:
			     style:
			     title:))
	(submit-single-attributes '(disabled:))
	(text-attributes '(type:
			   accesskey:
			   maxlength:
			   name:
			   onblur:
			   onchange:
			   onfocus:
			   onselect:
			   size:
			   tabindex:
			   value:
			   class: 
			   dir: 
			   id:
			   lang:
			   onclick:
			   ondblclick:
			   onkeydown: 
			   onkeypress:
			   onkeyup:
			   onmousedown:
			   onmousemove:
			   onmouseout:
			   onmouseover:
			   onmouseup:
			   style:
			   title:))
	(text-single-attributes '(disabled: readonly:)))
    `(define (<input> . args)
       (let ((type-tag (memq type: args)))
	 (if (or (not type-tag)
		 (null? (cdr type-tag))
		 (not (symbol? (cadr type-tag))))
	     (error "The input type tag must be present and its value must be a symbol" args))
	 (let* ((input-type (cadr type-tag))
		(attribute-alist (case input-type
				   ,@(map (lambda (input-type input-attributes)
					    `((,input-type) ,(if (null? input-attributes)
								 ''()
								 (cons 'list (map (lambda (x) `(cons ,x (make-attribute ,(keyword->string x) #t #f))) input-attributes)))))
					  '(button checkbox file hidden image password radio submit text)
					  (list button-attributes checkbox-attributes file-attributes hidden-attributes image-attributes password-attributes radio-attributes submit-attributes text-attributes))
				   (else (error "The input type tag is not valid" args))))
		(single-attribute-alist (case input-type
					  ,@(map (lambda (input-type input-single-attributes)
						   `((,input-type) ,(if (null? input-single-attributes)
									''()
									(cons 'list (map (lambda (x) `(cons ,x (make-attribute ,(keyword->string x) #f #f))) input-single-attributes)))))
						 '(button checkbox file hidden image password radio submit text)
						 (list button-single-attributes checkbox-single-attributes file-single-attributes hidden-single-attributes image-single-attributes password-single-attributes radio-single-attributes submit-single-attributes text-single-attributes))
					  (else (error "The input type tag is not valid" args)))))
	   (let ((args (html-parse-args <input> #t attribute-alist single-attribute-alist args)))
	     (html-build-form "input"
			      attribute-alist
			      single-attribute-alist
			      args
			      #t
			      #f
			      #f)))))))

(define-input-tag)

(declare (generic) (safe))
