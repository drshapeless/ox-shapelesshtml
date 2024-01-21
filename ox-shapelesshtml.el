;;; ox-shapelesshtml.el --- shapeless HTML export backend.
;;; Commentary:

;; This was a html export backend derived from ox-slimhtml.
;;
;; Since the original ox-slimhtml was abandoned, I rewrite the whole
;; package and strip off a lot of the functions that I don't
;; need. Such as html attributes of every org elements.
;;
;; ox-shapelesshtml is now a much slimer export backend.
;;
;; It adds code, and fixes unwanted newline character in Chinese
;; paragraph.

;;; Code:

(require 'ox-html)
(require 'cl-lib)

;;; formatting
;; *bold*               <strong>bold</strong>
;; /italic/             <em>italic</em>
;; =verbatim=           <kbd>verbatim</kbd>
;; ~code~               <code>code</code>

(defun ox-shapelesshtml-bold (bold contents info)
  "Transcode BOLD from Org to HTML.

CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (when contents (format "<strong>%s</strong>" contents)))

(defun ox-shapelesshtml-italic (italic contents info)
  "Transcode ITALIC from Org to HTML.

CONTENTS is the text with italic markup.
INFO is a plist holding contextual information."
  (when contents (format "<em>%s</em>" contents)))

(defun ox-shapelesshtml-verbatim (verbatim contents info)
  "Transcode VERBATIM string from Org to HTML.

CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value verbatim))))
    (when contents (format "<kbd>%s</kbd>" contents))))

(defun ox-shapelesshtml-code (code contents info)
  "Transcode CODE string from Org to HTML.
CONTENTS is nil.
INFO is a plist holding contextual information."
  (let ((contents (org-html-encode-plain-text
                   (org-element-property :value code))))
    (when contents (format "<code>%s</code>" contents))))

;;; headlines
;; * headline text      <h1>headline</h1>
;; ** headline text     <h2>headline</h2>
(defun ox-shapelesshtml-headline (headline contents info)
  "Transcode HEADLINE from Org to HTML.

CONTENTS is the section as defined under the HEADLINE.
INFO is a plist holding contextual information."
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info)))
    (format "<h%d>%s</h%d>%s" level text level (or contents ""))))

;;; sections
(defun ox-shapelesshtml-section (section contents info)
  "Transcode a SECTION element from Org to HTML.

CONTENTS is the contents of the section.
INFO is a plist holding contextual information.

Sections are child elements of org headlines;
'container' settings are found in slim-headlines."
  contents)

;;; links
;; [[link][content]]    <a href="link">content</a>
(defun ox-shapelesshtml-link (link contents info)
  "Transcode LINK from Org to HTML.

CONTENTS is the text of the link.
INFO is a plist holding contextual information."
  (let ((path (or (org-element-property :path link) "")))
    (format "<a href=\"%s\">%s</a>" path contents)))

;;; plain lists
;; - item 1             <ul>
;; - item 2             <li>item 1</li>
;;                      <li>item 2</li>
;;                      </ul>
;;
;; It also supports ordered list.
(defun ox-shapelesshtml-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST string from Org to HTML.

CONTENTS is the contents of the list element.
INFO is a plist holding contextual information."
  (let ((type (cl-case (org-element-property :type plain-list)
                (ordered "ol")
                (unordered "ul")
                (descriptive "dl"))))
    (format "<%s>%s</%s>"
            type
            (ox-shapelesshtml--remove-newline contents)
            type)))

;;; paragraphs
;; content              <p>content</p>
(defun ox-shapelesshtml-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to HTML.

CONTENTS is the contents of the paragraph.
INFO is a plist holding contextual information."
  (when contents
    (format "<p>%s</p>" (ox-shapelesshtml--remove-newline contents))))

;;; examples
;; #+BEGIN_EXAMPLE
;; content
;; #+END_EXAMPLE
;;
;; example is a lot less common to use in an html export.
(defun ox-shapelesshtml-example-block (example-block contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to HTML.

CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((code (org-html-format-code example-block info)))
    (when code
      (format "<pre><code class=\"%s\">%s</code></pre>"
              (or (org-element-property :language example-block) "example")
              code))))

;;; raw html
;; #+BEGIN_EXPORT html                        <span>export block</span>
;;   <span>export block</span>
;; #+END_EXPORT

;; #+BEGIN_EXPORT javascript                  <script>console.log()</script>
;;   console.log()
;; #+END_EXPORT

;; #+BEGIN_EXPORT css                         <style type="text/css">span{}</style>
;;   span {}
;; #+END_EXPORT
(defun ox-shapelesshtml-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element from Org to HTML.

CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((contents (org-element-property :value export-block))
        (language (org-element-property :type export-block)))
    (when contents
      (cond ((string= "JAVASCRIPT" language)
             (format "<script>%s</script>" contents))
            ((string= "CSS" language)
             (format "<style type=\"text/css\">%s</style>" contents))
            (t
             (org-remove-indentation contents))))))

;;; snippet
;;   @@html:<span>snippet</span>@@              <span>snippet</span>
(defun ox-shapelesshtml-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET object from Org to HTML.

CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((contents (org-element-property :value export-snippet)))
    (when contents contents)))

;;; source code
;; #+BEGIN_SRC c
;;   code
;; #+END_SRC
(defun ox-shapelesshtml-src-block (src-block contents info)
  "Transcode SRC-BLOCK from Org to HTML.

CONTENTS is the text of a #+BEGIN_SRC...#+END_SRC block.
INFO is a plist holding contextual information."
  (let ((code (org-html-format-code src-block info))
        (language (org-element-property :language src-block)))
    (when code
      (format "<pre><code class=\"%s\"%s>%s</code></pre>"
              language (ox-shapelesshtml--attr src-block) code))))

(defun ox-shapelesshtml-inner-template (contents info)
  "Return body of document string after HTML conversion.

CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (when (and contents (not (string= "" contents)))
    (let ((container (plist-get info :html-container)))
      (concat
       (when (and container (not (string= "" container))) (format "<%s>" container))

       contents

       (when (and container (not (string= "" container)))
         (format "</%s>" (cl-subseq container 0 (cl-search " " container))))))))

;; quote-block
;; #+begin_quote
;; #+end_quote          <blockquote>quote</blockquote>
(defun ox-shapelesshtml-quote-block (quote-block contents info)
  "Transcode a QUOTE-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (when contents
    (format "<blockquote>\n%s</blockquote>" contents)))

;; timestamp
;; <2021-10-10>         2021-10-10
;; [2021-10-10]         2021-10-10
(defun ox-shapelesshtml-timestamp (timestamp contents info)
  "Transcode a TIMESTAMP object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((value (org-html-plain-text (org-timestamp-translate timestamp) info)))
    (format "<span class=\"timestamp\">%s</span>"
            (replace-regexp-in-string "[]\[&lgt;]" "" value))))

;;; html page
;;                          <!DOCTYPE html>
;;                          <html lang="en">
;;                          <head>
;; #+title: document title  <title>document title</title>
;;                          </head>
;;                          <body>
;;                          content
;;                          </body>
;;                          </html>

(defun ox-shapelesshtml-template (contents info)
  "Return full document string after HTML conversion.

CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (let ((doctype (assoc (plist-get info :html-doctype) org-html-doctype-alist))
        (language (plist-get info :language))
        (title (plist-get info :title))
        (newline "\n"))
    (when (listp title)
      (setq title (car title)))
    (concat
     (when doctype (concat (cdr doctype) newline))
     "<html" (when language (concat " lang=\"" language "\"")) ">" newline
     "<head>" newline
     (when (and title (not (string= "" title)))
       (concat "<title>" title "</title>" newline))
     "</head>" newline
     "<body>" newline
     contents
     "</body>" newline
     "</html>")))

;;; plain text
(defun ox-shapelesshtml-plain-text (plain-text info)
  "Transcode a PLAIN-TEXT string from Org to HTML.

PLAIN-TEXT is the string to transcode.
INFO is a plist holding contextual information."
  (org-html-encode-plain-text plain-text))

;;; attributes
(defun ox-shapelesshtml--attr (element &optional property)
  "Return ELEMENT's html attribute properties as a string.

When optional argument PROPERTY is non-nil, return the value of
that property within attributes."
  (let ((attributes (org-export-read-attribute :attr_html element property)))
    (if attributes (concat " " (org-html--make-attribute-string attributes)) "")))

(defun ox-shapelesshtml--remove-newline (string)
  "Remove unwanted newline char in STRING.

In Chinese, we don't want an extra space after a normal
character."
  (string-join (mapcar
                (lambda (str)
                  (if (string-match-p "[0-9a-z,.?!<>\"']\\'" str)
                      (concat str "\n")
                    str))
                (split-string string "\n"))))

;;;###autoload
(defun ox-shapelesshtml-publish-to-html (plist filename pub-dir)
  "Publish an org file to html.

PLIST is the property list for the given project.  FILENAME
is the filename of the Org file to be published.  PUB-DIR is
the publishing directory.

Return output file name."
  (let ((html-extension (or (plist-get plist :html-extension) org-html-extension)))
    (org-publish-org-to 'slimhtml
                        filename
                        (if (and html-extension (not (string= "" html-extension)))
                            (concat "." html-extension) "")
                        plist
                        pub-dir)))

;;; org-export backend definition
(org-export-define-backend
    'shapelesshtml
  '((bold . ox-shapelesshtml-bold)
    (code . ox-shapelesshtml-code)
    (example-block . ox-shapelesshtml-example-block)
    (export-block . ox-shapelesshtml-export-block)
    (export-snippet . ox-shapelesshtml-export-snippet)
    (headline . ox-shapelesshtml-headline)
    (inner-template . ox-shapelesshtml-inner-template)
    (italic . ox-shapelesshtml-italic)
    (item . org-html-item)
    (link . ox-shapelesshtml-link)
    (paragraph . ox-shapelesshtml-paragraph)
    (plain-list . ox-shapelesshtml-plain-list)
    (plain-text . ox-shapelesshtml-plain-text)
    (section . ox-shapelesshtml-section)
    (src-block . ox-shapelesshtml-src-block)
    (template . ox-shapelesshtml-template)
    (timestamp . ox-shapelesshtml-timestamp)
    (verbatim . ox-shapelesshtml-verbatim)
    (quote-block . ox-shapelesshtml-quote-block))

  :menu-entry
  '(?s "Export to shapelesshtml"
       ((?H "As shapelesshtml buffer" ox-shapelesshtml-export-as-html)
        (?h "As shapelesshtml file" ox-shapelesshtml-export-to-html)))
  )

;;;###autoload
(defun ox-shapelesshtml-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a SHAPELESSHTML buffer.

Export as `org-html-export-as-html' does, with shapelesshtml
org-export-backend.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org SHAPELESSHTML export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'shapelesshtml "*Org SHAPELESSHTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun ox-shapelesshtml-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML file.

Export as `org-html-export-as-html' does, with shapelesshtml
org-export-backend.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :html-extension)
                                    org-html-extension
                                    "html")))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'shapelesshtml file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun ox-shapelesshtml-publish-to-html (plist filename pub-dir)
  "Publish an org file to html.

PLIST is the property list for the given project.  FILENAME
is the filename of the Org file to be published.  PUB-DIR is
the publishing directory.

Return output file name."
  (let ((html-extension (or (plist-get plist :html-extension) org-html-extension)))
    (org-publish-org-to 'shapelesshtml
                        filename
                        (if (and html-extension (not (string= "" html-extension)))
                            (concat "." html-extension) "")
                        plist
                        pub-dir)))

(provide 'ox-shapelesshtml)
;;; ox-shapelesshtml.el ends here
