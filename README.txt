cage


Table of Contents
─────────────────

1. Prerequisites:
2. Installation:
3. Usage
.. 1. Extracting translatable strings:
.. 2. Locating GNU message catalog files
.. 3. Simple example
4. String utils
5. Old version incompatibility
6. Bugs and issues
.. 1. Known issues
7. License
8. NO WARRANTY
9. Contributors:


[http://quickdocs.org/badge/cl-i18n.svg]


[http://quickdocs.org/badge/cl-i18n.svg] <http://quickdocs.org/cl-i18n/>


1 Prerequisites:
════════════════

  • ASDF
  • uiop (if your ASDF version is < 3)
  • cl-ppcre
  • alexandria
  • babel


2 Installation:
═══════════════

  The best way to get cl-i18n working is using the excellent [quicklisp]

  ┌────
  │ (ql:quickload "cl-i18n")
  └────


[quicklisp] <http://www.quicklisp.org/>


3 Usage
═══════

  This library can load translations from file formats of the following
  types:
  • [GNU gettext] text file (AKA PO files);
  • [GNU gettext] binary file (AKA MO files);
  • [Universal Terminology eXchange] format
  • its native file formats (essentially a Common lisp S-exp).

  Check the `examples/' directory for an usage example.


[GNU gettext] <https://www.gnu.org/software/gettext/>

[Universal Terminology eXchange]
<https://web.archive.org/web/20190407131733/https://www.aamt.info/english/utx/index.htm>

3.1 Extracting translatable strings:
────────────────────────────────────

  ┌────
  │ ;; define two convenience functions
  │
  │ (defun _ (a) (cl-i18n:translate a))
  │
  │ (defun n_ (s f n) (cl-i18n:ntranslate s f n))
  │
  │ (cl-i18n-utils:gen-translation-file "/src/" "klingon.lisp"
  │                                     :ext "lisp$"
  │                                     :prefix-re "\\(_\\s+")
  │
  │ (cl-i18n-utils:gen-translation-file "/src/" "klingon.lisp"
  │                                     :ext "lisp$"
  │                                     :prefix-re "\\(n_\\s+")
  │
  └────

  Will extract all `(_…'  strings from all the files in directory `/src'
  which name ends in "lisp" and set up a basic translation resource in
  klingon.lisp (*Note: the output file if exists will be overwritten*).

  To make it work with CL-WHO, use the “str” directive, as in

  ┌────
  │ (with-html-output *out*
  │   (:p (str (_ "Peace and love!"))))
  └────

  Please note that the library can accept gettext MO or PO files so
  nothing prevents you using GNU xgettext to extracts strings that needs
  to be translated and use its output as translation file.


3.2 Locating GNU message catalog files
──────────────────────────────────────

  According to [GNU gettext manual] the path to the catalog is system
  dipendent, the function /search-mo-repository/ will try to figure out
  where that catalog *could* be; but its implementation is slow, memory
  consuming and sometimes fails (i.e. crash).


[GNU gettext manual]
<https://www.gnu.org/software/gettext/manual/gettext.html#Locating-Catalogs>


3.3 Simple example
──────────────────

  Here is a way similar to GNU gettext style API; please note we are
  assuming `/usr/share/locale/' as the path where the catalog can be
  found and foo.mo is there.

  We also let the library guess the right locale with `find-locale'.

  ┌────
  │ (let ((*translation-file-root* "/usr/share/locale/"))
  │   (load-language "foo" :locale (find-locale))
  │   (format t "~a~%" (_ "Browse"))
  │   (format t "~a~%" (_ "Save as...")))
  └────


4 String utils
══════════════

  During the development of this library I have implemented a couple of
  string functions that I think could be useful beyond i18n.

  cl-i18n-utils:levenshtein-distance (string1 string2)
        Compute the levenshtein distance (i. e. how much are similars)
        between two strings

  cl-i18n-utils:fuzzy-match (…)
        Performs a Smith-Waterman affinity search.  See:
        <https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm>


5 Old version incompatibility
═════════════════════════════

  This version deeply changed the file format of the translation table,
  however you can convert from the old format to the new one with:

  ┌────
  │ (cl-i18n-utils:convert-save-dictionary oldfile-path new-filepath)
  └────

  *Note: the output file if exists will be overwritten*


6 Bugs and issues
═════════════════

  Please file bug report on the [issue tracker].


[issue tracker] <https://notabug.org/cage/cl-i18n/issues>

6.1 Known issues
────────────────

  • Documentation is missing;
  • Source code is mostly undocumented;
  • PO file parser is slow.


7 License
═════════

  This library is released under Lisp Lesser General Public license (see
  COPYING.LESSER file)

  Examples are released under GPL version 3 or later

  `doc/internals/pofiles_grammar' is © 2012 cage and is licensed under
  Creative Commons Attribution-ShareAlike 3.0 Unported

  File /function-name.lisp/ was got from [cl-store] © 2004 Sean Ross and
  included here with the original license stated below.

  Copyright (c) 2004 Sean Ross All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:
  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the
     distribution.
  3. The names of the authors and contributors may not be used to
     endorse or promote products derived from this software without
     specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHORS AND CONTRIBUTORS ``AS IS''
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS
  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
  BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
  OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
  IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


[cl-store] <http://common-lisp.net/project/cl-store/>


8 NO WARRANTY
═════════════

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.


9 Contributors:
═══════════════

  • Leslie P. Polzer (base)
  • Vilson Vieira (string extractor)
  • Cage (developer and maintainer)
