=============
 Company GHC
=============

.. contents:: Table of Contents
.. sectnum::

Overview
========

| `Company-mode`_ completion back-end for `haskell-mode`_ via `ghc-mod`_.
| It runs when the major mode is derived from `haskell-mode`_.

Installation
============

Depends
-------
* cl-lib
* `company-mode`_
* `ghc-mod`_

Setup
-----
1. Install from `MELPA`_::

     M-x package-install RET company-ghc RET

   or install from Git::

     git clone https://github.com/iquiw/company-ghc.git

2. Add ``company-ghc`` to ``company-backends`` after loading `company-mode`_ and `ghc-mod`_

   .. code:: emacs-lisp

     (add-to-list 'load-path "/path/to/company-ghc")
     (require 'company-ghc)
     (add-to-list 'company-backends 'company-ghc)


Feature
=======

Completion
----------
The following completions are available.

1. Pragma names. (``ghc-pragma-names``)

   .. image:: images/pragma.png
      :alt: Completion for pragma

2. Language extensions. (``ghc-language-extensions``)

   .. image:: images/language.png
      :alt: Completion for language extensions

3. GHC option flags. (``ghc-options-flags``)

   .. image:: images/option.png
      :alt: Completion for GHC options

4. Import module names. (``ghc-modules-names``)

   .. image:: images/module.png
      :alt: Completion for import modules

5. Variables and functions in import spec. (``ghc-module-keyword``)

   .. image:: images/impspec.png
      :alt: Completion for import specs

6. Keywords from imported modules.

   .. image:: images/keyword.png
      :alt: Completion for keywords of imported modules

Show type info in minibuffer
----------------------------
* If ``company-ghc-show-info`` is ``t``, ``oneline`` or ``nomodule``,
  then type info of completion candidate is displayed in minibuffer
  by ``ghc-mod info``.

  .. image:: images/showinfo.png
     :alt: Show info in minibuffer (``nomodule``)

Show module name as annotation
------------------------------
* Module name is displayed as completion annotation
  if ``company-ghc-show-module`` is non-nil (default) as in the above images.

Note
====
Currently, company-ghc treats all symbols as completion prefix.
This means other back-ends after company-ghc have no chance to provide completion candidates in haskell-mode.

As of now, if you want to use other back-ends with company-ghc, use grouped back-end like below.

.. code:: emacs-lisp

   (add-to-list 'company-backends '(company-ghc :with company-dabbrev))

TODO
====
* Context sensitive completion for qualified imported keywords.
* Support doc-buffer using `haskell-docs`_
* Write unit test.

License
=======
Licensed under the GPL 3+ license.

.. _company-mode: http://company-mode.github.io/
.. _haskell-mode: https://github.com/haskell/haskell-mode
.. _ghc-mod: http://www.mew.org/~kazu/proj/ghc-mod/en/
.. _haskell-docs: https://github.com/chrisdone/haskell-docs
.. _MELPA: http://melpa.milkbox.net/
