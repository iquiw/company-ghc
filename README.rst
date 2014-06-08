=============
 Company GHC
=============
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
1. Install from Git::

     git clone https://github.com/iquiw/company-ghc.git

2. Add ``company-ghc`` to ``company-backends`` after loading `company-mode`_ and `ghc-mod`_::

     (add-to-list 'load-path "/path/to/company-ghc")
     (require 'company-ghc)
     (add-to-list 'company-backends 'company-ghc)


Feature
=======
The following completions are available.

1. Pragma names. (``ghc-pragma-names``)

   .. image:: images/pragma.png
      :width: 350px
      :alt: Completion for pragma

2. Language extensions. (``ghc-language-extensions``)

   .. image:: images/language.png
      :width: 350px
      :alt: Completion for language extensions

3. GHC option flags. (``ghc-options-flags``)

   .. image:: images/option.png
      :width: 350px
      :alt: Completion for GHC options

4. Import module names. (``ghc-modules-names``)

   .. image:: images/module.png
      :width: 350px
      :alt: Completion for import modules

5. Variables and functions in import spec. (``ghc-module-keyword``)

   .. image:: images/impspec.png
      :width: 350px
      :alt: Completion for import specs

6. Keywords. (``ghc-merged-keywords``)

   .. image:: images/keyword.png
      :width: 350px
      :alt: Completion for keywords

TODO
====
* Show type in minibuffer.
* Context sensitive completion for qualified imported keywords.

License
=======
Licensed under the GPL 3+ license.

.. _company-mode: http://company-mode.github.io/
.. _haskell-mode: https://github.com/haskell/haskell-mode
.. _ghc-mod: http://www.mew.org/~kazu/proj/ghc-mod/en/
