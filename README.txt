.. comment:  -*- mode:rst; coding:utf-8 -*-

Patchwork
#########

This directory contains the sources of the Patchwork application,
ported from MacOS (MCL) to MacOSX (ccl).


License
=======

See the file ``Patchwork_license.rtf`` or ``Patchwork_license.txt``.

Compiling Patchwork
===================

Installing the sources of Patchwork
-----------------------------------

0- Install Clozure CL

1- Install quicklisp. cf. _`http://beta.quicklisp.org/`.

2- Obtain the sources and dependencies

   cd "${SRC}"
   git clone git@gitorious.org:patchwork/scripts.git
   scripts/get-sources

3- Compile the dependencies and Patchwork

   cd "${SRC}"
   scripts/compile-all


Sources and dependencies
----------------------------------------

The sources of patchwork and dependencies are in the following git
repositories (cf `scripts/get-sources`): ::

    git clone git@gitorious.org:patchwork/scripts.git

    git clone git@gitorious.org:patchwork/midishare.git
    git clone git@gitorious.org:patchwork/mclgui.git
    git clone git@gitorious.org:patchwork/patchwork.git

    svn co http://svn.clozure.com/publicsvn/ffigen4/branches/ffigen-apple-gcc-5646/ffigen4
    svn co https://subversion.assembla.com/svn/portaudio/portaudio/trunk portaudio


Informatimago Library
----------------------------------------

The informatimago library can be obtained from quicklisp, but
Patchwork may need the latest version of informatimago, before it's
integrated into the monthly release of quicklisp.

You can get an up-to-date version of informatimago from git: ::

        cd ~/quicklisp/local-projects
        git clone https://git.gitorious.org/com-informatimago/com-informatimago.git


Note: with ccl-1.6 on MacOSX 10.5.8, symbolic links in
``local-projects`` are not processed correctly by quicklisp.
Therefore you should put com-informatimago anywhere, eg.: ::

        cd ~/src
        git clone https://git.gitorious.org/com-informatimago/com-informatimago.git
        
and just add this path to the list ``ql:*local-project-directories*``: ::
       
        (push (merge-pathnames #P"./src/com-informatimago/" (user-homedir-pathname))
              ql:*local-project-directories*)


.. comment: this is not needed anymore, the patchwork/builder.lisp script determines the logical pathname automatically.

    2- Edit ``~/LOGHOSTS/PATCHWORK`` to tell where the sources of
       Patchwork are installed, eg.: ::

          ;; -*- mode:lisp; coding:utf-8; -*-

          #.(list
             (list "PATCHWORK:**;*.*.*"
                   (merge-pathnames #P"works/patchwork/patchwork/**/*.*"
                                    (user-homedir-pathname) nil))
             (list "PATCHWORK:**;*.*"
                   (merge-pathnames #P"works/patchwork/patchwork/**/*.*"
                                    (user-homedir-pathname) nil))
             (list "PATCHWORK:**;*"
                   (merge-pathnames #P"works/patchwork/patchwork/**/*"
                                    (user-homedir-pathname) nil)))


Loading and compiling for development
-------------------------------------

1- Launch Clozure.app (AppStore), or any other version of ccl >= 1.7
   (or ccl-1.6 on MacOSX 10.5.8).   You may also launch ccl with slime
   in emacs.

2- (cd #P"/directory/where/are/installed/the/sources/of/patchwork/")

3- (load "loader.lisp")


Generate the Patchwork.app application
--------------------------------------

1- cd "/repertoire/ou/sont/installes/les/sources/de/patchwork/"

2- make clean

3- make application


The application is stored in a directory on the Desktop whose name
contains the version numbers of ccl and of the system,
eg. ``~/Desktop/patchwork-10.0-0.000-ccl-1.9_darwinx8664-darwin-apple-13.1.0-x86-64/``.

A manifest file is written to containing the systems used and their licenses.

The generated application is compatible with MacOSX 10.3 and up if
compiled with ccl-1.6 on MacOSX 10.5.8, and with MacOSX 10.6 and more
otherwise.


Development tips
================

- If when launching the application nothing happens, try to launch it
  from the terminal: you may be dropped them in ccl debugger, or get
  an error message that would disappear in the Finder.

- The ``*trace-output*`` is configured for now to save to the file
  ``~/Desktop/patchwork-trace.txt``.


.. comment:  THE END 

