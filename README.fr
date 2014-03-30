;; -*- mode:rst; coding:utf-8 -*-

Patchwork
#########

Ce répertoire contient les  sources de l'application Patchwork, portée
de MacOS (MCL) à MacOSX (ccl).

License
=======

Voir le fichier ``Patchwork_license.rtf`` ou ``Patchwork_license.txt``.

Compilation de Patchwork
========================

Installation des sources de Patchwork
-------------------------------------

0- Installer quicklisp. cf. _`http://beta.quicklisp.org/`.

1- Obtenir les sources.

1.1- Obtenir les sources de Patchwork via git  ::

        git clone git@gitorious.org:patchwork/patchwork.git

1.2- Obtenir une version à jour de informatimago via git: ::

        cd ~/quicklisp/local-projects
        git clone https://git.gitorious.org/com-informatimago/com-informatimago.git

     Normalement ils sont obtenus automatiquement via quicklisp, mais
     il y peut y avoir des modifications nécessaires pour Patchwork
     dans le dépôt git, qui ne seront prises en compte par quicklisp
     qu'à la prochaine itération mensuelle.  



     Note: avec ccl-1.6 sur MacOSX 10.5.8, la résolution des liens
     symboliques dans les ``local-projects`` ne fonctionne pas. Dans
     ce cas, il faut placer com-informatimago où on veut, par exemple: ::

        cd ~/src
        git clone https://git.gitorious.org/com-informatimago/com-informatimago.git
        
     et ajouter le chemin vers ce répertoire dans ``ql:*local-project-directories*``: ::
       
        (push (merge-pathnames #P"./src/com-informatimago/" (user-homedir-pathname))
              ql:*local-project-directories*)


2- Editer ``~/LOGHOSTS/PATCHWORK`` pour indiquer où sont installés les
   sources de Patchwork ; par exemple: ::

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



Chargement et compilation en cours de développement
---------------------------------------------------

1- Lancer Clozure.app  (AppStore), ou une autre version de  ccl >= 1.7
   (ou ccl-1.6  sur MacOSX 10.5.8).   Il est aussi possible  de lancer
   ccl avec slime sur emacs.

2- (cd #P"/repertoire/ou/sont/installes/les/sources/de/patchwork/")

3- (shell "make -C /repertoire/ou/sont/installes/les/sources/de/patchwork/ clean")

4- (load "loader.lisp")


Génération de l'application Patchwork.app
-----------------------------------------

1- cd "/repertoire/ou/sont/installes/les/sources/de/patchwork/"

2- make clean

3- make application


L'application est enregistré dans un répertoire sur le bureau
contenant les numéro de version de ccl et du système,
eg. ``~/Desktop/patchwork-ccl-1.9_darwinx8664-darwin-apple-13.1.0-x86-64/``.

Un fichier  manifest est  également enregistré indiquant  les systèmes
utilisés et leurs licenses.


L'application  générée est  compatible  avec MacOSX  10.3  et plus  si
compilée avec ccl-1.6  sur MacOSX 10.5.8, et avec MacOSX  10.6 et plus
sinon.

-- 
__Pascal Bourguignon__
Sun Mar 30 18:52:23 CEST 2014

