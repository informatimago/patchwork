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

        git clone git@gitlab.com:patchwork/patchwork.git
        git clone git@gitlab.com:patchwork/mclgui.git

1.2- Obtenir une version à jour de informatimago via git: ::

        cd ~/quicklisp/local-projects
        git clone https://git.gitlab.com/com-informatimago/com-informatimago.git

     Normalement ils sont obtenus automatiquement via quicklisp, mais
     il y peut y avoir des modifications nécessaires pour Patchwork
     dans le dépôt git, qui ne seront prises en compte par quicklisp
     qu'à la prochaine itération mensuelle.



     Note: avec ccl-1.6 sur MacOSX 10.5.8, la résolution des liens
     symboliques dans les ``local-projects`` ne fonctionne pas. Dans
     ce cas, il faut placer com-informatimago où on veut, par exemple: ::

        cd ~/src
        git clone https://git.gitlab.com/com-informatimago/com-informatimago.git

     et ajouter le chemin vers ce répertoire dans ``ql:*local-project-directories*``: ::

        (push (merge-pathnames #P"./src/com-informatimago/" (user-homedir-pathname))
              ql:*local-project-directories*)


Normalement, le script ``patchwork/builder.lisp``  determine les
logical pathnames pathnames automatiquement, ainsi nul besoin de
configuration.



Chargement et compilation en cours de développement
---------------------------------------------------

1- Lancer Clozure.app  (AppStore), ou une autre version de  ccl >= 1.7
   (ou ccl-1.6  sur MacOSX 10.5.8).   Il est aussi possible  de lancer
   ccl avec slime sur emacs.

2- (cd #P"/repertoire/ou/sont/installes/les/sources/de/patchwork/")

3- (shell "make -C /repertoire/ou/sont/installes/les/sources/de/patchwork/ clean")

4- (load "loader.lisp")

La version courante de ccl est 1.11 et celle de MacOSX est 10.12.3 macOS Sierra.


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
si compilé avec une version ultérieure de ccl sur une version
ultérieure de MacOSX.


Note pour les développeurs
==========================

- Si lorsqu'on lance l'application rien ne se passe, essayer de la
  lancer depuis le terminal : il se peut que l'on tombe dans le
  débogueur ccl, ou que l'on obtienne un message d'erreur qui
  n'apparait pas avec le Finder.

- Le flôt ``*trace-output*`` est configurer pour le moment pour
  s'enregistrer dans le fichier : ``~/Desktop/Patchwork-trace.txt``.


.. comment:
    __Pascal Bourguignon__
    Sat Mar  4 20:56:21 CET 2017
