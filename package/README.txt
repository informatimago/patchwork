.. comment:  -*- mode:rst; coding:utf-8 -*-

Introduction
========================================

PW (PatchWork) is an interactive environment for computer assisted
composition. It consists of a set of tools that help the composer
generate and manipulate material in the "precompositional"
stages. Such an environment can be thought of more as an intelligent
sketchpad or a powerful musical calculator, and less as a machine for
automated composition.

An environment for computer-assisted composition must provide tools
for the creation of musical material. These tools must be capable of
representing, playing and transforming the created material. The
composer must be able to reuse previous steps of work, to define
structural hierarchies in which low-level material can be elaborated.

PatchWork's lets users choose the level of access to the musical
objects (for example: a note, a beat, a measure, a section, a whole
piece); thus a composer can work on musical material and musical form
in a continuous manner.

Another requirement of an environment for computer-assisted
composition is communication with other programs for editing, live
performance, music printing, and sound synthesis.

Finally, an environment for computer-assisted composition must be
extensible. It must provide tools not just for the creation of musical
objects, but also for the creation of new concepts, formalizations,
and transformations.

To summarize, the PatchWork environment is an interactive environment
for music composition and control of sound synthesis. It helps
composers do the following:

• Generate and manipulate musical objects (chords, sequences and
  rhythms organized in hierarchical struc- tures)

• Exchange material with other environments such as Max, MIDI
  sequencers, musical notation programs

(Coda Finale), or any application that can make use of data in a text
format (ASCII), Enigma files and MIDI files.

• Control sound synthesis (MIDI synthesizers, Csound, CHANT, Mosaïc)

• And generally, create and execute Lisp programs through the
  intermediary of graphic patches

The current version, written by Mikael Laurson, Camilo Rueda and
Jacques Duthen, was developed in Apple Macin- tosh Allegro Common
Lisp. PatchWork’s paradigm is based on functional programming, through
the use of graphical patches. As well as graphical patches, users can
access the text-based programming environments Common Lisp and CLOS
(Common Lisp Object System).


History of the PatchWork Project
----------------------------------------

(from PW-introduction.pdf)

Computer-assisted composition and tools for synthesis control have
been permanent interests at Ircam, as witness these predecessors of
PatchWork:

- FORMES (1982) — developed for the control of synthesis and the
  organization of hierarchical tasks, especially for CHANT, the phase
  vocoder, and the Ircam 4X synthesizer.

- CRIME (1985-1986) — an environment for computer-assisted composition
  in Lisp; the first at Ircam to generate output in contemporary music
  notation.

- PreFORM (1986) — an object-oriented Lisp platform, for Apple
  Macintosh. This program offered the possibility of graphical
  interaction; it also controlled MIDI functions.

- Esquisse (1988) — an environment for computer assisted composition
  that incorporates aspects of spectral music

The original concept for PatchWork was conceived by Mikael Laurson in
Finland. It was then integrated into Ircam as a project of the
Department of Musical Research. The initial program was rewritten and
enhanced by Mikail Laur- son, Jacques Duthen and Camilo Rueda at
Ircam.

Musical applications and documentation were written by Xavier Chabot,
David Waxman, Francisco Iovino, Joshua Fineberg, and Mikhail
Malt. Jean-Baptiste Barrière, Tristan Murail and Andrew Gerzso
supervised the development work.

Pierre-François Baisnée, Yves Potard, Magnus Lindberg, Marc-André
Dalbavie, Kaija Saariaho, Antoine Bonnet, François Nicolas, and
Frédéric Durieux participated in the project at different stages of
the development. The MIDI driver and task manager are by Lee Boyton,
revised by Jacques Duthen.

The design of version 2.5.1 of PatchWork is the result of
collaboration between Camilo Rueda, Mikhail Malt, Tristan Murail,
Xavier Chabot, Gérard Assayag, and Antoine Bonnet. The actual version
2.5.1 of PW is the result of collab- oration between Gérard Assayag,
Carlos Augusto Agon and Mikhail Malt.

In 2014, Alexis Bosch contracted  Pascal Bourguignon to port Patchwork
from MacOS to MacOSX.  Here is the result of this work.


