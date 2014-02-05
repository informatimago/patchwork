;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cocoa-midi-player.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-08-17 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2013
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************


(defpackage "MIDI-PLAYER"
  (:use "COMMON-LISP")
  (:export "BAR" "BEAT" "UNIT" "S-BAR" "S-BEAT" "S-UNIT" "S-CURDATE"
           "S-TEMPO" "S-NUM" "S-DENOM" "S-CLICK" "S-QUARTER" "S-STATE"
           "S-SYNCIN" "S-SYNCOUT" "MF-FORMAT" "MF-TIMEDEF" "MF-CLICKS"
           "MF-TRACKS" "OPENPLAYER" "CLOSEPLAYER" "OPEN-PLAYER"
           "STARTPLAYER" "CONTPLAYER" "STOPPLAYER" "PAUSEPLAYER"
           "SETRECORDMODEPLAYER" "RECORDPLAYER"
           "SETRECORDFILTERPLAYER" "SETPOSBBUPLAYER" "SETPOSMSPLAYER"
           "SETLOOPPLAYER" "SETLOOPSTARTBBUPLAYER"
           "SETLOOPENDBBUPLAYER" "SETLOOPSTARTMSPLAYER"
           "SETLOOPENDMSPLAYER" "SETSYNCHROINPLAYER"
           "SETSYNCHROOUTPLAYER" "SETSMPTEOFFSETPLAYER"
           "GETSTATEPLAYER" "GETENDSCOREPLAYER" "FORWARDSTEPPLAYER"
           "BACKWARDSTEPPLAYER" "GETALLTRACKPLAYER" "GETTRACKPLAYER"
           "SETTRACKPLAYER" "SETALLTRACKPLAYER" "SETPARAMPLAYER"
           "GETPARAMPLAYER" "MIDIFILESAVE" "MIDIFILELOAD"
           "MIDI-FILE-LOAD" "MIDI-FILE-SAVE"))

(in-package "MIDI-PLAYER")



#||

#include <CoreServices/CoreServices.h> //for file stuff
#include <AudioUnit/AudioUnit.h>
#include <AudioToolbox/AudioToolbox.h> //for AUGraph
#include <unistd.h> // used for usleep...

// This call creates the Graph and the Synth unit...
OSStatus    CreateAUGraph (AUGraph &outGraph, AudioUnit &outSynth)
{
    OSStatus result;
    //create the nodes of the graph
    AUNode synthNode, limiterNode, outNode;

    AudioComponentDescription cd;
    cd.componentManufacturer = kAudioUnitManufacturer_Apple;
    cd.componentFlags = 0;
    cd.componentFlagsMask = 0;

    require_noerr (result = NewAUGraph (&outGraph), home);

    cd.componentType = kAudioUnitType_MusicDevice;
    cd.componentSubType = kAudioUnitSubType_DLSSynth;

    require_noerr (result = AUGraphAddNode (outGraph, &cd, &synthNode), home);

    cd.componentType = kAudioUnitType_Effect;
    cd.componentSubType = kAudioUnitSubType_PeakLimiter;  

    require_noerr (result = AUGraphAddNode (outGraph, &cd, &limiterNode), home);

    cd.componentType = kAudioUnitType_Output;
    cd.componentSubType = kAudioUnitSubType_DefaultOutput;  
    require_noerr (result = AUGraphAddNode (outGraph, &cd, &outNode), home);

    require_noerr (result = AUGraphOpen (outGraph), home);

    require_noerr (result = AUGraphConnectNodeInput (outGraph, synthNode, 0, limiterNode, 0), home);
    require_noerr (result = AUGraphConnectNodeInput (outGraph, limiterNode, 0, outNode, 0), home);

    // ok we're good to go - get the Synth Unit...
    require_noerr (result = AUGraphNodeInfo(outGraph, synthNode, 0, &outSynth), home);

home:
    return result;
}


// some MIDI constants:
enum {
    kMidiMessage_ControlChange      = 0xB,
    kMidiMessage_ProgramChange      = 0xC,
    kMidiMessage_BankMSBControl     = 0,
    kMidiMessage_BankLSBControl     = 32,
    kMidiMessage_NoteOn             = 0x9
};

int main (int argc, const char * argv[]) {
    AUGraph graph = 0;
    AudioUnit synthUnit;
    OSStatus result;
    char* bankPath = 0;

    UInt8 midiChannelInUse = 0; //we're using midi channel 1...

    // this is the only option to main that we have...
    // just the full path of the sample bank...

    // On OS X there are known places were sample banks can be stored
    // Library/Audio/Sounds/Banks - so you could scan this directory and give the user options
    // about which sample bank to use...
    if (argc > 1)
        bankPath = const_cast<char*>(argv[1]);

    require_noerr (result = CreateAUGraph (graph, synthUnit), home);

    // if the user supplies a sound bank, we'll set that before we initialize and start playing
    if (bankPath) 
    {
        FSRef fsRef;
        require_noerr (result = FSPathMakeRef ((const UInt8*)bankPath, &fsRef, 0), home);

        printf ("Setting Sound Bank:%s\n", bankPath);

        require_noerr (result = AudioUnitSetProperty (synthUnit,
                                                      kMusicDeviceProperty_SoundBankFSRef,
                                                      kAudioUnitScope_Global, 0,
                                                      &fsRef, sizeof(fsRef)), home);

    }

    // ok we're set up to go - initialize and start the graph
    require_noerr (result = AUGraphInitialize (graph), home);

    //set our bank
    require_noerr (result = MusicDeviceMIDIEvent(synthUnit, 
                                                 kMidiMessage_ControlChange << 4 | midiChannelInUse, 
                                                 kMidiMessage_BankMSBControl, 0,
                                                 0/*sample offset*/), home);

    require_noerr (result = MusicDeviceMIDIEvent(synthUnit, 
                                                 kMidiMessage_ProgramChange << 4 | midiChannelInUse, 
                                                 0/*prog change num*/, 0,
                                                 0/*sample offset*/), home);

    CAShow (graph); // prints out the graph so we can see what it looks like...

    require_noerr (result = AUGraphStart (graph), home);

    // we're going to play an octave of MIDI notes: one a second
    for (int i = 0; i < 13; i++) {
        UInt32 noteNum = i + 60;
        UInt32 onVelocity = 127;
        UInt32 noteOnCommand =  kMidiMessage_NoteOn << 4 | midiChannelInUse;

        printf ("Playing Note: Status: 0x%lX, Note: %ld, Vel: %ld\n", noteOnCommand, noteNum, onVelocity);

        require_noerr (result = MusicDeviceMIDIEvent(synthUnit, noteOnCommand, noteNum, onVelocity, 0), home);

        // sleep for a second
        usleep (1 * 1000 * 1000);

        require_noerr (result = MusicDeviceMIDIEvent(synthUnit, noteOnCommand, noteNum, 0, 0), home);
    }

    // ok we're done now

home:
    if (graph) {
        AUGraphStop (graph); // stop playback - AUGraphDispose will do that for us but just showing you what to do
        DisposeAUGraph (graph);
    }
    return result;
}

||#



;; Date structures ond constants
;;===============================

;;-------------------------------------------------------------------------- 
;; Player state   
;;-------------------------------------------------------------------------- 


(defparameter kIdle       0)
(defparameter kPause      1)
(defparameter kRecording  2)
(defparameter kPlaying    3)
(defparameter kWaiting    4)


;;-------------------------------------------------------------------------- 
;; Tracks state   
;;-------------------------------------------------------------------------- 

(defparameter kMaxTrack 256)
(defparameter kMuteOn   1)
(defparameter kMuteOff  0)
(defparameter kSoloOn   1)
(defparameter kSoloOff  0)
(defparameter kMute     0)
(defparameter kSolo     1)


;;------------------------------------------------------------------ 
;; Recording management  
;;-------------------------------------------------------------------------- 

(defparameter kNoTrack          -1)
(defparameter kEraseMode        1)
(defparameter kMixMode          0)

;;-------------------------------------------------------------------------- 
;; Loop  management  
;;-------------------------------------------------------------------------- 

(defparameter kLoopOn   0)
(defparameter kLoopOff  1)


;;-------------------------------------------------------------------------- 
;; Step Playing  
;;-------------------------------------------------------------------------- 

(defparameter kStepPlay  1)
(defparameter kStepMute  0)

;;-------------------------------------------------------------------------- 
;; Synchronisation  
;;-------------------------------------------------------------------------- 

(defparameter kInternalSync     0)
(defparameter kClockSync        1)
(defparameter kSMPTESync        2)


(defparameter kNoSyncOut        0)
(defparameter kClockSyncOut     1)


;;-------------------------------------------------------------------------- 
;; MIDIfile  
;;-------------------------------------------------------------------------- 

(defparameter midifile0  0)
(defparameter midifile1  1)
(defparameter midifile2  2)


(defparameter TicksPerQuarterNote       0)
(defparameter Smpte24                   24)
(defparameter Smpte25                   25)
(defparameter Smpte29                   29)
(defparameter Smpte30                   30)


;;-------------------------------------------------------------------------- 
;; Errors  : for the player
;;-------------------------------------------------------------------------- 

(defparameter PLAYERnoErr                       -1) ; no error
(defparameter PLAYERerrAppl                     -2) ; Unable to open MidiShare application
(defparameter PLAYERerrEvent                    -3) ; No more MidiShare Memory
(defparameter PLAYERerrMemory                   -4) ; No more Mac Memory
(defparameter PLAYERerrSequencer                -5) ; Sequencer error                                


;;-------------------------------------------------------------------------- 
;; Errors  :  for MidiFile
;;-------------------------------------------------------------------------- 

(defparameter noErr                     0) ; no error                                              
(defparameter ErrOpen                   1) ; file open error       
(defparameter ErrRead                   2) ; file read error       
(defparameter ErrWrite                  3) ; file write error      
(defparameter ErrVol                    4) ; Volume error          
(defparameter ErrGetInfo                5) ; GetFInfo error        
(defparameter ErrSetInfo                6) ; SetFInfo error        
(defparameter ErrMidiFileFormat         7) ;  bad MidiFile format  


;; Record for position management
;;================================

(defmacro defrecord (name &rest slots)
  `(defstruct ,name ,@(mapcar (function first) slots)))

(defmacro niy (item &rest vars)
  ``(format *trace-output* "~&(~40A (~S~:{(~S ~S)~^ ~}))~%"
            "not implemented yet:"
            ',',item ',(mapcar (lambda (var) (list var (type-of var)))
                               (list ,@vars))))

(defrecord Pos
  (bar  :short)  
  (beat :short)     
  (unit :short))

(defmacro bar (e &optional (d nil d?))
  (niy bar e d d?) #-(and)
  (if d?
      `(rset ,e :Pos.bar ,d)
      `(rref ,e :Pos.bar)))

(defmacro beat (e &optional (d nil d?))
  (niy beat e d d?) #-(and)
  (if d?
      `(rset ,e :Pos.beat ,d)
      `(rref ,e :Pos.beat)))

(defmacro unit (e &optional (d nil d?))
  (niy unit e d d?) #-(and)
  (if d?
      `(rset ,e :Pos.unit ,d)
      `(rref ,e :Pos.unit)))



;; Record for state management
;;================================

(defrecord (PlayerState (:conc-name s-))
    (date  :longint)
  (tempo :longint)
  (tsnum :short)
  (tsdenom :short)
  (tsclick :short)
  (tsquarter :short)
  (bar  :short)   
  (beat :short)     
  (unit  :short)
  (state  :short)
  (syncin  :short)
  (syncout  :short))

;; (defmacro s-bar (e )
;;   `(rref ,e :PlayerState.bar))
;; 
;; (defmacro s-beat (e)
;;   `(rref ,e :PlayerState.beat))
;; 
;; (defmacro s-unit (e)
;;   `(rref ,e :PlayerState.unit))
;; 
;; (defmacro s-curdate (e )
;;   `(rref ,e :PlayerState.date))
;; 
;; (defmacro s-tempo (e)
;;   `(rref ,e :PlayerState.tempo))
;; 
;; (defmacro s-num (e)
;;   `(rref ,e :PlayerState.tsnum))
;; 
;; (defmacro s-denom (e)
;;   `(rref ,e :PlayerState.tsdenom))
;; 
;; (defmacro s-click (e )
;;   `(rref ,e :PlayerState.tsclick))
;; 
;; (defmacro s-quarter (e )
;;   `(rref ,e :PlayerState.tsquarter))
;; 
;; (defmacro s-state (e )
;;   `(rref ,e :PlayerState.state))
;; 
;; (defmacro s-syncin (e )
;;   `(rref ,e :PlayerState.syncin))
;; 
;; (defmacro s-syncout (e )
;;   `(rref ,e :PlayerState.syncout))


;; Record for MidiFile
;;================================


(defrecord (MidiFileInfos (:conc-name mf-))
    (format  :longint)     
  (timedef  :longint)   
  (clicks :longint)      
  (tracks  :longint))    

;; (defmacro mf-format (e )
;;   `(rref ,e :MidiFileInfos.format))
;; 
;; (defmacro mf-timedef (e )
;;   `(rref ,e :MidiFileInfos.timedef))
;; 
;; (defmacro mf-clicks (e )
;;   `(rref ,e :MidiFileInfos.clicks))
;; 
;; (defmacro mf-tracks (e )
;;   `(rref ,e :MidiFileInfos.tracks))


;; Interface to C entry points
;;================================

(defmacro define-entry-point ((name (library)) (&rest arguments) &optional result-type)
  (declare (ignorable name library arguments result-type))
  `(defun ,(intern (string-upcase name)) ,(mapcar (function first) arguments)
     (niy  ,(intern (string-upcase name)) ,@(mapcar (function first) arguments))))

(defmacro with-cstrs (bindings &body body) `(let ,bindings ,@body))
(defmacro with-pstrs (bindings &body body) `(let ,bindings ,@body))

(define-entry-point ( "OpenPlayer" ("PlayerSharedPPC")) ((name :ptr) ) :short)
(define-entry-point ( "ClosePlayer" ("PlayerSharedPPC")) ((refnum :short)))

(defun open-player (name)
  (with-pstrs ((pstr name))
    (openplayer pstr)))

;; Transport control
;;===================

(define-entry-point ("StartPlayer" ("PlayerSharedPPC")) ((refnum :short)))
(define-entry-point ("ContPlayer"  ("PlayerSharedPPC")) ((refnum :short)))
(define-entry-point ("StopPlayer"  ("PlayerSharedPPC")) ((refnum :short)))
(define-entry-point ("PausePlayer" ("PlayerSharedPPC")) ((refnum :short)))

;; Record management
;;===================

(define-entry-point ("SetRecordModePlayer" ("PlayerSharedPPC"))  ((refnum :short) (state :short)))
(define-entry-point ("RecordPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum :short)))
(define-entry-point ("SetRecordFilterPlayer" ("PlayerSharedPPC"))  ((refnum :short) (filter :ptr)))


;; Position management
;;=====================

(define-entry-point ("SetPosBBUPlayer" ("PlayerSharedPPC")) ((refnum :short) (pos :ptr)))
(define-entry-point ("SetPosMsPlayer" ("PlayerSharedPPC")) ((refnum :short)  (date_ms :longint)))

;; Loop management
;;==================

(define-entry-point ("SetLoopPlayer" ("PlayerSharedPPC")) ((refnum :short) (state :short)))
(define-entry-point ("SetLoopStartBBUPlayer" ("PlayerSharedPPC")) ((refnum :short) (pos :ptr)) :long)
(define-entry-point ("SetLoopEndBBUPlayer" ("PlayerSharedPPC")) ((refnum :short) (pos :ptr)) :long)
(define-entry-point ("SetLoopStartMsPlayer"("PlayerSharedPPC")) ((refnum :short)  (date_ms :longint)) :long)
(define-entry-point ("SetLoopEndMsPlayer" ("PlayerSharedPPC")) ((refnum :short)  (date_ms :longint)) :long)

;; Synchronisation management
;;============================

(define-entry-point ("SetSynchroInPlayer" ("PlayerSharedPPC")) ((refnum :short) (state :short)))
(define-entry-point ("SetSynchroOutPlayer" ("PlayerSharedPPC")) ((refnum :short) (state :short)))
(define-entry-point ("SetSMPTEOffsetPlayer" ("PlayerSharedPPC")) ((refnum :short) (smptepos :ptr)))


;; State management
;;===================

(define-entry-point ("GetStatePlayer" ("PlayerSharedPPC")) ((refnum :short) (playerstate :ptr )))
(define-entry-point ("GetEndScorePlayer" ("PlayerSharedPPC")) ((refnum :short) (playerstate :ptr )))


;; Step playing 
;;==============

(define-entry-point ("ForwardStepPlayer" ("PlayerSharedPPC")) ((refnum :short) (flag :short)))
(define-entry-point ("BackwardStepPlayer" ("PlayerSharedPPC")) ((refnum :short) (flag :short)))

;; Tracks management
;;====================

(define-entry-point ("GetAllTrackPlayer" ("PlayerSharedPPC")) ((refnum :short)) :ptr)
(define-entry-point ("GetTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum :short)) :ptr)

(define-entry-point ("SetTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum :short) (seq :ptr)) :long)
(define-entry-point ("SetAllTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (seq :ptr) (ticks_per_quarter :long)) :long)

(define-entry-point ("SetParamPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum short) (param short) (value short )))
(define-entry-point ("GetParamPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum short) (param short)) :short )

;; Midifile management
;;====================

(define-entry-point ("MidiFileSave" ("PlayerSharedPPC")) (( name :ptr) (seq :ptr) (infos :ptr)) :long)
(define-entry-point ("MidiFileLoad" ("PlayerSharedPPC")) (( name :ptr) (seq :ptr) (infos :ptr)) :long)

(defun midi-file-load (name seq info)
  (with-cstrs ((cstr name))
    (MidiFileLoad cstr seq info)))

(defun midi-file-save (name seq info)
  (with-cstrs ((cstr name))
    (MidiFileSave cstr seq info)))


;;;; THE END ;;;;

