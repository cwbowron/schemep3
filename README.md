# schemep3
Schemep3 is a music playback and database program written in PLT Scheme. It can playback any audio format that libVLC supports and read tags in any format that TagLib understands. It uses Jay McCarthy’s SQLite module for database access.

## Features
* SQLite Database
  * Track last played and play count
* Tags via TagLib
  * FFI interface to C wrapper functions
  * scheme extension for id3v2 frame blocks
  * scheme extension for flac comments
* Playback
  * libvlc for playback
  * playback seek by clicking on progress bar
* Playlist
  * Accepts dropped files or results from database query
  * Dropping an m3u loads all the files contained in it
  * Configurable format
  * Marks the current playing file in the playlist
  * Playlist ordering using meta-up, meta-down
  * multiple playlists
* Playback Queue
  * Play next option
  * Add to queue option
* GUI
  * Optional album art display (folder.jpg)
  * UI for changing ratings
  * Progress bar
  * keyboard controls
    * enter -> play
    * space -> pause
    * n,N -> next
* Misc
  * playback queue viewer
  * Export to m3u (playlist context menu)
  * last.fm submission
  * store / restore database queries
  * Move to library options
    * Moves media file into my \Music\Library\A\ARTIST\YEAR – ALBUM\0x – TITLE format
    * Moves album art also
    * remove empty directories
  * copy to mini library [for mp3 player sync]
    * convert to mp3
    * input -> pcm -> mp3 via mplayer and lame
* OS Specific
  * Windows
     * Pause on terminal lock
     * global hotkeys (C-A-Space => Pause, C-A-Right Arrow => Next )
* Deprecated
  * external mplayer playback backend
  * id3lib ffi – deprecated for taglib
  * FLAC
    * tag parser
    * duration
  * mp3
    * id3v1 tag reader
    * id3v2 tag reader
    * estimates playback length (hacky)
