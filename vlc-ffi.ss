#lang scheme

(require scheme/foreign)
(unsafe!)

(provide (all-defined-out))

(define VLC-PATH "/Applications/VLC.app/Contents/MacOS/lib/" )
(define libvlccore (ffi-lib (string-append VLC-PATH "libvlccore")))
(define libvlc (ffi-lib (string-append VLC-PATH "libvlc")))

(define-syntax define-pointer
  (syntax-rules ()
    ((_ ptr-type)
     (define ptr-type 
       (_cpointer/null (quote ptr-type))))))

(define-pointer vlc-instance)
(define-pointer vlc-media)
(define-pointer vlc-media-player)

;;(define-cstruct _vlc-exception ((raised _bool) (code _int32) (message _string)))

(define vlc-state
  (_enum '(libvlc_NothingSpecial libvlc_Opening libvlc_Buffering libvlc_Playing
           libvlc_Paused libvlc_Stopped libvlc_Forward libvlc_Backward 
           libvlc_Ended libvlc_Error)))

(define-syntax define-vlc
  (syntax-rules ()
    ((_ fn args)
     (define fn 
       (get-ffi-obj (quote fn) libvlc args)))))



;;;(define-vlc libvlc_exception_init (_fun _vlc-exception-pointer -> _void))
(define-vlc libvlc_new         (_fun _int32 (_vector i _string/utf-8) -> vlc-instance))
(define-vlc libvlc_get_version (_fun -> _string/utf-8))
(define-vlc libvlc_release     (_fun vlc-instance -> _void))
(define-vlc libvlc_add_intf    (_fun vlc-instance _string/utf-8 -> _void))

;;;
;;(define-vlc libvlc_media_new          (_fun vlc-instance _string/utf-8 _vlc-exception-pointer -> vlc-media))
(define-vlc libvlc_media_new_path     (_fun vlc-instance _string/utf-8 -> vlc-media))
(define-vlc libvlc_media_get_duration (_fun vlc-media  -> _int64))
(define-vlc libvlc_media_get_meta     (_fun vlc-media _int32  -> _string))
(define-vlc libvlc_media_get_mrl      (_fun vlc-media  -> _string))
(define-vlc libvlc_media_release      (_fun vlc-media -> _void))

;;;;;;;;;;;;;;
(define-vlc libvlc_media_player_new_from_media (_fun vlc-media  -> vlc-media-player))
(define-vlc libvlc_media_player_release        (_fun vlc-media-player -> _void))

(define-vlc libvlc_media_player_play           (_fun vlc-media-player  -> _void))
(define-vlc libvlc_media_player_pause          (_fun vlc-media-player  -> _void))
(define-vlc libvlc_media_player_stop           (_fun vlc-media-player  -> _void))
(define-vlc libvlc_media_player_set_position   (_fun vlc-media-player _float  -> _void))

(define-vlc libvlc_media_player_get_time       (_fun vlc-media-player  -> _int64))
(define-vlc libvlc_media_player_get_position   (_fun vlc-media-player  -> _float))
(define-vlc libvlc_media_player_get_length     (_fun vlc-media-player  -> _int64))
(define-vlc libvlc_media_player_get_state      (_fun vlc-media-player  -> vlc-state))
;;;(define-vlc libvlc_media_player_get_state      (_fun vlc-media-player _vlc-exception-pointer -> _fixnum))
