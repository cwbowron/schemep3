#lang scheme

(require "vlc-ffi.ss")

(define (inspect ex)
  (printf "E: ~A~%" ex)
  (printf "Raised: ~A~%" (vlc-exception-raised ex))
  (printf "Code: ~A~%" (vlc-exception-code ex)))

(define (play-to-end media-player e)
  (let loop ()
    (let ([state (libvlc_media_player_get_state media-player e)]
          [time (libvlc_media_player_get_time media-player e)])
      (printf "State: ~A Time: ~A~%" state time)
      (unless (member state '(#f libvlc_Stopped libvlc_Ended libvlc_Error))
        (sleep .5)
        (loop)))))

(define (vlc-play media-path)
  (unless (file-exists? media-path)
    (raise (format "Missing File!: ~A" media-path)))

  (printf "VLC Version: ~A~%" (libvlc_get_version))
  (let ([e (make-vlc-exception #f 0 "")])
    (let* ([vlc-args (vector
                      "--plugin-path=C:\\Program Files\\VideoLAN\\VLC\\plugins"
                      "--aout=audiofile"
                      "--audiofile-file=out.wav"
                      "--audiofile-format=u16"
                      "-I" "dummy")]
           [vlc (libvlc_new (vector-length vlc-args) vlc-args e)])
      (let* ([media-mrl (string-append "file://" media-path)]
             [vlc-media
              (libvlc_media_new vlc media-mrl e)]
             [vlc-media-player (libvlc_media_player_new_from_media vlc-media e)])
        (libvlc_media_player_play vlc-media-player e)
        (play-to-end vlc-media-player e)
        (libvlc_media_player_stop vlc-media-player e)
        (libvlc_media_player_release vlc-media-player)
        (libvlc_media_release vlc-media)
        (libvlc_release vlc)))))

(vlc-play "etc/test2.flac")
