; =============================================================================
; DPV_speed_control.lisp - simple trigger speed and cruise control for Dive Xtras BlackTip / Sierra
; Inspiration from https://github.com/vedderb/vesc_pkg/blob/main/blacktip_dpv/blacktip_dpv.lisp
;
; "Click" = release and re-grab the trigger. The last press of a sequence is
; held to act immediately, or released to act after RELEASE_WINDOW.
;
; Stopped:  double click, hold   -> start at START_SPEED
;           triple click, hold   -> start at BOOST_SPEED
;           (taps without holding do nothing)
; Running:  single click         -> one speed slower
;           double click         -> one speed faster
;           triple click         -> cruise on, (keeps running when released)
;           let go > 0.5 s       -> stop
; Cruise:   single click         -> cruise off (keeps running while held,
;                                   stops 0.5 s after you let go)
;           double click         -> one speed faster (stays in cruise)
;           triple click         -> one speed slower (stays in cruise)
;           no trigger press for CRUISE_TIMEOUT -> stop
; =============================================================================

; ---------------- Settings ----------------
(define SCOOTER 'sierra)                    ; 'blacktip or 'sierra
(define SPEEDS (list 30 38 46 54 62 70 78 86 93 100)) ; % of max ERPM for speeds 1..10
(define START_SPEED 5)                        ; normal start speed (1..10)
(define BOOST_SPEED 8)                        ; triple-click start speed (1..10)
(define CRUISE_TIMEOUT 0)                    ; s without a trigger press before cruise stops (0 = never)


; ---------------- Constants ----------------
(define MAX_ERPM    (if (eq SCOOTER 'sierra) 7000 4100)) ;(sierra / blacktip)
(define MAX_CURRENT (if (eq SCOOTER 'sierra) 30 25))   ; l-in-current-max when running (sierra / blacktip)
(define SOFT_CURRENT (if (eq SCOOTER 'sierra) 0.35 1.7)) ; l-in-current-max during soft start (sierra / blacktip)

(define LOOP_TIME 0.02)        ; 50 Hz
(define CLICK_WINDOW 0.3)      ; hold this long to commit a click sequence
(define RELEASE_WINDOW 0.5)    ; let go this long to stop
(define SOFT_START_TIME 0.5)   ; duration of gentle duty spin-up
(define SOFT_START_DUTY 0.06)

; ---------------- State ----------------
(define state 'off)            ; 'off 'counting 'pressed 'releasing
(define clicks 0)
(define t0 0)                  ; timestamp for click/release windows
(define speed 0)               ; 0 = stopped, 1..10 = running speed
(define next_start START_SPEED)
(define soft_start_t 0)        ; > 0 while soft start is in progress
(define cruise nil)            ; t while cruise is on
(define cruise_t 0)            ; last trigger activity while cruising

; ---------------- Motor ----------------
(defun speed_erpm (n)
    (* MAX_ERPM (/ (ix SPEEDS (- n 1)) 100.0)))

(defun set_speed (n)
{
    (cond
        ((= n 0) {                                  ; stop
            (set-current 0)
            (conf-set 'l-in-current-max MAX_CURRENT)
            (setq soft_start_t 0)
            (setq speed 0)
        })
        ((= speed 0) {                              ; start from stop: soft start
            (conf-set 'l-in-current-max SOFT_CURRENT)
            (set-duty SOFT_START_DUTY)
            (setq soft_start_t (systime))
            (setq speed n)
        })
        (t {                                        ; change while running
            (setq speed n)
            (if (= soft_start_t 0) (set-rpm (speed_erpm n)))
        })
    )
})

(defun update_soft_start ()
    (if (and (> soft_start_t 0) (> (secs-since soft_start_t) SOFT_START_TIME)) {
        (conf-set 'l-in-current-max MAX_CURRENT)
        (set-rpm (speed_erpm speed))
        (setq soft_start_t 0)
    }))

; ---------------- Click actions ----------------
(defun apply_clicks (n)
    (cond
        ((= speed 0)                                ; stopped
            (cond
                ((= n 2) (set_speed next_start))
                ((= n 3) (set_speed BOOST_SPEED))))
        (cruise                                     ; cruising (one expression per cond clause)
            (cond
                ((= n 1) (setq cruise nil))                                    ; cancel cruise
                ((= n 2) (if (< speed (length SPEEDS)) (set_speed (+ speed 1)))) ; faster & stay in cruise
                ((= n 3) (if (> speed 1) (set_speed (- speed 1))))              ; slower & stay in cruise
            ))
        ((= n 1)                                    ; slower
            (if (> speed 1) (set_speed (- speed 1))))
        ((= n 2)                                    ; faster
            (if (< speed (length SPEEDS)) (set_speed (+ speed 1))))
        ((= n 3) {                                  ; cruise on
            (setq cruise t)
            (setq cruise_t (systime))
        })
    ))

(defun stop_motor ()
{
    (if (> speed 0) (setq next_start (if (< speed START_SPEED) speed START_SPEED))) ; restart no faster than last speed
    (set_speed 0)
    (setq cruise nil)
    (setq clicks 0)
    (setq state 'off)
})

; ---------------- Main loop ----------------
(gpio-configure 'pin-ppm 'pin-mode-in-pd)
(set-current 0)

; Ignore a trigger that is already held at power-up
(loopwhile (= (gpio-read 'pin-ppm) 1) (sleep 0.05))

(loopwhile t {
    (sleep LOOP_TIME)
    (var pressed (= (gpio-read 'pin-ppm) 1))

    (cond
        ((eq state 'off)
            (if pressed {
                (setq clicks 1)
                (setq t0 (systime))
                (setq state 'counting)
            }))

        ((eq state 'counting)                       ; held, waiting for click window
            (if (not pressed) {
                (setq t0 (systime))
                (setq state 'releasing)
            } (if (> (secs-since t0) CLICK_WINDOW) {
                (apply_clicks clicks)
                (setq clicks 0)
                (setq state 'pressed)
            })))

        ((eq state 'pressed)                        ; held, running
            (if (not pressed) {
                (setq t0 (systime))
                (setq state 'releasing)
            }))

        ((eq state 'releasing)                      ; let go: re-grab, cruise or stop
            (if pressed {
                (setq clicks (+ clicks 1))
                (setq t0 (systime))
                (setq state 'counting)
            } (if (> (secs-since t0) RELEASE_WINDOW)
                (if (and (> clicks 0) (> speed 0)) { ; quick taps while running
                    (apply_clicks clicks)
                    (setq clicks 0)
                    (setq t0 (systime))
                } (if (not cruise) (stop_motor))))))
    )

    ; Cruise safety timeout: any trigger press restarts it
    (if cruise {
        (if pressed (setq cruise_t (systime)))
        (if (and (> CRUISE_TIMEOUT 0) (> (secs-since cruise_t) CRUISE_TIMEOUT))
            (stop_motor))
    })

    (update_soft_start)
    (if (> speed 0) (timeout-reset))                ; keep motor command alive
})
