.macro CHECK_BUTTON button_const
  lda button_const
  bit Buttons
.endmacro

.macro PERFORM_POSITIVE_ACCEL
  lda XVel                              ; load current XVel
  clc                                   ; add ACCEL to XVel
  adc #ACCEL
  cmp #MAXSPEED                         ; is new XVel > MAXSPEED?
  bcc :+                                ; if no, continue with new XVel
      lda #MAXSPEED                     ; else, new XVel = MAXSPEED
: sta XVel                              ; store new XVel
.endmacro

.macro PERFORM_NEGATIVE_ACCEL
  lda XVel                              ; load current XVel
  sec                                   ; subtract ACCEL from XVel
  sbc #ACCEL
  cmp #256-MAXSPEED                     ; is new XVel < MAXSPEED?
  bcs :+                                ; if no, continue with new XVel
      lda #256-MAXSPEED                 ; else, new XVel = MAXSPEED
  : sta XVel                            ; store new XVel
.endmacro

.macro PERFORM_POSITIVE_DECEL
  lda XVel                              ; load current XVel
  sec                                   ; subtract BRAKE from XVel
  sbc #BRAKE
  ; cmp #0                              ; is new XVel > 0?
  bpl :+                                ; if yes, continue with current XVel
      lda #0                            ; else, new XVel = 0
  : sta XVel                            ; store new XVel
.endmacro

.macro PERFORM_NEGATIVE_DECEL
  lda XVel                      ; load current XVel
  clc                           ; subtract BRAKE from XVel
  adc #BRAKE
  ; cmp #0                      ; is new XVel < 0?
  bmi :+                        ; if yes, continue with current XVel
      lda #0                    ; else, new XVel = 0
  : sta XVel
.endmacro