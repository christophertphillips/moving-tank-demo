.macro INIT_NES

  sei                         ; disable all IRQ interrputs
  cld                         ; clear BCD mode (unsupported)
  ldx #$FF                    ; initialize stack pointer at $01FF
  txs

  lda #0
  sta PPU_CTRL                ; disable NMI
  sta PPU_MASK                ; disable rendering (masking background and sprites)
  sta $4010                   ; disable DMC IRQs

  lda #$40                    ; disable APU frame IRQ
  sta $4017

Wait1stVBlank:                ; wait for the first VBlank from the cpu
  bit PPU_STATUS              ; check if currently in a VBlank (via bit 7 in PPU_STATUS)
  bpl Wait1stVBlank           ; loop until bit 7 (sign bit) in PPU_STATUS is 1 (inside VBlank)

  lda #$0                     ; A = 0
  ldx #$0                     ; X = 0 (used to ensure off-by-one error doesn't occur at $0100)
ClearRAM:
  sta $0000,X                 ; zero RAM address from $0000 to $00FF
  sta $0100,X                 ; zero RAM address from $0000 to $00FF
  sta $0200,X                 ; zero RAM address from $0000 to $00FF
  sta $0300,X                 ; zero RAM address from $0000 to $00FF
  sta $0400,X                 ; zero RAM address from $0000 to $00FF
  sta $0500,X                 ; zero RAM address from $0000 to $00FF
  sta $0600,X                 ; zero RAM address from $0000 to $00FF
  sta $0700,X                 ; zero RAM address from $0000 to $00FF
  inx                         ; X++
  bne ClearRAM                ; if X is not zero, loop back to MemLoop

Wait2ndVBlank:                ; wait for the first VBlank from the cpu
  bit PPU_STATUS              ; check if currently in a VBlank (via bit 7 in PPU_STATUS)
  bpl Wait2ndVBlank           ; loop until bit 7 (sign bit) in PPU_STATUS is 1 (inside VBlank)

.endmacro