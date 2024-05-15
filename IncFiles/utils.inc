.macro PPU_SETADDR addr
  
  bit PPU_STATUS              ; reset PPU_ADDR latch
  lda #>addr                  ; read hi-byte of addr
  sta PPU_ADDR                ; send hi-byte of addr to PPU_ADDR ($2006)
  lda #<addr                  ; read lo-byte of addr
  sta PPU_ADDR                ; send lo-byte of addr to PPU_ADDR ($2006)

.endmacro

.macro PPU_SETDATA val
  lda val
  sta PPU_DATA                ; send value to PPU_DATA ($2007)
.endmacro
