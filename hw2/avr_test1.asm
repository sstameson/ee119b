;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; AVR test program #1
;
; Hayward Melton
; Spiro Stameson
;
; This a test program for an AVR CPU. This file contains code that
; checks all AVR instructions and there are annotated comments to enable the
; future generation of test vectors.
;
; `;W <val> <addr>` means that the 8-bit value <val> is written
;     to the 16-bit address <addr> on the data memory bus
; `;R <val> <addr>` means that the 8-bit value <val> is read
;     from the 16-bit address <addr> on the data memory bus
; `;J` means that a jump is taken
; `;S` means that the next instruction is skipped
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;
; set all flags
;
    BSET 0               
    BSET 1               
    BSET 2               
    BSET 3               
    BSET 4               
    BSET 5               
    BSET 6               
    BSET 7               

;
; check all flags set
;
    BRBC 0,Skip1         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 0,Skip1         ;J flag C set so branch taken
    .ORG $0040           
Skip1:
    BRBC 1,Skip2         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 1,Skip2         ;J flag Z set so branch taken
    .ORG $007F           
Skip2:
    BRBC 2,Skip3         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 2,Skip3         ;J flag N set so branch taken
    .ORG $00BE           
Skip3:
    BRBC 3,Skip4         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 3,Skip4         ;J flag V set so branch taken
    .ORG $00FD           
Skip4:
    BRBC 4,Skip5         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 4,Skip5         ;J flag S set so branch taken
    .ORG $013C           
Skip5:
    BRBC 5,Skip6         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 5,Skip6         ;J flag H set so branch taken
    .ORG $017B           
Skip6:
    BRBC 6,Skip7         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 6,Skip7         ;J flag T set so branch taken
    .ORG $01BA           
Skip7:
    BRBC 7,Skip8         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 7,Skip8         ;J flag I set so branch taken
    .ORG $01F9           
Skip8:

;
; clear all flags
;
    BCLR 0               
    BCLR 1               
    BCLR 2               
    BCLR 3               
    BCLR 4               
    BCLR 5               
    BCLR 6               
    BCLR 7               

;
; check all flags clear
;
    BRBS 0,Skip9         ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 0,Skip9         ;J flag C clear so branch taken
    .ORG $0238           
Skip9:
    BRBS 1,Skip10        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 1,Skip10        ;J flag Z clear so branch taken
    .ORG $0277           
Skip10:
    BRBS 2,Skip11        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 2,Skip11        ;J flag N clear so branch taken
    .ORG $02B6           
Skip11:
    BRBS 3,Skip12        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 3,Skip12        ;J flag V clear so branch taken
    .ORG $02F5           
Skip12:
    BRBS 4,Skip13        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 4,Skip13        ;J flag S clear so branch taken
    .ORG $0334           
Skip13:
    BRBS 5,Skip14        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 5,Skip14        ;J flag H clear so branch taken
    .ORG $0373           
Skip14:
    BRBS 6,Skip15        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 6,Skip15        ;J flag T clear so branch taken
    .ORG $03B2           
Skip15:
    BRBS 7,Skip16        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 7,Skip16        ;J flag I clear so branch taken
    .ORG $03F1           
Skip16:

;
; check T flag operations
;
; check bit set
    LDI  R16,$FF         ; load R16 <- 0xff
    BLD  R16,$01         ; load R16(1) <- T
    BLD  R16,$03         ; load R16(3) <- T
    STS  $1000,R16       ;W F5 1000 check bit load from T flag result
; check bit load
    BST  R16,$00         ; set T <- R16(0)
    BRBC 6,Skip17        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 6,Skip17        ;J flag T set so branch taken
    .ORG $0430           
Skip17:
    BST  R16,$01         ; set T <- R16(1)
    BRBS 6,Skip18        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 6,Skip18        ;J flag T clear so branch taken
    .ORG $046F           
Skip18:

;
; check compare operations
;
; check compare
; test carry flag
    LDI  R16,$02         ; load R16 <- 0x2
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBS 0,Skip19        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 0,Skip19        ;J flag C clear so branch taken
    .ORG $04AE           
Skip19:
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$80         ; load R17 <- 0x80
    CP   R16,R17         ; compute R16 - R17
    BRBC 0,Skip20        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 0,Skip20        ;J flag C set so branch taken
    .ORG $04ED           
Skip20:
; test zero flag
    LDI  R16,$02         ; load R16 <- 0x2
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBS 1,Skip21        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 1,Skip21        ;J flag Z clear so branch taken
    .ORG $052C           
Skip21:
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    CP   R16,R17         ; compute R16 - R17
    BRBC 1,Skip22        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 1,Skip22        ;J flag Z set so branch taken
    .ORG $056B           
Skip22:
; test negative flag
    LDI  R16,$02         ; load R16 <- 0x2
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBS 2,Skip23        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 2,Skip23        ;J flag N clear so branch taken
    .ORG $05AA           
Skip23:
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBC 2,Skip24        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 2,Skip24        ;J flag N set so branch taken
    .ORG $05E9           
Skip24:
; test signed overflow flag
    LDI  R16,$02         ; load R16 <- 0x2
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBS 3,Skip25        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 3,Skip25        ;J flag V clear so branch taken
    .ORG $0628           
Skip25:
    LDI  R16,$80         ; load R16 <- 0x80
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBC 3,Skip26        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 3,Skip26        ;J flag V set so branch taken
    .ORG $0667           
Skip26:
; test corrected signed flag
    LDI  R16,$02         ; load R16 <- 0x2
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBS 4,Skip27        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 4,Skip27        ;J flag S clear so branch taken
    .ORG $06A6           
Skip27:
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBC 4,Skip28        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 4,Skip28        ;J flag S set so branch taken
    .ORG $06E5           
Skip28:
; test half carry flag
    LDI  R16,$02         ; load R16 <- 0x2
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBS 5,Skip29        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 5,Skip29        ;J flag H clear so branch taken
    .ORG $0724           
Skip29:
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$01         ; load R17 <- 0x1
    CP   R16,R17         ; compute R16 - R17
    BRBC 5,Skip30        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 5,Skip30        ;J flag H set so branch taken
    .ORG $0763           
Skip30:
; check compare with intermediate
    LDI  R16,$02         ; load R16 <- 0x2
    CPI  R16,$01         ; compute R16 - 0x1
    BRBS 0,Skip31        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 0,Skip31        ;J flag C clear so branch taken
    .ORG $07A2           
Skip31:
    LDI  R16,$00         ; load R16 <- 0x0
    CPI  R16,$80         ; compute R16 - 0x80
    BRBC 0,Skip32        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 0,Skip32        ;J flag C set so branch taken
    .ORG $07E1           
Skip32:
; test zero flag
    LDI  R16,$02         ; load R16 <- 0x2
    CPI  R16,$01         ; compute R16 - 0x1
    BRBS 1,Skip33        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 1,Skip33        ;J flag Z clear so branch taken
    .ORG $0820           
Skip33:
    LDI  R16,$00         ; load R16 <- 0x0
    CPI  R16,$00         ; compute R16 - 0x0
    BRBC 1,Skip34        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 1,Skip34        ;J flag Z set so branch taken
    .ORG $085F           
Skip34:
; test negative flag
    LDI  R16,$02         ; load R16 <- 0x2
    CPI  R16,$01         ; compute R16 - 0x1
    BRBS 2,Skip35        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 2,Skip35        ;J flag N clear so branch taken
    .ORG $089E           
Skip35:
    LDI  R16,$00         ; load R16 <- 0x0
    CPI  R16,$01         ; compute R16 - 0x1
    BRBC 2,Skip36        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 2,Skip36        ;J flag N set so branch taken
    .ORG $08DD           
Skip36:
; test signed overflow flag
    LDI  R16,$02         ; load R16 <- 0x2
    CPI  R16,$01         ; compute R16 - 0x1
    BRBS 3,Skip37        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 3,Skip37        ;J flag V clear so branch taken
    .ORG $091C           
Skip37:
    LDI  R16,$80         ; load R16 <- 0x80
    CPI  R16,$01         ; compute R16 - 0x1
    BRBC 3,Skip38        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 3,Skip38        ;J flag V set so branch taken
    .ORG $095B           
Skip38:
; test corrected signed flag
    LDI  R16,$02         ; load R16 <- 0x2
    CPI  R16,$01         ; compute R16 - 0x1
    BRBS 4,Skip39        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 4,Skip39        ;J flag S clear so branch taken
    .ORG $099A           
Skip39:
    LDI  R16,$00         ; load R16 <- 0x0
    CPI  R16,$01         ; compute R16 - 0x1
    BRBC 4,Skip40        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 4,Skip40        ;J flag S set so branch taken
    .ORG $09D9           
Skip40:
; test half carry flag
    LDI  R16,$02         ; load R16 <- 0x2
    CPI  R16,$01         ; compute R16 - 0x1
    BRBS 5,Skip41        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBC 5,Skip41        ;J flag H clear so branch taken
    .ORG $0A18           
Skip41:
    LDI  R16,$00         ; load R16 <- 0x0
    CPI  R16,$01         ; compute R16 - 0x1
    BRBC 5,Skip42        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 5,Skip42        ;J flag H set so branch taken
    .ORG $0A57           
Skip42:
; check compare with carry
; clear carry
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    CP   R16,R17         ; compute R16 - R17
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    CPC  R16,R17         ; compute R16 - R17 - C
    BRBC 1,Skip43        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 1,Skip43        ;J flag Z set so branch taken
    .ORG $0A96           
Skip43:
; generate a carry
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$01         ; load R17 <- 0x1
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
; make sure carry is used
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    CPC  R16,R17         ; compute R16 - R17 - C
    BRBC 2,Skip44        ; branch not taken
    NOP                  ; no-op after missed branch
    BRBS 2,Skip44        ;J flag N set so branch taken
    .ORG $0AD5           
Skip44:

;
; check add variants
;
; add without carry
    LDI  R16,$07         ; load R16 <- 0x7
    LDI  R17,$08         ; load R17 <- 0x8
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 0F 1000 check add result
    LDI  R16,$0F         ; load R16 <- 0xf
    LDI  R17,$01         ; load R17 <- 0x1
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 10 1000 check add result
    LDI  R16,$CD         ; load R16 <- 0xcd
    LDI  R17,$1F         ; load R17 <- 0x1f
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W EC 1000 check add result
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
; add with carry
; clear carry
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
    LDI  R16,$25         ; load R16 <- 0x25
    LDI  R17,$59         ; load R17 <- 0x59
    ADC  R16,R17         ; compute R16 <- R16 + R17 + C
    STS  $1000,R16       ;W 7E 1000 check add result (w/ carry)
; generate carry
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$01         ; load R17 <- 0x1
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
; make sure carry is used
    LDI  R16,$25         ; load R16 <- 0x25
    LDI  R17,$59         ; load R17 <- 0x59
    ADC  R16,R17         ; compute R16 <- R16 + R17 + C
    STS  $1000,R16       ;W 7F 1000 check add result (w/ carry)
; add immediate to word
    LDI  R24,$EF         ; load R24 <- 0xef
    LDI  R25,$BE         ; load R25 <- 0xbe
    ADIW R25:R24,$01     ; compute R25:R24 <- R25:R24 + 0x1
    STS  $1000,R24       ;W F0 1000 check add result (lower byte)
    STS  $1000,R25       ;W BE 1000 check add result (upper byte)
    LDI  R24,$CD         ; load R24 <- 0xcd
    LDI  R25,$AB         ; load R25 <- 0xab
    ADIW R25:R24,$3F     ; compute R25:R24 <- R25:R24 + 0x3f
    STS  $1000,R24       ;W 0C 1000 check add result (lower byte)
    STS  $1000,R25       ;W AC 1000 check add result (upper byte)

;
; check sub variants
;
; subtract without carry
    LDI  R16,$38         ; load R16 <- 0x38
    LDI  R17,$63         ; load R17 <- 0x63
    SUB  R16,R17         ; compute R16 <- R16 - R17
    STS  $1000,R16       ;W D5 1000 check sub result
    LDI  R16,$AB         ; load R16 <- 0xab
    LDI  R17,$20         ; load R17 <- 0x20
    SUB  R16,R17         ; compute R16 <- R16 - R17
    STS  $1000,R16       ;W 8B 1000 check sub result
; subtract immediate
    LDI  R16,$1B         ; load R16 <- 0x1b
    SUBI R16,$04         ; compute R16 <- R16 - 0x4
    STS  $1000,R16       ;W 17 1000 check sub result
    LDI  R16,$08         ; load R16 <- 0x8
    SUBI R16,$09         ; compute R16 <- R16 - 0x9
    STS  $1000,R16       ;W FF 1000 check sub result
; subtract with carry
; clear carry
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
    LDI  R16,$80         ; load R16 <- 0x80
    LDI  R17,$01         ; load R17 <- 0x1
    SBC  R16,R17         ; compute R16 <- R16 - R17 - C
    STS  $1000,R16       ;W 7F 1000 check sub result (w/ carry)
; generate a carry
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$01         ; load R17 <- 0x1
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
; make sure carry is used
    LDI  R16,$20         ; load R16 <- 0x20
    LDI  R17,$08         ; load R17 <- 0x8
    SBC  R16,R17         ; compute R16 <- R16 - R17 - C
    STS  $1000,R16       ;W 17 1000 check sub result (w/ carry)
; subtract immediate with carry
; clear carry
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
    LDI  R16,$80         ; load R16 <- 0x80
    SBCI R16,$01         ; compute R16 <- R16 - 0x1 - C
    STS  $1000,R16       ;W 7F 1000 check sub result (w/ carry)
; generate a carry
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$01         ; load R17 <- 0x1
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
; make sure carry is used
    LDI  R16,$20         ; load R16 <- 0x20
    SBCI R16,$08         ; compute R16 <- R16 - 0x8 - C
    STS  $1000,R16       ;W 17 1000 check sub result (w/ carry)
; subtract immediate from word
    LDI  R24,$EF         ; load R24 <- 0xef
    LDI  R25,$BE         ; load R25 <- 0xbe
    SBIW R25:R24,$1E     ; compute R25:R24 <- R25:R24 - 0x1e
    STS  $1000,R24       ;W D1 1000 check sub result (lower byte)
    STS  $1000,R25       ;W BE 1000 check sub result (upper byte)
    LDI  R24,$CD         ; load R24 <- 0xcd
    LDI  R25,$AB         ; load R25 <- 0xab
    SBIW R25:R24,$07     ; compute R25:R24 <- R25:R24 - 0x7
    STS  $1000,R24       ;W C6 1000 check sub result (lower byte)
    STS  $1000,R25       ;W AB 1000 check sub result (upper byte)

;
; check unary arithmetic
;
; check increment
    LDI  R16,$00         ; load R16 <- 0x0
    INC  R16             ; compute R16 <- R16 + 1
    STS  $1000,R16       ;W 01 1000 check increment result
    LDI  R16,$FF         ; load R16 <- 0xff
    INC  R16             ; compute R16 <- R16 + 1
    STS  $1000,R16       ;W 00 1000 check increment result
; check decrement
    LDI  R16,$00         ; load R16 <- 0x0
    DEC  R16             ; compute R16 <- R16 - 1
    STS  $1000,R16       ;W FF 1000 check decrement result
    LDI  R16,$01         ; load R16 <- 0x1
    DEC  R16             ; compute R16 <- R16 - 1
    STS  $1000,R16       ;W 00 1000 check decrement result
; check negation
    LDI  R16,$01         ; load R16 <- 0x1
    NEG  R16             ; compute R16 <- -R16
    STS  $1000,R16       ;W FF 1000 check negation result
    LDI  R16,$80         ; load R16 <- 0x80
    NEG  R16             ; compute R16 <- -R16
    STS  $1000,R16       ;W 80 1000 check negation result

;
; check logic operations
;
; check and
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$AA         ; load R17 <- 0xaa
    AND  R16,R17         ; compute R16 <- R16 & R17
    STS  $1000,R16       ;W AA 1000 check and result
; check and with immediate
    LDI  R16,$FF         ; load R16 <- 0xff
    ANDI R16,$AA         ; compute R16 <- R16 & 0xaa
    STS  $1000,R16       ;W AA 1000 check and result
; check or
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$AA         ; load R17 <- 0xaa
    OR   R16,R17         ; compute R16 <- R16 | R17
    STS  $1000,R16       ;W FF 1000 check or result
; check or with immediate
    LDI  R16,$FF         ; load R16 <- 0xff
    ORI  R16,$AA         ; compute R16 <- R16 & 0xaa
    STS  $1000,R16       ;W FF 1000 check or result
; check exclusive or
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$AA         ; load R17 <- 0xaa
    EOR  R16,R17         ; compute R16 <- R16 ^ R17
    STS  $1000,R16       ;W 55 1000 check eor result
; check complement
    LDI  R16,$AF         ; load R16 <- 0xaf
    COM  R16             ; compute R16 <- ~R16
    STS  $1000,R16       ;W 50 1000 check complement result

;
; check shift operations
;
; check arithmetic right shift
    LDI  R16,$AA         ; load R16 <- 0xaa
    ASR  R16             ; compute R16 <- R16 >> 1
    STS  $1000,R16       ;W D5 1000 check arithmetic shift result
    LDI  R16,$5F         ; load R16 <- 0x5f
    ASR  R16             ; compute R16 <- R16 >> 1
    STS  $1000,R16       ;W 2F 1000 check arithmetic shift result
; check logical right shift
    LDI  R16,$AA         ; load R16 <- 0xaa
    LSR  R16             ; compute R16 <- R16 >>> 1
    STS  $1000,R16       ;W 55 1000 check logical shift result
    LDI  R16,$5F         ; load R16 <- 0x5f
    ASR  R16             ; compute R16 <- R16 >> 1
    STS  $1000,R16       ;W 2F 1000 check arithmetic shift result
; check rotate right through carry
; remove carry
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
    LDI  R16,$AA         ; load R16 <- 0xaa
    ROR  R16             ; compute R16 <- (c, R16 >> 1)
    STS  $1000,R16       ;W 55 1000 check rotate through carry result
; generate a carry
    LDI  R16,$FF         ; load R16 <- 0xff
    LDI  R17,$01         ; load R17 <- 0x1
    ADD  R16,R17         ; compute R16 <- R16 + R17
    STS  $1000,R16       ;W 00 1000 check add result
    LDI  R16,$AA         ; load R16 <- 0xaa
    ROR  R16             ; compute R16 <- (c, R16 >> 1)
    STS  $1000,R16       ;W D5 1000 check rotate through carry result
; check swap nibbles
    LDI  R16,$AB         ; load R16 <- 0xab
    SWAP R16             ; compute R16 <- (R16(3:0), R16(7:4))
    STS  $1000,R16       ;W BA 1000 check swap nibble result

;
; check fibonacci program
;
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$01         ; load R16 <- 0x1
    MOV  R0,R16          ; load R0 <- R16
    MOV  R1,R17          ; load R1 <- R17
    MOV  R2,R0           ; load R2 <- R0
    ADD  R2,R1           ; compute R2 <- R2 + R1
    MOV  R3,R1           ; load R3 <- R1
    ADD  R3,R2           ; compute R3 <- R3 + R2
    MOV  R4,R2           ; load R4 <- R2
    ADD  R4,R3           ; compute R4 <- R4 + R3
    MOV  R5,R3           ; load R5 <- R3
    ADD  R5,R4           ; compute R5 <- R5 + R4
    MOV  R6,R4           ; load R6 <- R4
    ADD  R6,R5           ; compute R6 <- R6 + R5
    MOV  R7,R5           ; load R7 <- R5
    ADD  R7,R6           ; compute R7 <- R7 + R6
    MOV  R8,R6           ; load R8 <- R6
    ADD  R8,R7           ; compute R8 <- R8 + R7
    MOV  R9,R7           ; load R9 <- R7
    ADD  R9,R8           ; compute R9 <- R9 + R8
    MOV  R10,R8          ; load R10 <- R8
    ADD  R10,R9          ; compute R10 <- R10 + R9
    MOV  R11,R9          ; load R11 <- R9
    ADD  R11,R10         ; compute R11 <- R11 + R10
    MOV  R12,R10         ; load R12 <- R10
    ADD  R12,R11         ; compute R12 <- R12 + R11
    MOV  R13,R11         ; load R13 <- R11
    ADD  R13,R12         ; compute R13 <- R13 + R12
    MOV  R14,R12         ; load R14 <- R12
    ADD  R14,R13         ; compute R14 <- R14 + R13
    MOV  R15,R13         ; load R15 <- R13
    ADD  R15,R14         ; compute R15 <- R15 + R14
    MOV  R16,R14         ; load R16 <- R14
    ADD  R16,R15         ; compute R16 <- R16 + R15
    MOV  R17,R15         ; load R17 <- R15
    ADD  R17,R16         ; compute R17 <- R17 + R16
    MOV  R18,R16         ; load R18 <- R16
    ADD  R18,R17         ; compute R18 <- R18 + R17
    MOV  R19,R17         ; load R19 <- R17
    ADD  R19,R18         ; compute R19 <- R19 + R18
    MOV  R20,R18         ; load R20 <- R18
    ADD  R20,R19         ; compute R20 <- R20 + R19
    MOV  R21,R19         ; load R21 <- R19
    ADD  R21,R20         ; compute R21 <- R21 + R20
    MOV  R22,R20         ; load R22 <- R20
    ADD  R22,R21         ; compute R22 <- R22 + R21
    MOV  R23,R21         ; load R23 <- R21
    ADD  R23,R22         ; compute R23 <- R23 + R22
    MOV  R24,R22         ; load R24 <- R22
    ADD  R24,R23         ; compute R24 <- R24 + R23
    MOV  R25,R23         ; load R25 <- R23
    ADD  R25,R24         ; compute R25 <- R25 + R24
    MOV  R26,R24         ; load R26 <- R24
    ADD  R26,R25         ; compute R26 <- R26 + R25
    MOV  R27,R25         ; load R27 <- R25
    ADD  R27,R26         ; compute R27 <- R27 + R26
    MOV  R28,R26         ; load R28 <- R26
    ADD  R28,R27         ; compute R28 <- R28 + R27
    MOV  R29,R27         ; load R29 <- R27
    ADD  R29,R28         ; compute R29 <- R29 + R28
    MOV  R30,R28         ; load R30 <- R28
    ADD  R30,R29         ; compute R30 <- R30 + R29
    MOV  R31,R29         ; load R31 <- R29
    ADD  R31,R30         ; compute R31 <- R31 + R30
    STS  $1000,R0        ;W 00 1000 check R0 = fib(0) % 256
    STS  $1000,R1        ;W 01 1000 check R1 = fib(1) % 256
    STS  $1000,R2        ;W 01 1000 check R2 = fib(2) % 256
    STS  $1000,R3        ;W 02 1000 check R3 = fib(3) % 256
    STS  $1000,R4        ;W 03 1000 check R4 = fib(4) % 256
    STS  $1000,R5        ;W 05 1000 check R5 = fib(5) % 256
    STS  $1000,R6        ;W 08 1000 check R6 = fib(6) % 256
    STS  $1000,R7        ;W 0D 1000 check R7 = fib(7) % 256
    STS  $1000,R8        ;W 15 1000 check R8 = fib(8) % 256
    STS  $1000,R9        ;W 22 1000 check R9 = fib(9) % 256
    STS  $1000,R10       ;W 37 1000 check R10 = fib(10) % 256
    STS  $1000,R11       ;W 59 1000 check R11 = fib(11) % 256
    STS  $1000,R12       ;W 90 1000 check R12 = fib(12) % 256
    STS  $1000,R13       ;W E9 1000 check R13 = fib(13) % 256
    STS  $1000,R14       ;W 79 1000 check R14 = fib(14) % 256
    STS  $1000,R15       ;W 62 1000 check R15 = fib(15) % 256
    STS  $1000,R16       ;W DB 1000 check R16 = fib(16) % 256
    STS  $1000,R17       ;W 3D 1000 check R17 = fib(17) % 256
    STS  $1000,R18       ;W 18 1000 check R18 = fib(18) % 256
    STS  $1000,R19       ;W 55 1000 check R19 = fib(19) % 256
    STS  $1000,R20       ;W 6D 1000 check R20 = fib(20) % 256
    STS  $1000,R21       ;W C2 1000 check R21 = fib(21) % 256
    STS  $1000,R22       ;W 2F 1000 check R22 = fib(22) % 256
    STS  $1000,R23       ;W F1 1000 check R23 = fib(23) % 256
    STS  $1000,R24       ;W 20 1000 check R24 = fib(24) % 256
    STS  $1000,R25       ;W 11 1000 check R25 = fib(25) % 256
    STS  $1000,R26       ;W 31 1000 check R26 = fib(26) % 256
    STS  $1000,R27       ;W 42 1000 check R27 = fib(27) % 256
    STS  $1000,R28       ;W 73 1000 check R28 = fib(28) % 256
    STS  $1000,R29       ;W B5 1000 check R29 = fib(29) % 256
    STS  $1000,R30       ;W 28 1000 check R30 = fib(30) % 256
    STS  $1000,R31       ;W DD 1000 check R31 = fib(31) % 256

;
; check skip instructions
;
; check skip if equal
    LDI  R16,$01         ; load R16 <- 0x1
    LDI  R17,$02         ; load R17 <- 0x2
    CPSE R16,R17         ; compare R16 to R17
    NOP                  ; run this NOP
    LDI  R16,$00         ; load R16 <- 0x0
    LDI  R17,$00         ; load R17 <- 0x0
    CPSE R16,R17         ; compare R16 to R17
    NOP                  ;S skip this NOP
; check skip if bit set
    LDI  R16,$01         ; load R16 <- 0x1
    SBRC R16,$00         ;R16(0) is set
    NOP                  ; run this NOP
    SBRS R16,$00         ;R16(0) is set
    NOP                  ;S skip this NOP
    LDI  R17,$02         ; load R17 <- 0x2
    SBRC R17,$01         ;R17(1) is set
    NOP                  ; run this NOP
    SBRS R17,$01         ;R17(1) is set
    NOP                  ;S skip this NOP
    LDI  R18,$04         ; load R18 <- 0x4
    SBRC R18,$02         ;R18(2) is set
    NOP                  ; run this NOP
    SBRS R18,$02         ;R18(2) is set
    NOP                  ;S skip this NOP
    LDI  R19,$08         ; load R19 <- 0x8
    SBRC R19,$03         ;R19(3) is set
    NOP                  ; run this NOP
    SBRS R19,$03         ;R19(3) is set
    NOP                  ;S skip this NOP
    LDI  R20,$10         ; load R20 <- 0x10
    SBRC R20,$04         ;R20(4) is set
    NOP                  ; run this NOP
    SBRS R20,$04         ;R20(4) is set
    NOP                  ;S skip this NOP
    LDI  R21,$20         ; load R21 <- 0x20
    SBRC R21,$05         ;R21(5) is set
    NOP                  ; run this NOP
    SBRS R21,$05         ;R21(5) is set
    NOP                  ;S skip this NOP
    LDI  R22,$40         ; load R22 <- 0x40
    SBRC R22,$06         ;R22(6) is set
    NOP                  ; run this NOP
    SBRS R22,$06         ;R22(6) is set
    NOP                  ;S skip this NOP
    LDI  R23,$80         ; load R23 <- 0x80
    SBRC R23,$07         ;R23(7) is set
    NOP                  ; run this NOP
    SBRS R23,$07         ;R23(7) is set
    NOP                  ;S skip this NOP
; check skip if bit clear
    LDI  R24,$FE         ; load R24 <- 0xfe
    SBRS R24,$00         ;R24(0) is clear
    NOP                  ; run this NOP
    SBRC R24,$00         ;R24(0) is clear
    NOP                  ;S skip this NOP
    LDI  R25,$FD         ; load R25 <- 0xfd
    SBRS R25,$01         ;R25(1) is clear
    NOP                  ; run this NOP
    SBRC R25,$01         ;R25(1) is clear
    NOP                  ;S skip this NOP
    LDI  R26,$FB         ; load R26 <- 0xfb
    SBRS R26,$02         ;R26(2) is clear
    NOP                  ; run this NOP
    SBRC R26,$02         ;R26(2) is clear
    NOP                  ;S skip this NOP
    LDI  R27,$F7         ; load R27 <- 0xf7
    SBRS R27,$03         ;R27(3) is clear
    NOP                  ; run this NOP
    SBRC R27,$03         ;R27(3) is clear
    NOP                  ;S skip this NOP
    LDI  R28,$EF         ; load R28 <- 0xef
    SBRS R28,$04         ;R28(4) is clear
    NOP                  ; run this NOP
    SBRC R28,$04         ;R28(4) is clear
    NOP                  ;S skip this NOP
    LDI  R29,$DF         ; load R29 <- 0xdf
    SBRS R29,$05         ;R29(5) is clear
    NOP                  ; run this NOP
    SBRC R29,$05         ;R29(5) is clear
    NOP                  ;S skip this NOP
    LDI  R30,$BF         ; load R30 <- 0xbf
    SBRS R30,$06         ;R30(6) is clear
    NOP                  ; run this NOP
    SBRC R30,$06         ;R30(6) is clear
    NOP                  ;S skip this NOP
    LDI  R31,$7F         ; load R31 <- 0x7f
    SBRS R31,$07         ;R31(7) is clear
    NOP                  ; run this NOP
    SBRC R31,$07         ;R31(7) is clear
    NOP                  ;S skip this NOP
