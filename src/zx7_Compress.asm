
MAX_OFFSET := 2176
MAX_LEN    := 65536

;zx7_Compress(void *dest,void *src,int *len,int src_len);
_zx7_Compress:
	ld hl,-15
	call ti._frameset
	xor a,a
	sbc hl,hl
	ld (output_index),hl
	inc hl
	ld (input_index),hl
	inc a
	ld (bit_mask),a
	ld hl,(ix+9)
	ld a,(hl)
	call _write_byte
.loop:
	call _get_match

	ld a,(best_cost)
	cp a,$FF
	jr nz,.check_cost

.literal:
	xor a,a
	call _write_bit
	ld hl,0
input_index:=$-3
	ld bc,(ix+9)
	add hl,bc
	inc bc
	ld (input_index),bc
	ld a,(hl)
	call _write_byte
	jq .next

.check_cost:
	ld hl,(ix-6)
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,(ix-6)
	add hl,de
	ld de,0
	ld e,a
	or a,a
	sbc hl,de
	jr c,.literal

	ld a,1
	call _write_bit
	
	ld hl,(ix-6)
	dec hl
	ld b,24
	scf
.bsf:
	dec b
	adc hl,hl
	jr nc,.bsf
.len:
	xor a,a
	push hl
	call _write_bit
	pop hl
	djnz .len
	add hl,hl
.val:
	sbc a,a
	push hl
	call _write_bit
	pop hl
	or a,a
	adc hl,hl
	jr nz,.val

	ld hl,(ix-9)
	ld de,129
	or a,a
	sbc hl,de
	jr nc,.over128
	add hl,de
	ld a,l
	call _write_byte
	jr .advancepattern
.over128:
	inc hl
	ld a,l
	set 7,a
	call _write_byte

.advancepattern:
	ld hl,(ix-9)
	ld de,-128
	add hl,de
	rl l
	rl h
	ld c,h
	ld b,4
.patternlenloop:
	ld a,c
	and a,1
	call _write_bit
	rr c
	djnz .patternlenloop

	ld hl,(input_index)
	ld bc,(ix-6)
	add hl,bc
	ld (input_index),hl

.next:
	ld hl,(input_index)
	ld bc,(ix+15)
	or a,a
	sbc hl,bc
	jq c,.loop

;write ending sequence
	ld a,1
	call _write_bit

	ld b,16 ;write 16 zero bits
.end_loop:
	xor a,a
	call _write_bit
	djnz .end_loop

	ld a,1
	call _write_bit

	ld de,(input_index)
	ld hl,(ix+12)
	ld (hl),de ;*len = input_index

	ld sp,ix
	pop ix
	ret

;output (ix-6) = match_len, (ix-9) = match_offset.
;uses (ix-3), (ix-12), and (ix-15) as scrap
_get_match:
	ld a,$FF
	ld (best_cost),a
	or a,a
	sbc hl,hl
	ld (ix-6),hl
	ld (ix-9),hl
	ld hl,MAX_OFFSET
	ld bc,(input_index)
	or a,a
	sbc hl,bc
	jr nc,.index_lt_maxoffset
	ld bc,MAX_OFFSET
.index_lt_maxoffset:
	;bc = (input_index>MAX_OFFSET?MAX_OFFSET:input_index)
	ld hl,(ix+9)
	ld de,(input_index)
	add hl,de
	ld (ix-15),hl
	ld (ix-3),hl
	ld (ix-12),bc
	jr .outer_loop_entry
.outer_loop:
	ld hl,(ix-3)
	ld bc,(ix-12)
.outer_loop_entry:
	ld a,(hl)
	dec hl
	cpdr
	ret nz
	ret po
	ld (ix-3),hl
	ld (ix-12),bc
	ld de,(ix-15)
	ld bc,MAX_LEN
.loop:
	ld a,(de)
	inc de
	cpi
	jr nz,.end_of_pattern
	jp pe,.loop
.end_of_pattern:
	ld hl,MAX_LEN
	or a,a
	sbc hl,bc ;hl = match_len
	push hl
	ld hl,(ix-15) ;hl = &src[input_index]
	ld de,(ix-3) ;de = match_ptr
	or a,a
	sbc hl,de ;hl = &src[input_index] - match_ptr --> match_offset
	pop de
	push hl ;match_offset
	push de ;match_len
	call _count_bits
	ld (current_cost),a
	pop hl ;match_len
	push hl
	ld a,$FF
best_cost:=$-1
	call ti._imul_b ;match_len*best_cost
	ld bc,0
current_cost:=$-3
	or a,a
	sbc hl,bc
	pop hl ;match_len
	pop de ;match_offset
	jp c,.outer_loop
.new_cost:
	ld (best_cost),bc
	ld (ix-6),hl ;match_len
	ld (ix-9),de ;match_offset
	jp .outer_loop

;input hl = offset, de = len
;output a = cost
_count_bits:
	ld a,10
	ld bc,128
	or a,a
	sbc hl,bc
	jr c,.offsetunder128
	add a,4
.offsetunder128:
	ex hl,de
	ld de,2
.gammaloop:
	rr h
	rr l
	or a,a
	sbc hl,de
	ret c
	add hl,de
	inc a
	inc a
	jr .gammaloop


;input a = bit to write
_write_bit:
	ld e,a
	ld a,1
bit_mask:=$-1
	rrca
	ld (bit_mask),a
	ld d,a
	jr nc,.noincrement
	push de
	ld de,(output_index)
	ld hl,(ix+6)
	add hl,de
	ld (hl),0
	ld (bit_index),de
	inc de
	ld (output_index),de
	pop de
.noincrement:
	ld a,e
	or a,a
	ret z
	ld a,d
	ld hl,(ix+6)
	ld de,0
bit_index:=$-3
	add hl,de
	or a,(hl)
	ld (hl),a
	ret

;input a = byte to write
_write_byte:
	ld hl,(ix+6)
	ld bc,0
output_index:=$-3
	add hl,bc
	ld (hl),a
	inc bc
	ld (output_index),bc
	ret


waitkey:
	call ti.GetCSC
	or a,a
	jr z,waitkey
	push af
.loop:
	call ti.GetCSC
	or a,a
	jr nz,.loop
	pop af
	ret

;try this if you can lol
;macro out_bit_init start
  ;ld iy,start
  ;ld c,1
;end macro
;macro out_bit_next
  ;local skip
  ;rl c
  ;jr nc,skip
  ;ld (iy),c
  ;inc iy
  ;ld c,1
;skip:
;end macro
