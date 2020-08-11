
MAX_OFFSET := 2176
MAX_LEN    := 65536

;zx7_Compress(void *dest,void *src,int *len,int src_len);
_zx7_Compress:
	ld hl,-9
	call ti._frameset
	xor a,a
	sbc hl,hl
	ld (input_index),hl
	ld (output_index),hl
	inc a
	ld (bit_mask),a
	ld hl,(ix+6)
	ld a,(hl)
	call _write_byte
.loop:
	call _get_match

	ld a,(best_cost)
	or a,a
	jr nz,.check_cost

.literal:
	xor a,a
	call _write_bit
	ld hl,0
input_index:=$-3
	ld a,(hl)
	ex hl,de
	call _write_byte
	ex hl,de
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
	ld hl,(input_index)
	ld bc,(ix-6)
	add hl,bc
	ld (input_index),hl

.next:
	ld bc,(ix+12)
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
	ld hl,(ix+9)
	ld (hl),de ;*len = input_index

	ld sp,ix
	pop ix
	ret

;output (ix-6) = match_len, (ix-9) = match_offset.
;uses (ix-3) as scrap
_get_match:
	xor a,a
	sbc hl,hl
	ld (best_cost),a
	ld (ix-6),hl
	ld (ix-9),hl
	ld hl,MAX_OFFSET
	ld bc,(input_index)
	or a,a
	sbc hl,bc
	jr c,.index_lt_maxoffset
	ld bc,MAX_OFFSET
.index_lt_maxoffset:
	;bc = (input_index>MAX_OFFSET?MAX_OFFSET:input_index)
	ld hl,(ix+6)
	add hl,bc
	ld (ix-3),hl
	ld a,(hl)
	dec hl
	cpdr
	ret nz
	push bc
	ld de,(ix+6)
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
	pop de
	push hl
	ld hl,(ix-3) ;hl = &src[input_index]
	or a,a
	sbc hl,de ;hl = &src[input_index] - match_ptr --> match_offset
	pop de
	push hl ;match_offset
	push de ;match_len
	call _count_bits
	ld (current_cost),a
	pop hl ;match_len
	push hl
	ld a,0
best_cost:=$-1
	or a,a
	jr z,.new_cost
	call ti._imul_b ;match_len*best_cost
	ld bc,0
current_cost:=$-3
	or a,a
	sbc hl,bc
	pop hl ;match_len
	pop de ;match_offset
	jp nc,_get_match
.new_cost:
	ld a,c
	ld (best_cost),a
	ld (ix-6),hl ;match_len
	ld (ix-9),de ;match_offset
	jp _get_match
	

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
	jr nc,.noincrement
	ld hl,(output_index)
	inc hl
	ld (hl),0
	ld (bit_index),hl
	inc hl
	ld (output_index),hl
.noincrement:
	ld (bit_mask),a
	ld d,a
	ld a,e
	or a,a
	ret z
	ld a,d
	ld hl,(ix+3)
	ld de,0
bit_index:=$-3
	add hl,de
	or a,(hl)
	ld (hl),a
	ret

;input a = byte to write
_write_byte:
	ld hl,(ix+3)
	ld bc,0
output_index:=$-3
	add hl,bc
	ld (hl),a
	inc bc
	ld (output_index),bc
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
