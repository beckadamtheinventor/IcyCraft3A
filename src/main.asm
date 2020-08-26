

include 'include/macros.inc'
format ti executable "IC3CRAFT"

kb_Data:=$F50010




clibs_program

main_init:

	or a,a
	sbc hl,hl
	add hl,sp
	ld (ErrorSP),hl	

	call ti.HomeUp
	call ti.RunIndicOff
	call gfx_Begin
	call ti_CloseAll
	ld l,1
	push hl
	call gfx_SetDraw
	ld l,0
	ex (sp),hl
	call gfx_SetTextTransparentColor
	call gfx_SetTransparentColor
	call gfx_SetTextBGColor
	ld l,$FF
	ex (sp),hl
	call gfx_SetTextFGColor
	pop hl

	call load_packs
	call load_textures

menu:
menu_draw:
	call gfx_ZeroScreen
	call draw_menu
	call gfx_SwapDraw
menu_loop:
	ld iy,player_data
	call kb_Scan
	ld hl,kb_Data+2
; Group 1
	ld a,(hl)
	bit 5,a
	jr nz,.select
; Group 2
	inc hl
	inc hl
	ld a,(hl)
	bit 7,a
	jr nz,.select2
; Group 3
	inc hl
	inc hl
	ld a,(hl)
; Group 4
	inc hl
	inc hl
	ld a,(hl)
; Group 5
	inc hl
	inc hl
	ld a,(hl)
; Group 6
	inc hl
	inc hl
	ld a,(hl)
	bit 6,a
	jr nz,.exit
	bit 0,a
	jr nz,.select
; Group 7
	inc hl
	inc hl
	ld a,(hl)
	bit 0,a
	jr z,.notdown

.notdown:
	bit 3,a
	jr z,.notup
	
.notup:
	bit 2,a
	jr z,.notright
	
.notright:
	bit 1,a
	jr z,.notleft
	
.notleft:
	and a,$F
	jp nz,menu_draw
	jp menu_loop
.select:
	call main
	jp menu_draw
.select2:
	
	jp menu_draw
.clear:
	
	jp menu_draw
.exit:
	jp full_exit

main:
	ret                ; this will be smc'd into a NOP once data is properly loaded
	call load_player
	call load_world_layer
main_loop:
	call gfx_ZeroScreen
	call draw_map_tiles
	or a,a
	sbc hl,hl
	push hl
	push hl
	ld hl,Strings.X
	push hl
	call gfx_PrintStringXY
	pop bc
	pop bc
	pop bc
	ld ix,player_data
	ld bc,(ix+o_player_x)
	ld de,(ix+o_player_y)
	or a,a
	sbc hl,hl
	push hl
	push de
	push hl
	push bc
	call gfx_PrintInt
	pop bc
	ld hl,Strings.Y
	ex (sp),hl
	call gfx_PrintString
	pop hl
	call gfx_PrintInt
	pop bc
	pop bc
	call gfx_SwapDraw
	ld iy,player_data
.keywait:
	call kb_Scan
	ld hl,kb_Data+2
; Group 1
	ld a,(hl)
; Group 2
	inc hl
	inc hl
	ld a,(hl)
; Group 3
	inc hl
	inc hl
	ld a,(hl)
; Group 4
	inc hl
	inc hl
	ld a,(hl)
; Group 5
	inc hl
	inc hl
	ld a,(hl)
; Group 6
	inc hl
	inc hl
	ld a,(hl)
	bit 6,a
	jr nz,.exit
; Group 7
	inc hl
	inc hl
	ld bc,(iy+o_player_y)
	ld de,(iy+o_player_x)
	ld a,(hl)
	bit 0,a
	jr z,.notdown
	inc bc
.notdown:
	bit 3,a
	jr z,.notup
	dec bc
.notup:
	bit 2,a
	jr z,.notright
	inc de
.notright:
	bit 1,a
	jr z,.notleft
	dec de
.notleft:
	ld (iy+o_player_y),bc
	ld (iy+o_player_x),de
	jp main_loop
.exit:
	call save_player
	call save_world_layer
	jp wait_key_unpress

error_draw:
	call gfx_ZeroScreen
	ld bc,0
	push bc
	push bc
	ld bc,Errors.Error
	push bc
	call gfx_PrintStringXY
	pop bc
	pop hl
	pop bc
	ld c,10
	push bc
	push hl
	ld bc,Errors.UnknownError
ErrorCode:=$-3
	push bc
	call gfx_PrintStringXY
	pop bc
	pop bc
	pop bc
	jp gfx_SwapDraw

error_to_menu:
	ld sp,(ErrorSP)
	call error_draw
	call WaitKey
	jp menu

error_exit:
	ld sp,0
ErrorSP:=$-3
	call error_draw
	call WaitKey
full_exit:
	call ti_CloseAll
	call gfx_End
	
	ld hl,ti.pixelShadow
	ld bc,69090
	call memclear
	ld hl,ti.cursorImage
	ld bc,1024
	call memclear
	call ti.ClrTxtShd
	ld hl,ti.textShadow
	ld de,ti.cmdShadow
	ld bc,260
	ldir
end_program


memclear:
	push hl
	pop de
	inc de
	ld (hl),0
	ldir
	ret


draw_map_tiles:
	push ix
	ld ix,player_data
	ld hl,world_data
	ld bc,0
	ld a,(ix+o_player_x)
	sub a,7
	ld c,a
	ld a,(ix+o_player_y)
	sub a,7
	ld b,a
	add hl,bc
	ex hl,de
	ld bc,$0F0F
	xor a,a
	sbc hl,hl
.loop:
	call draw_tile_2x ;expected to not destroy hl, de, bc, and a.
	inc de
	push bc
	ld bc,16
	add hl,bc
	pop bc
map_tile_draw_function:=$-3
	djnz .loop
	ld b,$0F
	or a,a
	sbc hl,hl
	add a,16
	dec c
	jr nz,.loop

	pop ix
	ret


draw_tile_2x:
	push bc
	push de
	ld c,2
	push bc
	push bc
	ld c,a
	push bc
	push hl
	ld a,(de)
	cp a,$FF
max_loaded_tile:=$-1
	jr nc,.dontdraw
	or a,a
	sbc hl,hl
	ld l,a
	push hl
	pop bc
	add hl,hl
	add hl,bc
	ld bc,tex_ptrs
	add hl,bc
	ld hl,(hl)
	push hl
	call gfx_ScaledSprite_NoClip
	pop bc
.dontdraw:
	pop hl
	pop bc
	ld a,c
	pop bc
	pop bc
	pop de
	pop bc
	ret

draw_menu:
	call draw_background
	ld l,3
	push hl
	dec l
	push hl
	call gfx_SetTextScale
	pop hl
	ld l,$1E
	ex (sp),hl
	call gfx_SetTextFGColor
	ld hl,20
	ex (sp),hl
	ld hl,100
	push hl
	ld hl,Strings.MenuTitle
	push hl
	call gfx_PrintStringXY
	pop hl
	pop hl
	pop hl
	ld l,1
	push hl
	push hl
	call gfx_SetTextScale
	pop hl
	ld l,$FF
	ex (sp),hl
	call gfx_SetTextFGColor
	pop hl
	ret

draw_background:
	ret         ; will be smc'd once textures are loaded to avoid a crash if the texture pack is not found
	ld hl,2     ; Scale factor
	push hl
	push hl
	ld l,0      ; X=0, Y=0
	push hl
	pop ix
	add ix,sp   ; (ix-6) --> X, (ix-3) --> Y
	push hl
	push hl
	ld hl,(tex_ptrs+9) ; texture 3, used for the background
	push hl
	ld a,15
	ld (.sva2),a
	ld a,20
.loop:
	ld (.sva),a
	call gfx_ScaledTransparentSprite_NoClip
	ld bc,16
	ld hl,(ix-6)
	add hl,bc
	ld (ix-6),hl
	ld a,0
.sva:=$-1
	dec a
	jr nz,.loop
	or a,a
	sbc hl,hl
	ld (ix-6),hl
	ld hl,(ix-3)
	add hl,bc
	ld (ix-3),hl
	ld a,0
.sva2:=$-1
	dec a
	ld (.sva2),a
	ld a,20
	jr nz,.loop
.exit:
	lea hl,ix+6
	ld sp,hl
	ret

; configs
load_packs:
	c_call ti_Open, Files.PackFile, Modes.R
	or a,a
	jr z,.default
	ld l,a
	push hl
	call ti_GetDataPtr
	ld de,world_file
	ld bc,24
	ldir
	call ti_Close
	pop hl
	ret
.default:
	ld hl,Files.DefaultWorldFile
	ld de,world_file
	ld bc,32
	ldir
	ret

load_textures:
	ld hl,Modes.R
	push hl
	ld hl,texture_pack
	push hl
	call ti_Open
	pop bc,bc
	or a,a
	jr nz,.found
	ld hl,Errors.TexturePackFile
	ld (ErrorCode),hl
	jp error_to_menu
.found:
	ld l,a
	push hl
	call ti_GetSize
	ex (sp),hl
	push hl
	call ti_GetDataPtr
	ex (sp),hl
	push hl
	call ti_Close
	pop bc,de,hl
	add hl,de
	ld (.filemax),hl
	xor a,a
	ld hl,tex_ptrs
.tex_loop:
	ld (hl),de
	inc hl
	inc hl
	inc hl
	ex hl,de
	ld bc,66
	add hl,bc
	ld bc,0
.filemax:=$-3
	inc a
	or a,a
	sbc hl,bc
	add hl,bc
	ex hl,de
	jr c,.tex_loop
	ld (max_loaded_tile),a
	xor a,a
	ld (draw_background),a  ; textures are loaded
	ld (main),a
	ret

load_behaviours:
	c_call ti_Open, behavior_pack,Modes.R
	or a,a
	ret z
	ld c,a
	push bc
	call ti_GetDataPtr
	ld (behaviours_ptr),hl
	call ti_Close
	pop bc
	xor a,a
	ld (generate_world_layer),a ;behaviours are loaded
	ret


; save/load player data

save_player:
	c_call ti_Open, player_file,Modes.W
	or a,a
	ret z
	ld c,a
	push bc
	c_call ti_Write, player_data, 64+72, 1
	call ti_Close
	pop bc
	ld hl,player_file
	jq ArcAppvarHL

load_player:
	c_call ti_Open, player_file,Modes.R
	or a,a
	jr z,.default
	ld c,a
	push bc
	c_call ti_Read, player_data, 64+72, 1
	call ti_Close
	pop bc
	ret
.default:
	ld hl,player_data
	ld bc,64+72
	jq memclear


; world saving/loading

load_world_layer:
	ld hl,world_chunk_gen_flags ;clear old data
	ld bc,65536+32
	call memclear
	ld hl,Modes.R
	push hl
	ld hl,world_file
	push hl
	call ti_Open
	pop bc
	pop bc
	or a,a
	jp z,generate_world_layer
	ld l,a
	push hl
	call ti_GetDataPtr
	ld bc,8 ;skip the header for now
	add hl,bc
	ex (sp),hl
	push hl
	call ti_Close
	pop bc
	ld hl,world_chunk_gen_flags
	push hl
	call _zx7_Decompress
	pop bc,bc
	ret

save_world_layer:
	ld hl,$D40000 ;force drawing from the LCD so we can use the back buffer as RAM
	ld ($E30010),hl
	call gfx_ZeroScreen
	c_call gfx_PrintStringXY, str_SavingWorld,1,1
	ld hl,65536+32 ;source length
	push hl
	ld hl,.worldLength ;pointer to destination length
	push hl
	ld hl,world_chunk_gen_flags ;source
	push hl
	ld hl,$D52C00 ;destination
	push hl
	call _zx7_Compress
	pop bc,bc,bc,bc
	ld hl,Modes.W
	push hl
	ld hl,world_file
	push hl
	call ti_Open
	pop bc,bc
	or a,a
	ret z
	ld c,a
	push bc
	c_call ti_Write,str_WorldVersionNumber,8,1
	ld hl,0
.worldLength:=$-3
	push hl
	ld hl,1
	push hl
	ld hl,$D52C00
	push hl
	call ti_Write
	pop bc,bc,bc
	call ti_Close
	pop bc
	ld hl,world_file

ArcAppvarHL:
	ld de,ti.OP1+1
	call ti.Mov8b
ArcAppvarOP1:
	ld a,ti.AppVarObj
	ld (ti.OP1),a
	jp _Arc_Unarc

; world generation

;generates chunks around the player
;no arguments.
generate_world_layer:
	nop                ;will be smc'd away once behaviours are loaded
	ld hl,world_data
	ld (hl),$02
	ld bc,65536
	push hl
	pop de
	inc de
	ldir
	ld a,(o_player_x)
	and a,$F0
	ld c,a
	ld a,(o_player_y)
	and a,$F0
	ld b,a

	ld hl,world_data
	add hl,bc
	push hl
	push bc
	pop hl
	add hl,hl
	ld a,c
	and a,7
	add a,a
	add a,a
	add a,a
	add a,$46
	ld (.bitsmc),a
	add a,$80
	ld (.bitsmc2),a
	ld a,h ;offset of byte to check
	ld hl,world_chunk_gen_flags
	call ti.AddHLAndA
	bit 0,(hl)
.bitsmc:=$-1
	pop iy
	ret nz ;chunk is already generated
	set 0,(hl)
.bitsmc2:=$-1

	call set_chunk_seed
	call random_number
	push hl
	ld a,l
	call .place_tree
	pop hl
	ld a,h
	cp a,l
	ret z
.place_tree:
	ld bc,0
	ld c,a
	and a,$F
	ld b,a
	ld a,c
	rrca
	rrca
	rrca
	rrca
	and a,$F
	ld c,a
	lea hl,iy
	add hl,bc
	ld a,28
	dec hl
	ld (hl),a
	inc hl
	ld (hl),13
	inc hl
	ld (hl),a
	ld bc,254
	add hl,bc
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld bc,-515
	add hl,bc
	ld (hl),a
	inc hl
	ld (hl),a
	inc hl
	ld (hl),a
	ret


gen_chunk:
	call set_chunk_seed
	call get_biome_layout
	ex hl,de
	ld hl,0
chunk_ptr:=$-3
;multiply coordinates by 16
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,world_data
	add hl,bc
;fill chunk with blocks
	push hl
	ld hl,(chunk_seed)
	ld a,(chunk_seed+3)
	call seed_random
	pop hl
	ld bc,$1010
	ex hl,de
.fillchunk:
	push bc
	call random_number
	ld a,l
	and a,$F
	ld hl,0
biome_blocks:=$-3
	call ti.AddHLAndA
	ldi
	pop bc
	djnz .fillchunk
	ld b,$10
	dec c
	jr nz,.fillchunk

;place some features
	call random_number
	ld a,l
	or a,a
	jr nz,.putfeatureloop
	ld a,4
.putfeatureloop:
	push af
	call .put_feature
	pop af
	dec a
	jr nz,.putfeatureloop


.put_feature:
	push af
	call random_number
	ld iy,(behaviours_ptr)
	ld a,l
	ld c,(iy+bvr_jt_feature_gen_l)
	call _bremu
	ld hl,(iy+bvr_jt_feature_gen)
	call ti.AddHLAndA
	ld a,(hl)
	or a,a
	jr z,.one_block
	ld b,a
	and a,$F
	ld (.feat_x),a
	ld c,a
	xor a,a
	sub a,c
	ld (.feat_dx),a
	ld a,b
	rlca
	rlca
	rlca
	rlca
	and a,$F
	ld b,a
	inc hl
	pop af
	push de
	push bc
	call random_number
	mlt hl
	ld a,l
	pop bc
	push bc
	rlc b
	rlc b
	rlc b
	rlc b
	sub a,b
	sub a,c
	pop bc
	ld hl,(chunk_ptr)
	call index_chunk_HL_A
	ex hl,de
	pop hl
	inc hl ;skip feature size byte
	inc hl ;skip feature extra byte (unused)
.featloop:
	push bc
	ld bc,0
.feat_x:=$-3
	ldir
	ld bc,0
.feat_dx:=$-3
	ex hl,de
	add hl,bc
	ex hl,de
	pop bc
	djnz .featloop
	ret
.one_block:
	call random_number
	mlt hl
	ld b,a
	ld a,l
	ld hl,(chunk_ptr)
	call index_chunk_HL_A
	ld (hl),b
	ret

index_chunk_HL_A:
	push bc
	push hl
	ld bc,0
	ld c,a
	and a,$F0
	ld l,a
	ld a,c
	and a,$0F
	ld c,a
	ld h,16
	mlt hl
	add hl,bc
	pop bc
	add hl,bc
	pop bc
	ret

seed_random:
	ld	(__state), hl
	ld	(__state+3),a
	ld	b, 12
__setstateloop:
	inc	hl
	ld	(hl), b
	djnz	__setstateloop
	ret

random_number:
	ld	iy, __state
	ld	hl, (iy+0*4+0)
	push	hl
	ld	hl, (iy+0*4+2)
	push	hl
	lea	hl, iy+1*4
	lea	de, iy+0*4
	ld	bc, 3*4
	ldir
	pop	bc
	pop	de
	ld	h, d
	ld	l, e
	add	hl, hl
	add	hl, hl
	add	hl, hl
	ld	a, b
	xor	a, h
	ld	h, a
	xor	a, (iy+3*4+2)
	ld	(iy+3*4+3), a
	ld	b, a
	ld	a, c
	xor	a, l
	ld	l, a
	xor	a, (iy+3*4+1)
	ld	(iy+3*4+2), a
	xor	a, a
	add.s	hl, hl
	adc	a, a
	add.s	hl, hl
	adc	a, a
	add.s	hl, hl
	adc	a, a
	xor	a, d
	xor	a, (iy+3*4+0)
	ld	(iy+3*4+1), a
	ld	a, e
	xor	a, h
	ld	(iy+3*4+0), a
	ld	hl, (iy+3*4)
	ld	a, b
	ld	de, (iy+2*4)
	add	hl, de
	ld	e, (iy+2*4+3)
	ret

__state:
	db	10h, 0Fh, 0Eh, 0Dh
	db	0Ch, 0Bh, 0Ah, 09h
	db	08h, 07h, 06h, 05h
	db	04h, 03h, 02h, 01h


;return hl = 16 block list of biome base blocks
get_biome_layout:
	ld a,(chunk_seed+1)
	ld c,(iy+bvr_jt_biome_l)
	call _bremu
	ld bc,0
	ld c,a
	ld hl,(iy+bvr_jt_biome_index)
	add hl,bc
	add hl,bc
	add hl,bc
	ld hl,(hl)
	lea bc,iy
	add hl,bc
	ret


;set chunk seed --> (world_seed bitxor (X + Y<<8)<<3) - 11
;input l=x,h=y
set_chunk_seed:
	add hl,hl
	add hl,hl
	add hl,hl
	ld (chunk_seed),hl
	ld de,world_seed
	ld hl,chunk_seed
	ld b,3
.xorloop:
	ld a,(de)
	xor a,(hl)
	ld (hl),a
	inc hl
	inc de
	djnz .xorloop
	ld hl,chunk_seed
	ld a,(hl)
	sub a,11
	ld (hl),a
	ret nc
	inc hl
	ld b,3
.subtractloop:
	ld c,(hl)
	ld a,c
	dec c
	ld (hl),c
	or a,a
	ret nz
	inc hl
	djnz .subtractloop
	ret

get_biome:
	
	ret

world_seed:      ;32-bit world seed
	db 4 dup 0
chunk_seed:      ;32-bit chunk seed
	db 4 dup 0


; utility

HashString:
	ex hl,de
	ld hl,.temp
	xor a,a
.clear:
	ld b,4
	ld (hl),a
	djnz .clear
.outer:
	ld hl,.temp
	ld b,4
.inner:
	ld a,(de)
	inc de
	or a,a
	jr z,.exit
	add a,(hl)
	ld (hl),a
	inc hl
	jr nc,.cont
	inc (hl)
.cont:
	djnz .inner
	jr .outer
.exit:
	ld hl,.temp
	ret
.temp:           ;32-bit hash of string
	db 4 dup 0

WaitKeyFull:
	call WaitKey

wait_key_unpress:
	call kb_AnyKey
	jr nz,wait_key_unpress
	ret

WaitKey:
	call kb_AnyKey
	jr z,WaitKey
	ret

memset:
	push hl
	pop de
	inc de
	xor a,a
	ld (hl),a
	ldir
	ret

GetKey:
	call kb_Scan
	ld hl,kb_Data+2
.loop:
	cp a,(hl)
	jr nz,.loop
	ld b,7
	ld c,49
.scanloop:
	ld a,(hl)
	or a,a
	jr nz,.keyispressed
	inc hl
	inc hl
	ld a,c
	sub a,8
	ld c,a
	djnz .scanloop
	xor a,a
	ret
.keyispressed:
	ld b,8
.keybitloop:
	rrca
	jr c,.this
	inc c
	djnz .keybitloop
.this:
	ld a,c
	ret

include 'arc_unarc.asm'
include 'zx7_Decompress.asm'
;include 'zx7_Compress.asm'
include 'compressor.c.src'
include 'bremu.asm'
include 'data.asm'
