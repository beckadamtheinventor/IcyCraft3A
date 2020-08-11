

include 'include/macros.inc'
format ti executable "IC3CRAFT"

kb_Data:=$F50010


gen_map_temp:=ti.pixelShadow
player_data:=gen_map_temp+64
player_inv_data:=player_data+64
chest_inv_data:=player_inv_data+72
game_flags:=chest_inv_data+72
map_ptr:=game_flags+1
map_len:=map_ptr+3
game_time:=map_len+3
tex_ptrs:=game_time+3
world_file:=tex_ptrs+768
behavior_pack:=world_file+8
texture_pack:=behavior_pack+8
world_chunk_gen_flags:=texture_pack+8
world_data:=world_chunk_gen_flags+32
;next:=world_data+$FFE8

assert world_data < ti.pixelShadow+3578

o_player_x:=0
o_player_y:=3
o_player_z:=6
o_player_inv:=64

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
	call ti._memclear
end_program


draw_map_tiles:
	push ix
	ld ix,player_data
	ld b,(ix+o_player_y)
	ld c,8
	mlt bc
	ld a,(ix+o_player_x)
	call ti.AddHLAndA
	ld hl,world_data
	add hl,bc
	ld c,(ix+o_player_y)
	ld b,8
	mlt bc
	add hl,bc
	ld a,(ix+o_player_x)
	call ti.AddHLAndA
	pop ix
	ret
	
.sub4div8and7:
	dec hl
	dec hl
	dec hl
	dec hl ;X-4
	rr h
	rr l
	rr h
	rr l
	rr h
	rr l
	ld a,l
	and a,7 ;get chunk coordinate
	ret

;input b=x,c=y
get_map:
	ld hl,world_data
	
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
	ld hl,Modes.R
	push hl
	ld hl,Files.PackFile
	push hl
	call ti_Open
	pop bc
	pop bc
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
	ld bc,24
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
	or a,a
	sbc hl,bc
	add hl,bc
	ex hl,de
	jr c,.tex_loop
	xor a,a
	ld (draw_background),a  ; textures are loaded
	ld (main),a
	ret

load_behaviours:
	ret

; world saving/loading

load_world_layer:
	ld hl,world_chunk_gen_flags ;clear old data
	ld bc,65536+32
	call ti._memclear
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
	ld hl,65536+32
	push hl
	ld hl,.worldLength
	push hl
	ld hl,$D52C00
	push hl
	ld hl,world_chunk_gen_flags
	push hl
	call _zx7_Compress
	pop bc,bc,bc,bc
	ld hl,Modes.W
	push hl
	ld hl,Files.TempFile
	push hl
	call ti_Open
	pop bc,bc
	or a,a
	ret z
	ld c,a
	push bc
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
	ld hl,Files.TempFile-1
	call ti.Mov9ToOP1
	jp _Arc_Unarc

; return nz if found, z if not found. HL = chunk
get_chunk:
	ld hl,world_data
.loop:
	ld a,0
.x:=$-1
	cp a,(hl)
	inc hl
	jr nz,.next
	ld a,0
.y:=$-1
	cp a,(hl)
	jr z,.found
.next:
	inc hl
	ld bc,0
world_end_ptr:=$-3
	or a,a
	sbc hl,bc
	add hl,bc
	jr c,.loop
	xor a,a
	sbc hl,hl
	ret
.found:
	dec hl
	xor a,a
	inc a
	ret

; world generation

generate_world_layer:
	ret

; X,Y must already be set in gen_map_temp
; chunk_seed = world_seed bitxor (X + Y*256) * 8
generate_chunk:
	xor a,a
	ld (gen_map_temp+2),a
	ld hl,(gen_map_temp)
	add hl,hl
	add hl,hl
	add hl,hl               ; multiply by 8
	ex hl,de
	ld hl,chunk_seed+1
	ld (hl),de
	dec hl
	ld de,world_seed
	ld a,(gen_map_temp+1)
	rlca
	rlca
	rlca
	and a,7
	ld (hl),a
	ld b,4
.xorloop:
	ld a,(de)
	xor a,(hl)
	ld (hl),a
	inc hl
	inc de
	djnz .xorloop
	
	ld de,(world_end_ptr)
	ld bc,66
	ldir
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
include 'compressor.asm'
include 'data.asm'
