

include 'include/ez80.inc'
include 'include/tiformat.inc'
include 'include/ti84pceg.inc'
format ti executable "DEMO"

kb_Data:=$F50010
tiflags:=$D00080

gen_map_temp:=ti.pixelShadow
player_inv_data:=gen_map_temp+64
player_data:=player_inv_data+72
chest_inv_data:=player_data+64
game_flags:=chest_inv_data+72
map_ptr:=game_flags+1
map_len:=map_ptr+3
game_time:=map_len+3
world_data:=game_time+3
;next:=world_data+$FFE8

macro voidptr adress, amount
	ld hl,adress
	ld bc,amount-1
	push hl
	pop de
	inc de
	xor a,a
	ld (hl),a
	ldir
end macro



init:
	call libload_load
	jr z,gfxInit
	call ti.HomeUp
	ld hl,.needlibload
	call ti.PutS
	xor a,a
	ld (ti.curCol),a
	inc a
	ld (ti.curRow),a
	call ti.PutS
	jr GetCSC
.needlibload:
	db "Need libLoad",0
	db "tiny.cc/clibs",0
GetCSC:
	call ti.GetCSC
	or a,a
	jr z,GetCSC
	ret
gfxInit:
	call ti.HomeUp
	call ti.RunIndicOff
	call gfx_Begin
	call ti_CloseAll
	ld l,1
	push hl
	call gfx_SetDraw
	pop hl
	ld l,0
	push hl
	call gfx_SetTextTransparentColor
	call gfx_SetTransparentColor
	call gfx_SetTextBGColor
	pop hl
	ld l,$FF
	push hl
	call gfx_SetTextFGColor
	pop hl

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
	jr nz,.clear
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
	ld iy,player_data
	ld a,(hl)
	bit 0,a
	call nz,moveDown
	bit 1,a
	call nz,moveLeft
	bit 2,a
	call nz,moveRight
	bit 3,a
	call nz,moveUp
	and a,$F
	jp nz,menu_draw
	jp menu_loop
.select:
	
	jp menu_draw
.clear:
	
	jp menu_draw
.exit:
	jp normal

main:
main_draw:
	call gfx_ZeroScreen
	call draw_map_tiles
	call gfx_SwapDraw
main_loop:
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
	ld iy,player_data
	ld a,(hl)
	bit 0,a
	call nz,moveDown
	bit 1,a
	call nz,moveLeft
	bit 2,a
	call nz,moveRight
	bit 3,a
	call nz,moveUp
	and a,$F
	jp nz,main_draw
	jp main_loop
.exit:
	jp wait_key_unpress
normal:
	call ti_CloseAll
	call gfx_End
	ld iy,tiflags
	voidptr ti.pixelShadow, 69090
	call ti.RunIndicOn
	call ti.DrawStatusBar
	jp ti.HomeUp

wait_key_unpress:
	push hl
	call kb_Scan
	pop hl
	ld a,(hl)
	or a,a
	jr nz,$-8
	ret


moveDown:
	push af
	inc (iy+2)
	jr nc,.noc
	inc (iy+3)
.noc:
	pop af
	ret

moveUp:
	push af
	dec (iy+2)
	jr nc,.noc
	dec (iy+3)
.noc:
	pop af
	ret

moveRight:
	push af
	inc (iy)
	jr nc,.noc
	inc (iy+1)
.noc:
	pop af
	ret

moveLeft:
	push af
	dec (iy)
	jr nc,.noc
	dec (iy+1)
.noc:
	pop af
	ret

draw_map_tiles:
	ret


include 'arc_unarc.asm'
include 'load_libs.asm'

