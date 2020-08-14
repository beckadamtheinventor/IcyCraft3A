
Errors:
	.Error:
		db "An error occured!",0
	.UnknownError:
		db "Something went wrong",0
	.TexturePackFile:
		db "Missing texture pack",0

Strings:
	.X:
		db " X: ",0
	.Y:
		db " Y: ",0
	.MenuTitle:
		db "ICYCRAFT",0

Files:
	.DefaultWorldFile:
		db "ICYMap00"
	.DefaultBehaviorFile:
		db "ICYbvr00"
	.DefaultTexturePack:
		db "ICYtxp00"
	.PackFile:
		db "ICYpacks"
	.TempFile:=$+1
		db ti.AppVarObj,"__icytmp",0

Modes:
	.R:
		db 'r',0
	.W:
		db 'w',0

str_SavingWorld:
	db "Saving World...",0
str_WorldVersionNumber:
	db "ICYv000",0

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
behaviours_ptr:=texture_pack+8
world_chunk_gen_flags:=behaviours_ptr+3
world_data:=world_chunk_gen_flags+32
;next:=world_data+$FFE8

assert world_data < ti.pixelShadow+69090-(65536+32)

o_player_x:=0
o_player_y:=3
o_player_z:=6
o_player_inv:=64

bvr_jt_block_flags   :=  8
bvr_jt_block_damage  := 11
bvr_jt_block_break   := 14
bvr_jt_block_place_d := 17
bvr_jt_block_place_u := 20
bvr_jt_block_update  := 23
bvr_jt_mob_init      := 26
bvr_jt_mob_drop      := 29
bvr_jt_mob_update    := 32
bvr_jt_biome_index   := 35
bvr_jt_world_gen     := 38
bvr_jt_feature_gen   := 41
bvr_jt_struc_gen     := 44

bvr_jt_block_l       := 47
bvr_jt_mob_l         := 48
bvr_jt_biome_l       := 49
bvr_jt_feature_gen_l := 50
bvr_jt_struc_gen_l   := 51


bvr_wg_block_index_l := 0
bvr_wg_block_index   := 1

