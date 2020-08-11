
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

