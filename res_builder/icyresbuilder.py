#!/usr/bin/python3

import os,sys,json


class Generator:
	def __init__(self):
		pass
	def export(self,f):
		f.write(bytes([0]))

class Block:
	def __init__(self):
		pass

class Mob:
	def __init__(self):
		pass

class Biome:
	def __init__(self):
		pass

class Structure:
	def __init__(self):
		pass

class Feature:
	def __init__(self):
		pass


class icyModPack:
	def __init__(self,prop):
		self.prop = prop
		self.header = bytes(list("IPKv0000"))
		self.blocks = []
		self.mobs = []
		self.biomes = []
		self.structures = []
		self.features = []
		self.generator = Generator()
	def export(self,fname=None):
		if fname is None:
			fname="ipk-"+self.prop["export-name"]+".bin"
		self.init_table_index()
		with open(fname,"wb") as f:
			f.write(self.header)
			f.write(bytes(64))
			self.write_table_entry(f)
			for b in self.blocks:
				b.export_flags(f)
			self.write_table_entry(f)
			for b in self.blocks:
				b.export_damage(f)
			self.write_table_entry(f)
			for b in self.blocks:
				b.export_break(f)
			self.write_table_entry(f)
			for b in self.blocks:
				b.export_place_d(f)
			self.write_table_entry(f)
			for b in self.blocks:
				b.export_place_u(f)
			self.write_table_entry(f)
			for b in self.blocks:
				b.export_update(f)
			self.write_table_entry(f)
			for m in self.mobs:
				m.export_init(f)
			self.write_table_entry(f)
			for m in self.mobs:
				m.export_drops(f)
			self.write_table_entry(f)
			for m in self.mobs:
				m.export_update(f)
			self.write_table_entry(f)
			for b in self.biomes:
				b.export(f)
			self.write_table_entry(f)
			self.generator.export(f)
			self.write_table_entry(f)
			
			self.write_table_entry(f)
			
			self.write_table_entry(f)

	def write_table_entry(self,f):
		tell = f.tell()
		f.seek(self._table_entry_offset+8)
		f.write(tell.to_bytes('little',3))
		f.seed(tell)
		self._table_entry_offset+=3
	def init_table_index(self):
		self._table_entry_offset=8


def walk(d):
	p=next(os.fwalk(d))
	for f in p[1]:
		for a in walk(d+"/"+f):
			yield d+"/"+a
	for f in p[2]:
		yield d+"/"+f

if __name__=='__main__':
	if len(sys.argv)<2:
		print("Usage:\n icyresbuilder.py src_dir\n")
		exit(0)
	with open(sys.argv[1]+"/pack.json") as f:
		prop = json.load(f)
	mod = icyModPack(prop)
	for fname in walk(sys.argv[1]):
		if "block/" in fname:
			mod.register_block(fname)
		elif "item/" in fname:
			mod.register_item(fname)
		elif "biome/" in fname:
			mod.register_biome(fname)
		elif "structure/" in fname:
			mod.register_structure(fname)

