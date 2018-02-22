# ShortTextGrid.py: Input and output for short format Praat TextGrid.     
# (This was basically used to convert textgrids into tables
# for further analysis in R).  
# Usage: python ShortTextGrid.py Filename
# 
# At the moment this will output one file per tier in the Textgrid 
# Catherine Lai (clai@inf.ed.ac.uk)  

import glob
import sys
	
class TextTier:
	def __init__(self, name):
		self.name = name
		self.start = 0
		self.end = 0
		self.nintervals = 0
		self.starttimes = []
		self.endtimes = []
		self.labels = []
	
	def fill_tier(self, lines): 	
		self.start = float(lines[2])
		self.end = float(lines[3])
		self.nintervals = int(lines[4])
		currlines = lines[5:] 	
		for i in range(0,self.nintervals): 
			self.starttimes.append(float(currlines[i*2].strip()))
			self.labels.append(currlines[i*2 + 1].strip().strip("\""))

		self.endtimes = self.starttimes

	def fill_from_slpa(self, xheader, xtimes):
		self.start = xtimes[0][3]
		self.end = xtimes[-1][4]
		self.nintervals = len(xtimes)
		self.starttimes = [x[3] for x in xtimes]
		self.endtimes = [x[4] for x in xtimes]
		self.labels = ["syl" for x in xtimes]  


	# print_table: print to standard out
	def print_table (self):
		for i in range(0,self.nintervals):
			print self.name.strip("\""), "\t",
			print self.labels[i].strip("\""), "\t",
			print self.starttimes[i],"\t",
			print self.endtimes[i]

	# write_tier: return table as string. 
	def write_tier (self):
		tier = "" 
		for i in range(0,self.nintervals):
			tier = tier + self.name.strip("\"") + "\t"
			tier = tier + self.labels[i].strip("\"") + "\t"
			tier = tier + str(self.starttimes[i]) + "\t"
			tier = tier + str(self.endtimes[i]) + "\n"
		return tier

	def write_tg (self) :
		tier = "\"TextTier\"\n" 
		tier = tier + "\"" + self.name + "\"\n"
		tier = tier + str(self.start) + "\n"
		tier = tier + str(self.end) + "\n"
		tier = tier + str(self.nintervals) + "\n"

		for i in range(0,self.nintervals):
			tier = tier + str(self.starttimes[i]) + "\n"
			tier = tier + "\"" + self.labels[i] + "\"\n"

		return tier
		
	def label_string (self): 
		labels = ""
		for i in range(0, self.nintervals):
			labels = labels + self.labels[i].strip("\"") + " "

		return labels


class IntervalTier:
	def __init__(self, name):
		self.name = name
		self.start = 0
		self.end = 0
		self.nintervals = 0
		self.starttimes = []
		self.endtimes = []
		self.labels = []
	
	def fill_tier(self, lines): 	
		self.start = float(lines[2])
		self.end = float(lines[3])
		self.nintervals = int(lines[4])
		currlines = lines[5:] 	
		for i in range(0,self.nintervals): 
			self.starttimes.append(float(currlines[i*3].strip()))
			self.endtimes.append(float(currlines[i*3 + 1].strip()))
			self.labels.append(currlines[i*3 + 2].strip().strip("\""))

	def fill_long_tier(self, lines): 	
		self.start = float(lines[3].split()[2])
		self.end = float(lines[4].split()[2])
		self.nintervals = int(lines[5].split()[3])
		currlines = lines[6:] 	

		for i in range(0,self.nintervals): 
			self.starttimes.append(float(currlines[i*4 + 1].strip().split()[2]))
			self.endtimes.append(float(currlines[i*4 + 2].strip().split()[2]))
			label=" ".join(currlines[i*4 + 3].strip().strip("\"").split()[2:])
			if label == "":
				label = "_NA_"
			self.labels.append(label)

	def fill_from_slpa(self, xheader, xtimes):
		self.start = xtimes[0][3]
		self.end = xtimes[-1][4]
		xstart = [x[3] for x in xtimes]
		xend = [x[4] for x in xtimes]
		xlabels = ["syl" for x in xtimes]  

		self.starttimes = [xstart[0]]	
		self.endtimes = [xend[0]]	
		self.labels = [xlabels[0]]	
		
		for i in range(1, len(xstart)):
			if (xstart[i] > xend[i-1]):  
				self.starttimes.append(xend[i-1])			
				self.endtimes.append(xstart[i])			
				self.labels.append("sil")			
			self.starttimes.append(xstart[i])			
			self.endtimes.append(xend[i])			
			self.labels.append(xlabels[i])			
				
		self.nintervals = len(self.starttimes)
		return
			

	def print_table (self):
		for i in range(0,self.nintervals):
			print self.name.strip("\""), "\t",
			print self.labels[i].strip("\""), "\t",
			print self.starttimes[i],"\t",
			print self.endtimes[i]

	def write_tier (self):
		tier = "" 
		for i in range(0,self.nintervals):
			tier = tier + self.name.strip("\"") + "\t"
			tier = tier + self.labels[i].strip("\"") + "\t"
			tier = tier + str(self.starttimes[i]) + "\t"
			tier = tier + str(self.endtimes[i]) + "\n"
		return tier

	def write_tg (self) :
		tier = "\"IntervalTier\"\n" 
		tier = tier + "\"" + self.name + "\"\n"
		tier = tier + str(self.start) + "\n"
		tier = tier + str(self.end) + "\n"
		tier = tier + str(self.nintervals) + "\n"


		for i in range(0,self.nintervals):
			tier = tier + str(self.starttimes[i]) + "\n"
			tier = tier + str(self.endtimes[i]) + "\n"
			tier = tier + "\"" + self.labels[i] + "\"\n"

		return tier

	def label_string (self): 
		labels = ""
		for i in range(0, self.nintervals):
			labels = labels + self.labels[i].strip("\"") + " "

		return labels


class TextGrid:
	def __init__ (self):
		self.filename = ""
		self.start = 0
		self.end = 0
		self.ntiers = 0
		self.tiers = {}

	def read_tiers(self, filename):
		self.filename = filename
		f = open(filename, "r")
		lines = f.readlines()
		f.close()

		self.start = float(lines[3].split()[2])
		self.end = float(lines[4].split()[2])
		self.ntiers = int(lines[6].split()[2])	
		
		print "***", self.start, self.end, self.ntiers
		print "***" 

		self.tiers = getTiers(lines, self.ntiers)	

	def set_start (self, starttime):
		self.start = starttime

	def set_end (self, endtime):
		self.end = endtime	

	def whoami (self):
		print self.filename	
		print self.start	
		print self.end
		print self.ntiers
		print self.tiers

	def write_textgrid (self) :
		tg = "File type = \"ooTextFile short\"\nObject class = \"TextGrid\"\n\n" 
		tg = tg + str(self.start) + "\n"
		tg = tg + str(self.end) + "\n"
		tg = tg + "<exists>\n"
		tg = tg + str(self.ntiers) + "\n"

		for k in self.tiers.keys():
			tg = tg  +  self.tiers[k].write_tg()

		return tg

	def add_tier (self, tier):
		self.ntiers += 1
        	self.tiers[tier.name] = tier 
 
		
def getTiers (lines, ntiers, long=True): 
	if long:
		startline = 8
	else:
		startline = 7
	tiers = {}
	currlines = lines[startline:]
	nextlines = []
	inttier=0
	texttier=0
	nlines=len(currlines)
	#print "getTiers"
	#print currlines

	for i in range(ntiers):
#		print currlines[0]
		if currlines[0].split()[0] != "item":
			print "Warning: No new item?"
			print currlines[0]
			break	


		tiertype = currlines[1].split()[2].strip("\"")
		print tiertype
		print currlines[2]
		tiername = "".join(currlines[2].split()[2:]).strip("\"")
		print tiername
		nint = int(currlines[5].split()[3])
		endtier = 5+4*nint 	

		#print tiertype, tiername, endtier
		#print "NEXT?:", currlines[min(endtier, nlines)]

		if tiertype == "IntervalTier": 
			tiers[tiername] = IntervalTier(tiername)
		else:
			print "Need to fill other tier types!"		
			break

		tiers[tiername].fill_long_tier(currlines)

		currlines = currlines[endtier+1:]

	return tiers

#########################################################################3

def get_tab_data(filename): 
	ax = []
	with open(filename) as f:
		xheader = f.readline().strip().split()
		for line in f: # read rest of lines
			sline = line.strip().split()
			ax.append([float(x) for x in sline])
	return [xheader, ax]


def test_case ():
	fname = "test.TextGrid"
	x = TextGrid(fname)
	print  x.tiers.keys()

	#tiers = ""
	#for k in x.tiers.keys():
	#	tiers = tiers  +  x.tiers[k].write_tier()

	tiers = x.write_textgrid()
	outfname = fname + ".tab"
	f = open(outfname, "w")
	f.write(tiers)
	f.close()


def test_slpa ():
	fname = "/disk/scratch/icsi/prosody/syl/Bed017_chanF.slpa"
	x, y = get_tab_data("/disk/scratch/icsi/prosody/syl/Bed017_chanF.slpa")
	tt = IntervalTier("syl")
	tt.fill_from_slpa(x, y)
	#print tt, tt.start, tt.end

	tg = TextGrid()
	tg.set_start(tt.start)
	tg.set_end(tt.end)
	tg.add_tier(tt)

	print tg.write_textgrid()

def convert_slpa (fname, outdir):
	fstem = fname.split("/")[-1].split(".")[0]
	x, y = get_tab_data(fname)
	tt = IntervalTier("syl")
	tt.fill_from_slpa(x, y)
	#print tt, tt.start, tt.end

	tg = TextGrid()
	tg.set_start(tt.start)
	tg.set_end(tt.end)
	tg.add_tier(tt)

	outfname = outdir + "/" + fstem + ".syl" 
	with open(outfname, "w") as outf:
		outf.write(tg.write_textgrid())

	return

def convert_slpa_files (slpadir):
	filenames = glob.glob(slpadir)
	for filename in filenames:
		print filename
		convert_slpa(filename, "/disk/scratch/icsi/prosody/syl/")
	
if __name__ == '__main__':
	#convert_slpa_files("/disk/scratch/icsi/prosody/slpa/*slpa")
	
	fname = sys.argv[1]
	print fname
	x = TextGrid()
	x.read_tiers(fname)
	print  x.tiers.keys()
	
	tiers = ""
	for k in x.tiers.keys():
		outfname = fname + "." + k +".txt"
		f = open(outfname, "w")
		f.write(x.tiers[k].write_tier())
		f.close()

		#tiers = tiers  +  x.tiers[k].write_tier()

#	outfname = fname + ".tab"
#	f = open(outfname, "w")
#	f.write(tiers)
#	f.close()
