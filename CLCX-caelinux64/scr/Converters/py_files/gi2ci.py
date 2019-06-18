#
#Gmsh to Calculix inp  by dip28p
#for Gmsh version 2.5.1-svn Build date: 20110804 ( or later?)

import os
import sys
#####################################################################################
_VERSION="INCOMPLETION Version 0.010  2011/Aug/23" #1st                 by dip28p
_VERSION="INCOMPLETION Version 0.021  2011/Aug/26" #add check node No   by dip28p
#####################################################################################
_NAME = os.path.split(sys.argv[0])[1]	#who am i?

_TOR = 1e-10

print("%s : %s" % (_NAME , _VERSION))
print("----------------------------------")


if len(sys.argv)>4:
	mshFile=sys.argv[1]
	outFile=sys.argv[2]
	elemDim=sys.argv[3]
	elemPreWord=sys.argv[4]
	elemPostWord=""
	if len(sys.argv)>5:
		elemPostWord=sys.argv[5]
	
else:
	print("usage > python  %s  infile   outfile  Dim  elemPreWord  elemPostWord " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   2     S                      " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   2     S              R       " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   2     CPS                    " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   2     CPE                    " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   2     CAX                    " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   3     C3             R       " % _NAME)
	print("   ex > python  %s  gmsh.inp sol.inp   3     F3                     " % _NAME)
	print("")
	print("cgx -c outfile")
	print("     prnt set")
	print("     plot e all")
	print("     plus n N**")
	print("     comp N** down")
	print("     send all abq names")
	print("     send N** abq pres 0.1")
	sys.exit() 
#endif

if ( elemDim !="2" and elemDim != "3" ):
	print("*****ERROR******")
	print("in =%s" % mshFile)
	print("out=%s" % outFile)
	print("Dim=%s" % elemDim)
	print("Pre=%s" % elemPreWord)
	print("Post=%s" % elemPostWord)
	print("------------------")
	print("Dimension[%s] must be 2 or 3 " % elemDim)
	sys.exit("*CMD ERROR*")
#endif

###################################################################
"""
print("in =",mshFile)
print("out=",outFile)
print("elemDim=",elemDim)
print("elemPre=",elemPreWord)
print("elemPost=",elemPostWord)
"""
##########################################################

Nodes={}
lineCount=0

def ReadInp(fi):
	global lineCount
	s=fi.readline()
	if (s!=""):
		lineCount=lineCount+1
	#endif
	return s
#end def

def Line2Float(line):
    """Convert a string into a list of Float"""
    return map(float,line.split())

##########################################################
#------------Remove Line (newer GMSH)----

f = open(mshFile,"r")
lines = f.readlines()
f.close()
f = open(mshFile,"w")
for line in lines:
  if line!="******* E L E M E N T S *************"+"\n":
    f.write(line)
f.close()

#------------File----------------------------------------
fi = open(mshFile,'r')
fo = open(outFile,'w')



#----read  until '*Node---------------------------------
s1=""
while True:
	s=ReadInp(fi)
	if (s==""):
		break
	s1=s.strip() #chomp
	i=s1.upper().find("*NODE")
	if ( i != -1 ):
		break
	#endif
	fo.write(s)

#----read  until '*Element---------------------------------

fo.write("*Node\n")
s1=""

while True:
	s=ReadInp(fi)
	if (s==""):
		break
	s1=s.strip() #chomp
	s2=s1.split(",")
	i=s1.upper().find("*ELEMENT")
	if ( i != -1 ):
		break
	#endif
	#fo.write("%d,%25.14g,%25.14g,%25.14g\n" % (int(s2[0]) , float(s2[1]) ,  float(s2[2]) ,  float(s2[3])))
	fo.write("%d,%10e,%10e,%10e\n" % (int(s2[0]) , float(s2[1]) ,  float(s2[2]) ,  float(s2[3])))
	MaxNode=int(s2[0])
#end while

print("# of Node= %d" % MaxNode)

#*Element, type=C3D4, ELSET=VOLUME58

while  True:
	s2=s1.split(",")
	if (s==""):
		break



#	print("###s2=",s2)
	for i in range(len(s2)):
		iT=s2[i].upper().find("TYPE=")
		if ( iT != -1 ):
			iType=i
			nType=iT
		iE=s2[i].upper().find("ELSET=")
		if ( iE != -1 ):
			iElem=i
			nElem=iE
	#end For

	sType=s2[iType][nType+5:]
	sElset=s2[iElem][nElem+6:]
	iNodes=sType.upper().find("D")


#	print("###$",sType , sElset)	
	flgValidElement=False
#-----------------------------------------------------C1D3-------------------
	if (sType.upper().find("C1D3") != -1) or (sType.upper().find("T3D3") != -1):
		flgValidElement=True
		NodeList=[0]*MaxNode

		ss="*NSET,NSET=%s" % sElset
		print(ss)
		fo.write(ss); 
		fo.write("\n"); 
		while True:
			s=ReadInp(fi)
#			print ("### C13D : s="+s)
			s1=s.strip() #chomp
			if (s==""):
				break

			i=s1.upper().find("*ELEMENT")
			if ( i!=-1 ):
				break
			#endif
			s2=s1.split(",")
			nnode=len(s2)-1
			if (nnode != 3) : 
				print("**ERROR:Elements should has 3 node but it has %d " % nnode )
				print("          %s(%d) : [%s]" % (mshFile , lineCount , s1))
				sys.exit("*INP ERROR*") 
			#endif
			for i in range(1,4):
				NodeList[int(s2[i])]=1
			#end for
		#end while
		for i in range(MaxNode):
			if ( NodeList[i]==1):
				fo.write("%d,\n" % i )
			#endif
		#end for
		
	#end if
#-----------------------------------------------------C2D3-------------------
	if (sType.upper().find("C2D3") != -1) or (sType.upper().find("CPS3") != -1):
		flgValidElement=True
		NodeList=[0]*MaxNode
#		print("###C2D3 founbd")
		ss="*NSET,NSET=%s" % sElset
		print(ss)
		fo.write(ss); 
		fo.write("\n"); 
		
		while True:
			s=ReadInp(fi)
			s1=s.strip() #chomp
			if (s==""):
				break
			i=s1.upper().find("*ELEMENT")
			if ( i!=-1 ):
				break
			#endif
			s2=s1.split(",")
			nnode=len(s2)-1
			if (nnode != 3) : 
				print("*************************************************")
				print("          %s(%d) : [%s]" % (mshFile , lineCount , s1))
				print("**ERROR:Elements should has 3 node but it has %d " % nnode )
				sys.exit("*INP ERROR*") 
			#endif
			for i in range(1,4):
				NodeList[int(s2[i])]=1
			#end for
		#end while
		for i in range(MaxNode):
			if ( NodeList[i]==1):
				fo.write("%d,\n" % i )
			#endif
		#end for
		
	#end if
#-----------------------------------------------------C2D6-------------------
	if (sType.upper().find("C2D6") != -1) or (sType.upper().find("CPS6") != -1):
		flgValidElement=True
		NodeList=[0]*MaxNode
		if (elemDim == "2"):
#			print("###2D-C2D6 found")
			sType=elemPreWord + "6" + elemPostWord
			ss="*Element , Type=%s , ELSET=%s" % (sType , sElset)
			print(ss) 
			fo.write(ss); 
			fo.write("\n")
			while True:
				s=ReadInp(fi)
#				print("###2D-C2D6-s:%s" % s)
				if (s==""):
					break
				s1=s.strip() #chomp
				
				i=s1.upper().find("*ELEMENT")
				if ( i!=-1 ):
					break
				#endif

				s2=s1.split(",")
				nnode=len(s2)-1
				if (nnode != 6) : 
					print("*************************************************")
					print("          %s(%d) : [%s]" % (mshFile , lineCount , s1))
					print("**ERROR:Elements should has 6 node but it has %d " % nnode )
					sys.exit("*INP ERROR*") 
				#endif

				fo.write("%s,\n" % s1 )
		#endif
		if (elemDim == "3"):
#			print("###3D-C2D6 found")
			ss="*NSET,NSET=%s" % sElset
			print(ss)
			fo.write(ss); 
			fo.write("\n"); 
			while True:
				s=ReadInp(fi)
				s1=s.strip() #chomp
				if (s==""):
					break
				i=s1.upper().find("*ELEMENT")
				if ( i!=-1 ):
					break
				#endif
				s2=s1.split(",")
				nnode=len(s2)-1
				if (nnode != 6) : 
					print("*************************************************")
					print("          %s(%d) : [%s]" % (mshFile , lineCount , s1))
					print("***ERROR:Elements should has 6 node but it has %d " % nnode )
					sys.exit("*INP ERROR*") 
				#endif
				for i in range(1,7):
					NodeList[int(s2[i])]=1
				#end for
			#end while
			for i in range(MaxNode):
				if ( NodeList[i]==1):
					fo.write("%d,\n" % i )
				#endif
			#end for
		#endif
	#end if

#-----------------------------------------------------C3**-------------------
	if (sType.upper().find("C3") != -1):
		flgValidElement=True
		NodeList=[0]*MaxNode
		if (elemDim == "3"):
#			print("###3D-C3 found")
			sType=elemPreWord + sType[iNodes:] + elemPostWord
			ss="*Element , Type=%s , ELSET=%s" % (sType , sElset)
			print(ss) 
			fo.write(ss)
			fo.write("\n")
			while True:
				s=ReadInp(fi)
				if (s==""):
					break
				s1=s.strip() #chomp
				i=s1.upper().find("*ELEMENT")
				if ( i!=-1 ):
					break
				#endif
				fo.write("%s,\n" % s1 )
		#endif
	#end if


	if (flgValidElement==False):
		print("****************************************************")
		print("**** ERROR : Type:[%s] is not supported " % sType )
		print("          %s(%d) : [%s]" % (mshFile , lineCount , s1))
		print("")
		print("** please confirm that Elements order == 2 **") 
		sys.exit("*INP ERROR*")
	#endif 
# end while

fi.close()
fo.close()

#------------------------------bonus track :-) -------------------
#removed


print("----------------------------------")
print("%s -> %s      Done" % (mshFile , outFile))

import time
time.sleep(5)
