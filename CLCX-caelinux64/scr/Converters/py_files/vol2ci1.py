#
#Negten VOL to Calculix inp  by dip28p
#
#####################################################################################
_VERSION="INCOMPLETION Version 0.010  2011/Nov/15" #1st                 by dip28p
_VERSION="INCOMPLETION Version 0.020  2011/Nov/24" #Separate Option     by dip28p
_VERSION="INCOMPLETION Version 0.022  2011/Nov/25" #debug               by dip28p
_VERSION="INCOMPLETION Version 0.023  2011/Nov/25" #debug(STL)          by dip28p
#####################################################################################
import os
import sys

_NAME = os.path.split(sys.argv[0])[1]	#who am i?

_TOR = 1e-10

#              0     1             2       3
# >python vol2ci.py netgen.vol mesh.inp    S
#

print("%s : %s" % (_NAME , _VERSION))
print("----------------------------------")
#print("Narg=",len(sys.argv))
#

if len(sys.argv)<3 :
	print("usage > python  %s  infile     outfile   ForceSeparate(Option) " % _NAME)
	print("   ex > python  %s  netgen.vol out.inp                     " % _NAME)
	print("   ex > python  %s  netgen.vol out.inp            s        " % _NAME)
	print("")
	print("------------------------------------------")
	print("*EXAMPLE----------------------------------")
	print("------------------------------------------")
	print(">python %s netgen.vol  out.inp  s" % _NAME)
	print(">cgx -c  out.inp")
	print("     read r1")
	print("     quit")
	print(">ccx sol")

	sys.exit() 
#endif

inFile=(sys.argv[1])
outFile=(sys.argv[2])

FlugSeparate=False

if len(sys.argv)==4:
	if ( sys.argv[3].lower()[0]=="s" ):
		FlugSeparate=True
	#endif
#endif


###################################################################
"""
print("inFile=",inFile)
print("outFile=",outFile)
"""
##########################################################



lineCount=0

def ReadInp(fi):
	global lineCount
	s=fi.readline()
	if (s!=""):
		lineCount=lineCount+1
	#endif
	return s
#end def

##########################################################


#------------File----------------------------------------
fi = open(inFile,'r')
fo = open(outFile,'w')

#----read  until 'surfaceelementsuv---------------------------------
i=-1

s1=""
while True:
	s=ReadInp(fi)
	if (s==""):
		break
	s1=s.strip() #chomp
	i=s1.lower().find("surfaceelements")
	if ( i != -1 ):
		break
	i=s1.lower().find("surfaceelementsgi")
	if ( i != -1 ):
		break
	#endif
#end while
if (i==-1):
	fi.close()
	fo.close()
	print("surfaceelemt cannot found")
	sys.exit()
#endif


#----read  surface BC---------------------------------
s=ReadInp(fi)	# this is No of Surface
nSurface=int(s)
#     0       1       2       3       4       5       6       7
#surfnr    bcnr   domin  domout      np      p1      p2      p3
#     3     111       1       0       6       5       6       7      15      23      14  
BCs={}
NodesOnBC={}		#for separate nodes on BC

for i in range(nSurface):
	s=ReadInp(fi)
	s1=s.strip() #chomp
	s2=s1.split()
	iBC=int(s2[1])
	domain=int(s2[2])
	if ( iBC!=0 ):  # s2[1]=bncr , if the surface is BC
		if (iBC not in BCs):
			BCs[iBC]={}	# list initialize
			BCs[iBC]['domain']=domain	#Domain is defined by found 1st only.
			BCs[iBC]['nlist']=[]
		np=int(s2[4])	#np is s2[4]
		for j in range(np):
			nNode=int(s2[5+j])		#node list=s2[5],s2[6],,,,
			BCs[iBC]['nlist'].append(nNode)	
			if (FlugSeparate==True):
				if( (nNode in NodesOnBC )==False):	#if virgin , initialize
					NodesOnBC[nNode]={}
				#endif
				domain=int(s2[2])
				NodesOnBC[nNode][domain]=nNode
			#endif
		#end for j
	#end if
#end for






#----read  until 'volumeelements---------------------------------
s1=""
while True:
	s=ReadInp(fi)
	if (s==""):
		break
	s1=s.strip() #chomp
	i=s1.lower().find("volumeelements")
	if ( i != -1 ):
		break
	#endif

#----read  Volume data---------------------------------
s=ReadInp(fi)	# this is No of SolidElements
nVolEle=int(s)
#     0       1       2       3       4       5       6       7
#  matnr      np      p1      p2      p3      p4
#      1      10     501     467     468    1739    6950    6951    6952    2229    6953    6954

MeshListC3D4={}
MeshListC3D10={}
VolList={}
for i in range(1,nVolEle+1):
	s=ReadInp(fi)
	s1=s.strip() #chomp
	s2=s1.split()
	iVol=int(s2[0])
	np=int(s2[1])
	nodelist=[]
	for j in range(np):
		nodelist.append(int(s2[2+j]))
	#end for
	
	if(np==4):	#C3D4
		MeshListC3D4[i]={}
		MeshListC3D4[i]['domain']=iVol	#Domain
		MeshListC3D4[i]['nlist']=(
			nodelist[0],
			nodelist[2],
			nodelist[1],
			nodelist[3],
		)
	elif(np==10):	#C3D10
		MeshListC3D10[i]={}
		MeshListC3D10[i]['domain']=iVol	#Domain
		MeshListC3D10[i]['nlist']=(
			nodelist[0],
			nodelist[2],
			nodelist[1],
			nodelist[3],
			nodelist[5],
			nodelist[7],
			nodelist[4],
			nodelist[6],
			nodelist[9],
			nodelist[8],
		)
#		print("MeshList",i,MeshListC3D10[i])
	else:
		print("NodeSizeError")
		sys.exit()
	#endif
	
	if ( iVol not in VolList ):
		VolList[iVol]=[] #list initialize
	#endif
	VolList[iVol].append(i)
	
#end for




#----read  until 'points---------------------------------
s1=""
while True:
	s=ReadInp(fi)
	if (s==""):
		break
	s1=s.strip() #chomp
	i=s1.lower().find("points")
	if ( i != -1 ):
		break
	#endif

#----read  point data---------------------------------

NodeList={}

s=ReadInp(fi)	# this is No of Points
nPoint=int(s)
fp=open(outFile,"w")
fp.write("*NODE,Nset=Nall\n")

for i in range(1,nPoint+1):
	s=ReadInp(fi)
	s1=s.strip() #chomp
	s2=s1.split()
	NodeList[i,0]=(s2[0])
	NodeList[i,1]=(s2[1])
	NodeList[i,2]=(s2[2])
#	fp.write("%d,%s,%s,%s\n" % (i,s2[0],s2[1],s2[2]))
#end for

MaxNode=i
#print("MaxNode=",MaxNode)



for i in NodesOnBC:
	n=len(NodesOnBC[i])
	
	if n != 1 :
		k=0
		nod={}
		for j in NodesOnBC[i]:
			nod[k]=j
			k=k+1
		#end for j
		
		for j in range(1,len(nod)):
			MaxNode=MaxNode+1
			NodesOnBC[i][nod[j]]=MaxNode
			NodeList[MaxNode,0]=NodeList[i,0]
			NodeList[MaxNode,1]=NodeList[i,1]
			NodeList[MaxNode,2]=NodeList[i,2]
			print("separate %d -> %d" % (i,MaxNode))
		#end for j
	#endif
#end for 



for i in range(1,MaxNode+1):
#	fp.write("%d,%s,%s,%s\n" % (i,NodeList[i,0],NodeList[i,1],NodeList[i,2]))
	fp.write("%d,%10e,%10e,%10e\n" % (int(i),float(NodeList[i,0]),float(NodeList[i,1]),float(NodeList[i,2])))
#end for 



Nodes={}
nlist={}
iElements=1



############TODO C3D4########################################
if (len(MeshListC3D4) != 0 ):
	nop=0
#endif


if (len(MeshListC3D10) != 0 ):
	fp.write("\n")
	fp.write("*ELEMENT, TYPE=C3D10, ELSET=Eall\n")
	for im in range(1,nVolEle+1):
#		print("im=",im)
		fp.write(str(iElements))
		domain=MeshListC3D10[im]['domain']
		nlist=MeshListC3D10[im]['nlist']
		for i in range(10):
			ii=nlist[i]
#			print("i=",i)
			if (( ii in NodesOnBC )==True) :
#				print ("dbg","im",im,"i",i,"domain",domain,NodesOnBC[i] )
				Nodes[i]=NodesOnBC[ii][domain]
			else :
				Nodes[i]=ii
			#end if
		#end for i

		for i in range(10):
			fp.write(",")
			fp.write(str(Nodes[i]))
		#end for i
		fp.write("\n")
		iElements=iElements+1
	#end for im
#endif


for lst in VolList:
	s="VOL" + str(lst)
	print("ELSET-%s Created" % s )
	fp.write("\n")
	fp.write("*ELSET,ELSET=" + s+"\n") 
	nlist=(sorted(set(VolList[lst])))
	for i in nlist:
		fp.write("%d,\n" % i)
	#end for i
#end for NodeList


#fw1=open("r1","w")

#fw1.write("plot e all\n")
#fw1.write("view elem on\n")
#fw1.write("view edge on\n")



for lst in BCs:
	s="BC" + str(lst)
	print("BCSET-%s Created" % s )
#	fw1.write("plus n %s\n" % s )
#	fw1.write("comp %s down\n" % s)
#	fw1.write("comp %s down\n"% s)
#	fw1.write("plus f %s\n"% s)
#	fw1.write("send %s abq sur\n"% s)
#	fw1.write("send %s abq pres 1.0\n"% s)

	fp.write("\n")
	fp.write("*NSET,NSET=BC" + str(lst)+"\n") 
	nlist=(sorted(set(BCs[lst]['nlist'])))
	domain=BCs[lst]['domain']
#	print BCs[lst]
	for i in nlist:
		ii = i
		if ( i in NodesOnBC ):
			if (len(NodesOnBC[i]) >= 2  ):
				ii = NodesOnBC[i][domain]
			#endif
		#endif
		
		fp.write("%d,\n" % ii)
	#end for i
#end for NodeList
#fw1.write("frame\n")
#fw1.close()
fi.close()
fp.close()



print("==================================")
print("%s -> %s      Done" % (inFile , outFile))
print("==================================")
print("Conversion has been done. \n   The INP file should been copied into working folder,\n    " % (outFile))
print("-----------------------------------")

import time
time.sleep(3)
