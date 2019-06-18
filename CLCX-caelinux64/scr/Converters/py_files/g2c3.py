#
#Gmsh to Calculix inp  by dip28p
#

import os
import sys
##############################################################
_VERSION="INCOMPLETION Version 0.01  2011/Aug/19"
#############################################################
_NAME = os.path.split(sys.argv[0])[1]	#who am i?

print("%s : %s" % (_NAME , _VERSION))
print("----------------------------------")


if len(sys.argv)>3:
	mshFile=sys.argv[1]
	outFile=sys.argv[2]
	elemPreWord=sys.argv[3]
	elemPostWord=""
	if len(sys.argv)>4:
		elemPostWord=sys.argv[4]
	
else:
	print("usage > python  %s  infile   outfile  elemPreWord  elemPostWord " % _NAME)
	print("   ex > python  %s  gmsh.msh sol.inp    C3                      " % _NAME)
	print("   ex > python  %s  gmsh.msh sol.inp    C3             R        " % _NAME)
	print("")
	print("cgx -c outfile")
	print("     prnt set")
	print("     plot e all")
	print("     plus n N**")
	print("     comp N** down")
	print("     send all abq names")
	print("     send N** abq names")
	print("     send N** abq pres 0.1")
	sys.exit() 


##########################################################
ElemDict={}	#No of nodes
ElemDict[1]=2	#1:2-node line. 
ElemDict[2]=3	#2:3-node triangle. 
ElemDict[3]=4	#3:4-node quadrangle. 
ElemDict[4]=4	#4:4-node tetrahedron. 
ElemDict[5]=8	#5:8-node hexahedron. 
ElemDict[6]=6	#6:6-node prism. 
ElemDict[7]=5	#7:5-node pyramid. 
ElemDict[8]=3	#8:3-node second order line 
ElemDict[9]=6	#9:6-node second order triangle 
ElemDict[10]=9	#10:9-node second order quadrangle 
ElemDict[11]=10	#11:10-node second order tetrahedron 
ElemDict[12]=27	#12:27-node second order hexahedron 
ElemDict[13]=18	#13:18-node second order prism 
ElemDict[14]=14	#14:14-node second order pyramid
ElemDict[15]=1	#15:1-node point. 
ElemDict[16]=8	#16:8-node second order quadrangle 
ElemDict[17]=20	#17:20-node second order hexahedron 
ElemDict[18]=15	#18:15-node second order prism 
ElemDict[19]=13	#19:13-node second order pyramid 
##########################################################
ElemOrder={}	#1D=1 2D=2 3D=3
ElemOrder[1]=1	#1:2-node line. 
ElemOrder[2]=2	#2:3-node triangle. 
ElemOrder[3]=2	#3:4-node quadrangle. 
ElemOrder[4]=3	#4:4-node tetrahedron. 
ElemOrder[5]=3	#5:8-node hexahedron. 
ElemOrder[6]=3	#6:6-node prism. 
ElemOrder[7]=3	#7:5-node pyramid. 
ElemOrder[8]=1	#8:3-node second order line 
ElemOrder[9]=2	#9:6-node second order triangle 
ElemOrder[10]=2	#10:9-node second order quadrangle 
ElemOrder[11]=3	#11:10-node second order tetrahedron 
ElemOrder[12]=3	#12:27-node second order hexahedron 
ElemOrder[13]=3	#13:18-node second order prism 
ElemOrder[14]=3	#14:14-node second order pyramid
ElemOrder[15]=1	#15:1-node point. 
ElemOrder[16]=2	#16:8-node second order quadrangle 
ElemOrder[17]=3	#17:20-node second order hexahedron 
ElemOrder[18]=3	#18:15-node second order prism 
ElemOrder[19]=3	#19:13-node second order pyramid 

##########################################################

##########################################################
#------------File----------------------------------------
fi = open(mshFile,'r')
fo = open(outFile,'w')

fo.write("%s\n" % "*Heading" )
fo.write(" Convert from %s\n" % mshFile )

#----read  until '$Nodes---------------------------------
s1=""
while True:
	s=fi.readline()
	s1=s.strip() #chomp
	if (s1 == "$Nodes" ):
		break
#	print("data=",ss)



############# NODES ########################################
nNodes=fi.readline()  # No of Nodes
nNodes=int(nNodes)
fo.write("*Node\n")

#----read and write until '$EndNodes----------------------
ss=""
while True:
	s=fi.readline()
	s1=s.strip() #chomp
	if (s1 == "$EndNodes" ):
		break
	s2=s1.split() #split with space 
#	print("data=",s2)
#	fo.write("%s,%s,%s,%s\n" % (s2[0],s2[1],s2[2],s2[3]) )
	fo.write("%d,%10e,%10e,%10e\n" % (int(s2[0]) , float(s2[1]) ,  float(s2[2]) ,  float(s2[3]))) 

#----read  until '$Elements-----------------------------------
while True:
	s=fi.readline()
	s1=s.strip() #chomp
	if (s1 == "$Elements" ):
		break

#print("##DBG0:end of nodes")
nElements=fi.readline()  # No of Elements


############# ELEMENTS #############################################
# 0            1         2           3  4        5   6  7   8   9   10 ....
#elm-number elm-type number-of-tags < tag > ... node-number-list
#--------------------------------------------------------------------------------------
#79           11         2           58 1       553 307 888 901 910 911 912 913 914 915
#--------------------------------------------------------------------
#----read and write until '$$EndElements
ss=""

listElem={}	#NODE list for Elements
listTemp={}
listWork={}

iPhysicalOld=-1
tElemOld=-1
iElsetNo=0
listNodes={}
listElem={}
while True:
	s=fi.readline()
	s1=s.strip() #chomp
	if (s1 == "$EndElements" ):
		break
	s2=s1.split() #split with space 
#	print("data=",s2)
	iElem=int(s2[0])		#Elem No	79
	tElem=int(s2[1])	#ElemType	11
	nTag=int(s2[2])		#NoOfTag	2
	if(nTag != 0):
		iPhysical=int(s2[3])

	else:
		iPhysical=0
#	print("###DBG1:","#",iElem,"Type",tElem,"nTag",nTag,"Phy",iPhysical,"PhyOld",iPhysicalOld,ElemDict[tElem],"LEN=",len(listNodes))
	
	
	if ( iPhysicalOld != iPhysical ):
#		print("###DBG2:",iPhysicalOld,iPhysical)
		if len(listNodes) != 0 :
			sw="*NSET,NSET=N%d\n" % (iPhysicalOld)
			print(sw)
			fo.write(sw)
			for i in listNodes:
				if ( listNodes[i] != 0 ):
					fo.write("%d,\n" % i )
		#endif

		listNodes={}		#list to strage BC node list

		if ( tElem == 4  ) : #4=C3D4
			Ename=elemPreWord + "D4" + elemPostWord
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)

		if ( tElem == 5  ) : #5=C3D8
			Ename=elemPreWord + "D8" + elemPostWord
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)

		if ( tElem == 6  ) : #6=C3D6
			Ename=elemPreWord + "D6" + elemPostWord
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)

		if ( tElem == 11  ) : #11=C3D10 tet
			Ename=elemPreWord + "D10" + elemPostWord
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)
		if ( tElem == 18  ) : #13=C3D18
			Ename=elemPreWord + "D15" + elemPostWord
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)
		if ( tElem == 17  ) : #17=C3D20
			Ename=elemPreWord + "D20" + elemPostWord
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)

		if ( tElem == 9  ) : #9=S6
			Ename="S6"
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)

		if ( tElem == 10  ) : #9=S8
			Ename="S8R"
			sw="*Element, type=%s, ELSET=E%d\n" % (Ename,iPhysical)
			print(sw)
			fo.write(sw)

   
		iPhysicalOld = iPhysical

	if ( ElemOrder[tElem] != 3  ) : #2D Elements , Point Line Plane

		for i in range(ElemDict[tElem]):
			listElem[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
			listNodes[listElem[i]]=1		#flug


	if ( tElem == 6  or tElem == 4  or tElem == 5  ) : #6=C3D6  #4=C3D4  #5=C3D8  
#		print("###DBG31:",iPhysicalOld,iPhysical)

		for i in range(ElemDict[tElem]):
			listElem[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
#			print ("###DBG3:","i",i,listElem[i])

		fo.write("%d," % iElem)
		for i in range(ElemDict[tElem]):
			fo.write("%d," % listElem[i])
		fo.write("\n")

	if ( tElem == 11  ) : #11=C3D10
#		print("###DBG31:",iPhysicalOld,iPhysical)

		for i in range(ElemDict[tElem]):
			listElem[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
#			print ("###DBG3:","i",i,listElem[i])

		listElem[8],listElem[9] = listElem[9],listElem[8]


		fo.write("%d," % iElem)
		for i in range(ElemDict[tElem]):
			fo.write("%d," % listElem[i])
		fo.write("\n")
  
  
  	if ( tElem == 9 ) : #10=S6
#		print("###DBG31:",iPhysicalOld,iPhysical)

		for i in range(ElemDict[tElem]):
			listElem[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
#			print ("###DBG3:","i",i,listElem[i])


		fo.write("%d," % iElem)
		for i in range(ElemDict[tElem]):
			fo.write("%d," % listElem[i])
		fo.write("\n")
  
  	if ( tElem == 10 ) : #10=S8
#		print("###DBG31:",iPhysicalOld,iPhysical)

		for i in range(ElemDict[tElem]):
			listElem[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
               # listElem=listElem.pop()

   
   

		fo.write("%d," % iElem)
		for i in range(ElemDict[tElem]):
			fo.write("%d," % listElem[i])
		fo.write("\n")  
  

	if ( tElem == 18  ) : #13=C3D15
#		print("###DBG31:",iPhysicalOld,iPhysical)

		for i in range(ElemDict[tElem]):
			listWork[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
#			print ("###DBG3:","i",i,listWork[i])
		listElem[0]=listWork[0]
		listElem[1]=listWork[1]
		listElem[2]=listWork[2]
		listElem[3]=listWork[3]
		listElem[4]=listWork[5]
		listElem[5]=listWork[5]
		listElem[6]=listWork[6]
		listElem[7]=listWork[9]
		listElem[8]=listWork[7]
		listElem[9]=listWork[12]
		listElem[10]=listWork[14]
		listElem[11]=listWork[13]
		listElem[12]=listWork[8]
		listElem[13]=listWork[10]
		listElem[14]=listWork[11]


		fo.write("%d," % iElem)
		for i in range(ElemDict[tElem]):
			fo.write("%d," % listElem[i])
		fo.write("\n")

	if ( tElem == 17  ) : #17=C3D20
#		print("###DBG31:",iPhysicalOld,iPhysical)

		for i in range(ElemDict[tElem]):
			listWork[i]=int(s2[2+nTag+1+i])		#553 307 888 ....
#			print ("###DBG3:","i",i,listWork[i])
		listElem[0]=listWork[0]
		listElem[1]=listWork[1]
		listElem[2]=listWork[2]
		listElem[3]=listWork[3]
		listElem[4]=listWork[5]
		listElem[5]=listWork[6]
		listElem[6]=listWork[7]
		listElem[7]=listWork[8]
		listElem[8]=listWork[9]
		listElem[9]=listWork[11]
		listElem[10]=listWork[13]
		listElem[11]=listWork[9]
		listElem[12]=listWork[16]
		listElem[13]=listWork[18]
		listElem[14]=listWork[19]
		listElem[15]=listWork[17]
		listElem[16]=listWork[10]
		listElem[17]=listWork[12]
		listElem[18]=listWork[14]
		listElem[19]=listWork[16]


		fo.write("%d," % iElem)
		for i in range(ElemDict[tElem]):
			fo.write("%d," % listElem[i])
		fo.write("\n")

fi.close()
fo.close()
#sys.exit() 

print("----------------------------------")
print("%s -> %s      Done" % (mshFile , outFile))


import time
time.sleep(5)
