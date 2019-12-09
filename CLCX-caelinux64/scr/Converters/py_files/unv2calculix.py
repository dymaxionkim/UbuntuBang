from unv2xc import *


# Map of the nodes, between unv files and Calculix
#inp_to_med_C3D10=[3,2,1,4,6,5,7,10,9,8]
inp_to_med_C3D10=[1,3,5,10,2,4,6,7,8,9]
inp_to_med_C3D20=[1,3,5,7,13,15,17,19,2,4,6,8,14,16,18,20,9,10,11,12]
inp_to_med_S6=[1,3,5,2,4,6]
inp_to_med_S8=[1,3,5,7,2,4,6,8]
inp_to_med_C3D15=[1,3,5,10,12,14,2,4,6,11,13,15,7,8,9]


if __name__=='__main__':
    helpmsg=""" UNV2Abaqus: Convert UNV file from Salome to Abaqus INP mesh
        usage: UNV2Abaqus unvfile prefix R|N
        what it does: read unvfile, create an internal FEM object structure
        in memory and writes the prefix.inp file
        If the last argument is R Reduced elements will be used.
        All elements in groups named X_* will just be deleted before conversion
        (usefull to delete all additionnal 1D & 2D elements created by Salome)
        For the moment, all Beam elements are automatically deleted...
        only linear tri, quads, tetra, hexa, wedge 
        quadratic tri, quads, tetra, hexa, wedge
        are supported
    """
    if len(sys.argv)>2:
        unvfile=sys.argv[1]
        prefix=sys.argv[2]
	# if Reduced argument is omitted, default to reduced=R
	if len(sys.argv)>3:
            Reduced=sys.argv[3]
	else:
	    Reduced='R'
	# check 'reduced' argument
        if (Reduced=='R')|(Reduced=='r'):
            types={41:'STRI35',42:'S6',44:'S4R5',45:'S8R',111:'C3D4',112:'C3D6',113:'C3D15',115:'C3D8R',116:'C3D20R',118:'C3D10'}
        elif (Reduced=='N')|(Reduced=='n'):
            types={41:'STRI35',42:'S6',44:'S4R5',45:'S8',111:'C3D4',112:'C3D6',113:'C3D15',115:'C3D8',116:'C3D20',118:'C3D10'}
        else:
            print(helpmsg)
            sys.exit() 
        elemdic={41:[],42:[],44:[],45:[],111:[],112:[],113:[],115:[],116:[],118:[]}
        ls=os.linesep
        # read UNV file in FEM object structure
        UNV=UNVParser(unvfile)
        FEM=UNV.parse()
        # write ELMER UNV file
        # units
        fil=open(prefix + '.inp','w')
        #fil.write('*HEADER'+ls)
        #fil.write('Created by UNV2Abaqus.py,J.Cugnoni,www.caelinux.com,2006' + ls)
        # node         
        fil.write('*NODE, NSET=NALL'+ls)
        for node in FEM.nodes:
            fil.write(('%d,%10e,%10e,%10e'+ls) % (node.id,node.coords[0],node.coords[1],node.coords[2]))
        # elements
        #  first delete all elems from X_* groups
        X_Ids=[]        
        for group in FEM.elemsets:
            if group.name.startswith('X_'):
                X_Ids.extend(group.items)
                FEM.elemsets.remove(group)                
        FEM.nelemsets=len(FEM.elemsets)        
        #  split element list by types
        for elem in FEM.elems:
            if elem.type in types.keys():
                elemdic[elem.type].append(elem)
        # write elements
        for typ in elemdic.keys():
	    # select the appropriate map between the nodes
            if len(elemdic[typ])>0:                
                fil.write('*ELEMENT,TYPE='+types[typ]+',ELSET='+types[typ]+ls)
		if types[typ]=='C3D10':
			themap=inp_to_med_C3D10
			print(themap)
		elif types[typ]=='C3D20':
			themap=inp_to_med_C3D20
                elif types[typ]=='S6':
                        themap=inp_to_med_S6
                elif types[typ]=='S8':
                        themap=inp_to_med_S8
                elif types[typ]=='C3D15':
                        themap=inp_to_med_C3D15
		#if no map is available translate as it is
		else:
			themap=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
                for elem in elemdic[typ]:
                    if not(elem.id in X_Ids):
                        nnodes=elem.nnodes
                        fil.write('%8d,' % elem.id )
                        count=0
                        lst=elem.cntvt
                        for i in range(nnodes):
                            count=count+1
                            if (count<10)&(i<>nnodes-1):
                                fil.write('%8d,' % lst[themap[i]-1])
			    else:
				fil.write('%8d,' % lst[themap[i]-1])
                                fil.write(ls)
				count=0

        # group definitions
        for group in FEM.nodesets:
            fil.write('*NSET,NSET=%s' % group.name)
            count=0            
            lst=group.items                        
            for i in range(group.nitems):
                count=count+1
                if (count<8)&(i<>group.nitems-1):
                    fil.write('%8d,' % (lst.pop(0)))
                else:
                    fil.write(('%8d'+ls) % (lst.pop(0)))
                    count=0
        for group in FEM.elemsets:
            fil.write('*ELSET,ELSET=%s' % group.name)
            count=0            
            lst=group.items                        
            for i in range(group.nitems):
                count=count+1
                if (count<8)&(i<>group.nitems-1):
                    fil.write('%8d,' % (lst.pop(0)))
                else:
                    fil.write(('%8d'+ls) % (lst.pop(0)))
                    count=0
        fil.close()
        print("UNV file converted successfully to Abaqus INP format")
    else:
        print(helpmsg)



import time
time.sleep(3)        
