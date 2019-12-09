
#include <cgx.h>

int write2dolfyn(char *setname, int strings, char **string, Summen *anz, Nodes *node, Faces *face, Elements *e_enqire, Sets *set)
{
    FILE *handle1=NULL;
    char datout[MAX_LINE_LENGTH];
    int  i, j=0, k, n, setNr;
    
    /* check if we have a mesh */
    if(anz->e==0) {
        printf("ERROR: No mesh!\n");
        return(0);
    }
    
    setNr=getSetNr(setname);
    if(setNr>-1)
    {
        /* write vertices */
        sprintf(datout, "%s.vrt", setname);
        handle1 = fopen (datout, "w+b");
        if (handle1==NULL) {
            printf ("\nThe output file \"%s\" could not be opened.\n\n", datout);
            return(-1);
        } else {
            printf (" file %s opened\n",datout);
        }
      
        for(i=0; i<set[setNr].anz_n; i++)
        {
            fprintf (handle1, "%9d      %16.9E%16.9E%16.9E\n", set[setNr].node[i],
                     node[set[setNr].node[i]].nx, node[set[setNr].node[i]].ny,
                     node[set[setNr].node[i]].nz);
        }
        
        fclose(handle1);
        handle1 = NULL;
        
        /* write elements */
        sprintf(datout, "%s.cel", setname);
        handle1 = fopen (datout, "w+b");
        if (handle1==NULL) {
            printf ("\nThe output file \"%s\" could not be opened.\n\n", datout);
            return(-1);
        } else {
            printf (" file %s opened\n",datout);
        }
        
        for(i=0; i<set[setNr].anz_e; i++)
        {
            if (e_enqire[set[setNr].elem[i]].type == 1) {
                /* HEXA8 */
                fprintf (handle1, "%9d      %9d%9d%9d%9d%9d%9d%9d%9d    %5d%5d\n",
                         set[setNr].elem[i],
                         e_enqire[set[setNr].elem[i]].nod[0],
                         e_enqire[set[setNr].elem[i]].nod[1],
                         e_enqire[set[setNr].elem[i]].nod[2],
                         e_enqire[set[setNr].elem[i]].nod[3],
                         e_enqire[set[setNr].elem[i]].nod[4],
                         e_enqire[set[setNr].elem[i]].nod[5],
                         e_enqire[set[setNr].elem[i]].nod[6],
                         e_enqire[set[setNr].elem[i]].nod[7], 1, 1);
            } else if (e_enqire[set[setNr].elem[j]].type == 2) {
                /* PENTA6 */
                fprintf (handle1, "%9d      %9d%9d%9d%9d%9d%9d%9d%9d    %5d%5d\n",
                         set[setNr].elem[i],
                         e_enqire[set[setNr].elem[i]].nod[0],
                         e_enqire[set[setNr].elem[i]].nod[1],
                         e_enqire[set[setNr].elem[i]].nod[2],
                         e_enqire[set[setNr].elem[i]].nod[2],
                         e_enqire[set[setNr].elem[i]].nod[3],
                         e_enqire[set[setNr].elem[i]].nod[4],
                         e_enqire[set[setNr].elem[i]].nod[5],
                         e_enqire[set[setNr].elem[i]].nod[5], 1, 1);
                
            } else if (e_enqire[set[setNr].elem[j]].type == 3) {
                /* TET4 */
                fprintf (handle1, "%9d      %9d%9d%9d%9d%9d%9d%9d%9d    %5d%5d\n",
                         set[setNr].elem[i],
                         e_enqire[set[setNr].elem[i]].nod[0],
                         e_enqire[set[setNr].elem[i]].nod[1],
                         e_enqire[set[setNr].elem[i]].nod[2],
                         e_enqire[set[setNr].elem[i]].nod[2],
                         e_enqire[set[setNr].elem[i]].nod[3],
                         e_enqire[set[setNr].elem[i]].nod[3],
                         e_enqire[set[setNr].elem[i]].nod[3],
                         e_enqire[set[setNr].elem[i]].nod[3], 1, 1);
            } else {
                printf("Element type %d in element %d not supported!\n",
                        e_enqire[set[setNr].elem[j]].type, set[setNr].elem[i]);
            }
        }
        
        fclose(handle1);
        handle1 = NULL;
    }
    
    /* write boundary */
    if (strings > 0) {
        sprintf(datout, "%s.bnd", setname);
        handle1 = fopen (datout, "w+b");
        if (handle1==NULL) {
            printf ("\nThe output file \"%s\" could not be opened.\n\n", datout);
            return(-1);
        } else {
            printf (" file %s opened\n",datout);
        }
    }
    
    j = k = 1;
    n = 0;
    do {
        setNr=getSetNr(string[n + 1]);
        if(setNr>-1) {
            for(i=0; i<set[setNr].anz_f; i++)
            {
                if (face[set[setNr].face[i]].type == 7) {
                    /* TRI3  */
                    fprintf (handle1, "%9d%9d%9d%9d    %5d%5d%10s\n",
                    face[set[setNr].face[i]].nod[0],
                    face[set[setNr].face[i]].nod[1],
                    face[set[setNr].face[i]].nod[2],
                    face[set[setNr].face[i]].nod[2],
                    j, 0, string[n]);
                } else if (face[set[setNr].face[i]].type == 9) {
                    /* QUAD4  */
                    fprintf (handle1, "%9d%9d%9d%9d%9d    %5d%5d%10s\n", k,
                    face[set[setNr].face[i]].nod[0],
                    face[set[setNr].face[i]].nod[1],
                    face[set[setNr].face[i]].nod[2],
                    face[set[setNr].face[i]].nod[3],
                    j, 0, string[n]);
                } else {
                    printf("ERROR: face-type:%d not supported\n",
                            face[set[setNr].face[i]].type);
                }
                k++;
            }
        }
        n += 2;
        j++;
    } while(n < strings);
    
    if (strings > 0) {
        fclose(handle1);
        handle1 = NULL;
    }
    
    return (1);
}
