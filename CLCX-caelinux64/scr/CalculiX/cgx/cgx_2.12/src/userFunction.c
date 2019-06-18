/* --------------------------------------------------------------------  */
/*                          CALCULIX                                     */
/*                   - GRAPHICAL INTERFACE -                             */
/*                                                                       */
/*     A 3-dimensional pre- and post-processor for finite elements       */
/*              Copyright (C) 1996 Klaus Wittig                          */
/*                                                                       */
/*     This program is free software; you can redistribute it and/or     */
/*     modify it under the terms of the GNU General Public License as    */
/*     published by the Free Software Foundation; version 2 of           */
/*     the License.                                                      */
/*                                                                       */
/*     This program is distributed in the hope that it will be useful,   */
/*     but WITHOUT ANY WARRANTY; without even the implied warranty of    */ 
/*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the      */
/*     GNU General Public License for more details.                      */
/*                                                                       */
/*     You should have received a copy of the GNU General Public License */
/*     along with this program; if not, write to the Free Software       */
/*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.         */
/* --------------------------------------------------------------------  */

#include <cgx.h>

extern CopiedNodeSets copiedNodeSets[1];
extern Nodes    *node;                                                  
extern Faces    *face;                                            
extern Elements *e_enqire;                                               
extern Datasets *lcase;
extern Sets     *set;
extern Points   *point;
extern Lines    *line ;
extern Lcmb     *lcmb ;
extern Gsur     *surf ;
extern Gbod     *body ;
extern Nurbl    *nurbl;
extern Nurbs    *nurbs;
extern Shapes   *shape;
void calcFshear(char *name, int lc, double factor, double fstat, double mue, Summen *sum );
void fuss (Points **point_f, Lines **line_f, Sets **set_f, int anzSchaufel, double abstandAE);
void dispratio(Summen *sum, char *string);
void tableNSMS(Summen *sum, char *string);
void checkrbe2(char *name);
double calcMass(char *setname, Summen *sum);
void addModes(char *string, Summen   *sum);
int readWf(char *wfname);
void sendFacesNodes( char *setname );

/* --------------------------------------------------------------------  */
/* Userfunctions                                                         */
/* interface to the mesh, geometry and datasets                          */
/*                                                                       */
/* string:  parameter list from command line                             */
/* sum:     mesh related number of entities                              */
/* sumGeo:  geometrie related number of entities                         */
/*                                                                       */
/*                                                                       */
/*                                                                       */
/* --------------------------------------------------------------------  */
                                                                
void userFunction(char *string, Summen   *sum, SumGeo   *sumGeo )
{                                                               
  int i,j,k,l,n;
  static int ds, enti[12], ncomps, setNr=0;
                                    
  FILE *handle;
  char buf1[MAX_LINE_LENGTH], buf2[MAX_LINE_LENGTH], buf3[MAX_LINE_LENGTH];
  char rec_str[MAX_LINE_LENGTH];                  
  int en, e, f, nod1=0, nod2=0, length;
  double p[6], p01[3], dh, tf, ts, tref, wl, vf, vs, dv, rho, tau, yplus, pf, ps, R=287.1, nue=1.5e-5;

  int lc,sum_lc=0, lmin=0, lmax=MAX_INTEGER, suml_ini=0;

  int profSet;
  double vd[3],v1[3], v2[3], v3[3], vn[3], vF[3], evF[3], a1[3], a2[3], factor, fstat, mue;
  double earea, area, eFres, Fres, vFres[3], t1,t2,dt=0.;
  int nodnr, dispFlag, forcFlag;
  double energy;

  double vx[100], vy[100], val=0.;

  int anzS;
  double absAE;

  int *connectedNode=NULL;
  double *averageValue;
  double lc_time=0., rev=0.;

  // aflib addons
  char buf[12][MAX_LINE_LENGTH];                                            

  int assembleFEM(char *parameters, Summen   *sum, SumGeo   *sumGeo ) { return(0); }
  int assembleFEMVane(char *parameters, Summen   *sum, SumGeo   *sumGeo ){ return(0); }
  int createAF(char *sections, char *parameters, Summen   *sum, SumGeo   *sumGeo ){ return(0); }
  int createAF_nlb(char *sections, char *parameters, Summen   *sum, SumGeo   *sumGeo ){ return(0); }
  int sendFlutter(Summen *sum, char string[2][MAX_LINE_LENGTH] ) { return(0); }                  
  // end

  /* list the implemented user-functions if no parameter was provided for the "call" command */
  if(strlen(string)==0)                         
  {
  // aflib addons
    printf("  aflib functions:\n");
    printf("    assemble <parameter-file>                      // assembles a FEM based on the references in a parameter-file \n");
    printf("    assembleVane <parameter-file>                      // assembles a FEM based on the references in a parameter-file \n");
    printf("    createAF <section-file> <parameter-file>       // creates a solid or hollow airfoil based on a sections- and a parameter-file \n");
    printf("    createAFnlb <section-file> <parameter-file>       // creates a solid or hollow airfoil based on a sections- and a parameter-file \n");
  // end
    printf("  abs <dataset-nr> <dataset-nr> // changes the entities to abs(value) between both datasets\n");
    printf("  add <set> <dataset-nr> <entity-nr>  // just sums up all node-vals (ie. RF)\n");
    printf("  addModes <m1-ds1> <m1-ds2> <factor> <m2-ds1> <m2-ds2> <factor> [<nodenr>]  // searches worst combination of stresses for two modes\n");
    printf("  alpha <set> tref    // calculate the heat-transfer-coefficient alpha\n");
    printf("  average <common-DATASET-name> <dataset-nr> <dataset-nr>  // average all entities between both datasets\n");
    printf("  changeValue <dataset-nr> <dataset-nr> <value>  // changes the 'value' between both datasets\n");
    printf("  calcFres <set> <pres-dataset-nr> <entity-nr> // resulting force of a set of faces based on pressure\n");
    printf("  checkrbe2 <set> // checks the position of dep and indep node of a rbe2 with 2 nodes (nastran)\n");
    printf("  dispratio <set> <set> c|r<dof> // generates a table of data based on the disp-ratio (max_abs_disp(set1)/disp<dof>(set2)\n");
    printf("            r:rec,c:cyl system[works only for cycsym-calcs, else use trfm before] \n");
    printf("  energy <set> <dataset-nr> <dataset-nr>  // calculate Energy = SUM(FORC*DISP) between both datasets\n");
    printf("  facenodes set  // writes an abaqus formated file with elementfaceNr,node1,noden,.. relationship for flutter analyses\n");
    printf("  Fshear <set> <dataset-nr> <factor> <fstat> <mue> //force components normal and co-planar based on RF\n");
    printf("  fuss nblades distAEplane // generates input for prg fuss based on prg bladiator output\n");
    printf("  htc   <set> <dataset-nr> <entity-nr> rauh1 rauh2 lambda1 lambda2 hrd_m\n");
    printf("        // htc [W/m2/K] between two faces in contact. Remarks:\n");
    printf("          loadcase of pressure (MPa!), lambda_fluid=0.06 W/mK (900K) hard wired \n");
    printf("          rauh in m, lambda in W/mK, hrd_m (hardness of softer mat) in Pa!, temp in K \n");
    printf("        -> divide the result by 1e3 to get the usual TESB units [N/s/mm/K]\n");
    printf("  hydro // hydrostatic pressure\n");
    printf("  lift <setin> <setprofil> // for cfd (tbd)\n");
    printf("  mass  // for cfd, integrates the mass of the mesh for all time-steps\n"); 
    printf("  maxVals <common-DATASET-name> // max&min values per ds of a certain name written to maxVals.out\n");                                          
    printf("  move set <file> //moves nodes in set by interpolated values from <file> \n");                                          
    printf("         1st line defines operation (column1-descriptor(xyz) column2-descriptor(xyz) operator(*,+)\n");
    printf("         all other lines: data data\n");
    printf("  nsms <set> ds [ds]  // generates a table of data based on disp of a freq calc\n");
    printf("  readTimeFile time.dat // reads values from file time.dat and places the 'speed' in the textBlock and time in the value of the datasets. The nr of records must match the nr of datasets\n");
    printf("  smooth <set> <dsnr> <entity>  // smooth values by calc average of near nodes\n");
    printf("  strain <dsnr> <dsnr> <nu,E>|file (abq:e,nu,t) // mechanical strain based on stress\n");
    printf("  statistics <common-DATASET-name> <entity-nr> [<time>]|[<common-DATASET-name> <entity-nr> <minval> <maxval>] // creates a new dataset with average and statistical values and stores also the ds-nr of the max and min values\n");
    printf("     Optional regard only datasets with a 'value' of <time> or\n");
    printf("     regard only nodes which have values in the specified range of a second reference dataset-name and entity belonging to the same loadcase as the value from the basic dataset\n");
    printf("  value <set> <value> <dsnr> <entity> // adds <value> in <e>(entity) of <dsnr>(datasetnr)  \n");
    printf("  wireframe <filename> // reads a file of wireframe format\n");
    printf("  worstValues <dsnr> <entity> // <dsnr>: a 'statistics' datasetnr, <entity>: 'statistsics' entity storing dsnr of either max or min value\n");
    printf("  writeDS <set> <dsnr> <entity> ...(up to 6 entities) // writes a file with 'nodeNr' 'entity'..\n");
  }
  // aflib addons
  else if(compare(string, "assembleVane", 12)==12)
  {
    sscanf(string, "%*s %s", buf[0]);
    assembleFEMVane(buf[0], sum, sumGeo);
  }
  else if(compare(string, "assemble", 8)==8)
  {
    sscanf(string, "%*s %s", buf[0]);
    assembleFEM(buf[0], sum, sumGeo);
  }
  else if(compare(string, "createAFnlb", 11)==11)
  {
    sscanf(string, "%*s %s %s", buf[0], buf[1]);
    createAF_nlb(buf[0], buf[1], sum, sumGeo);
  }
  else if(compare(string, "changeValue", 10)==10)                         
  {                                                             
    i=sscanf(string, "%*s %d %d %lf", &lmin, &lmax, &val);
    printf("string:%s\n",string);
    printf("i:%d l:%d %d\n",i, lmin, lmax);
    printf("val :%f %lf\n", val,val);
    for(l=0; l<sum->l; l++) if((l+1>=lmin)&&(l+1<=lmax))                              
    {                                                           
      printf("change value:%f to %f for dataset %d\n", lcase[l].value, val, l+1);
      lcase[l].value=val;
    }                                                      
  }                                                      
  else if(compare(string, "facenodes", 9)==9)
  {
    sscanf(string, "%*s %s", buf[0]);
    sendFacesNodes( buf[0] );
  }
  else if(compare(string, "createAF", 8)==8)
  {
    sscanf(string, "%*s %s %s", buf[0], buf[1]);
    createAF(buf[0], buf[1], sum, sumGeo);
  }
  else if(compare(string, "sendFlutter", 8)==8)
  {
    buf[1][0]=0;
    sscanf(string, "%*s %s %s", buf[0], buf[1]);
    sendFlutter(sum, buf);
  }
  // end
  else if(compare(string, "wireframe", 5)==5)                         
  {                                                             
    sscanf(string, "%*s %s", buf1);
    readWf(buf1);
  }                                                                    
  else if(compare(string, "abs", 3)==3)                         
  {                                                             
    sscanf(string, "%s %d %d", buf1, &lmin, &lmax);

    for(l=0; l<sum->l; l++) if((l+1>=lmin)&&(l+1<=lmax))                              
    {                                                           
      printf("change value to abs(value) for dataset %d\n", l+1);
      /* check if the data of the specified lcase (Dataset) are already available */
      if (!lcase[l].loaded)
      {
       if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
       {
         printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
         return;
       }
       calcDatasets( l, sum, node, lcase );
       recompileEntitiesInMenu(l);
      }
      for(e=0; e<lcase[l].ncomps; e++)
      {
	printf("change value to abs(value) for dataset %d e:%d\n", l+1, e+1);
        lcase[l].max[e]=-MAX_INTEGER;
        lcase[l].min[e]=MAX_INTEGER;
        for(n=0; n<sum->n; n++)
        {
          lcase[l].dat[e][node[n].nr]=abs(lcase[l].dat[e][node[n].nr]);
          if (lcase[l].dat[e][node[n].nr] >  lcase[l].max[e])
          {  lcase[l].max[e]=lcase[l].dat[e][node[n].nr]; lcase[l].nmax[e]=node[n].nr;}
          if (lcase[l].dat[e][node[n].nr] <  lcase[l].min[e])
          {  lcase[l].min[e]=lcase[l].dat[e][node[n].nr]; lcase[l].nmin[e]=node[n].nr;}
        }
      }
    }
  }
  else if(compare(string, "addModes", 4)==4)
  {
    addModes(string, sum);
  }                                                      
  else if(compare(string, "dispratio", 8)==8)
  {
    dispratio(sum, string);                         
  }                                                      
  else if(compare(string, "mass", 4)==4)
  {
    calcMass("all", sum);
  }                                                      
  else if(compare(string, "nsms", 4)==4)
  {
    tableNSMS(sum, string);                         
  }                                                      
  else if(compare(string, "checkrbe2", 5)==5)
  {
    sscanf(string, "%*s %s", buf1);
    checkrbe2(buf1);
  }                                                      
  else if(compare(string, "Fshear", 4)==4)
  {
    factor=1.;
    sscanf(string, "%*s %s %d %lf %lf %lf", buf1, &l, &factor, &fstat, &mue);
    printf("set:|%s| lc:%d factor:%f fstat:%f mue:%f\n", buf1, l, factor, fstat, mue);

    calcFshear(buf1,l, factor, fstat, mue, sum);                         
  }                                                      
  else if(compare(string, "move", 4)==4)                         
  {                                                             
    sscanf(string, "%*s %s %s", buf1, buf2);
    printf("set:|%s| file:|%s|\n", buf1, buf2);

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" ERROR: set:%s does not exist\n", buf1);
      return;
    }

    handle = fopen (buf2, "r");
    if ( handle== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n", buf2); return; }
    else  printf ("\n%s opened\n\n",buf2);

    length = frecord( handle, rec_str);
    sscanf(rec_str, "%s %s %s", buf1, buf2, buf3);
    printf("   data %s, move in %s by operator:%s \nData:\n", buf1, buf2, buf3);

    i=0;
    do
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
      else rec_str[length] =(char)0;
      if (!length) break;
      //printf ("record:%s\n", rec_str);
      sscanf( rec_str, "%lf %lf", &vx[i], &vy[i]);
      printf( "%lf %lf\n", vx[i], vy[i]);
      i++;
    }while(length);
    sum_lc=i;

    /* go over all nodes in set and move them by operator buf3 */
    for(i=0; i<set[setNr].anz_n; i++)
    {
      nodnr=set[setNr].node[i];

      if(buf1[0]=='x') val= intpol( vx, vy, sum_lc, node[nodnr].nx );
      if(buf1[0]=='y') val= intpol( vx, vy, sum_lc, node[nodnr].ny );
      if(buf1[0]=='z') val= intpol( vx, vy, sum_lc, node[nodnr].nz );
      if(buf2[0]=='x')
      {
        if(buf3[0]=='*') node[nodnr].nx*=val;
        else if(buf3[0]=='+') node[nodnr].nx+=val;
        else { printf("ERROR: Operator:%s not known\n", buf3); return; }
      }
      if(buf2[0]=='y')
      {
        if(buf3[0]=='*') node[nodnr].ny*=val;
        else if(buf3[0]=='+') node[nodnr].ny+=val;
        else { printf("ERROR: Operator:%s not known\n", buf3); return; }
      }
      if(buf2[0]=='z')
      {
        if(buf3[0]=='*') node[nodnr].nz*=val;
        else if(buf3[0]=='+') node[nodnr].nz+=val;
        else { printf("ERROR: Operator:%s not known\n", buf3); return; }
      }
      //printf("n:%d dy:%f y:%f\n",nodnr, val,node[nodnr].ny);
    }
  }
  else if(compare(string, "readTimeFile", 9)==9)                         
  {                                                             
    sscanf(string, "%*s %s", buf1);
    printf("file:%s\n", buf1);

    handle = fopen (buf1, "r");
    if ( handle== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n", buf1); return; }
    else  printf ("\n%s opened\n\n",buf1);

    n=0;
    lc=0;
    do
    {
      length = frecord( handle, rec_str);
      if (rec_str[length] == (char)EOF) break;
      else rec_str[length] =(char)0;
      if (!length) break;
      //printf ("record:%s\n", rec_str);
      sscanf( rec_str, "%s %s %s %s %s %s", buf[0], buf[1], buf[2], buf[3], buf[4], buf[5]);
      for (i=0; i<132; i++) if (buf[3][i]=='D') buf[3][i]='e'; 
      lc_time=atof(buf[3]);
      rev=atof(buf[5]);
      printf ("%d time:%f speed:%f \n", n, lc_time, rev );

      sprintf(lcase[lc].dataset_text, "%f", rev);
      lcase[lc].value=lc_time;
      while(sum->l>lc)
      {
        lc++;
        if(lcase[lc].step_number==lcase[lc-1].step_number)
        {
          sprintf(lcase[lc].dataset_text, "%f", rev);
          lcase[lc].value=lc_time;
	}
        else break;
      }

      n++;
    }while(length);
    sum_lc=n;
    if(lc!=sum->l) printf("WARNING: sum of datasets:%d different than sum of records in file:%d\n",sum->l,lc);
  }
  else if(compare(string, "add", 3)==3)                         
  {                                                             
    sscanf(string, "%*s %s %d %d", buf1, &l, &e);
    printf("set:%s ds:%d e:%d\n", buf1, l,e);
    l--; e--;


    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" calcFres: set:%s does not exist\n", buf1);
      return;
    }
    if(l>=sum->l)
    {
      printf (" calcFres: dataset:%d does not exist\n", l+1);
      return;
    }
    if(e>=lcase[l].ncomps)
    {
      printf (" calcFres: entity:%d does not exist\n", e+1);
      return;
    }

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[l].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
     {
       printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
       return;
     }
     calcDatasets( l, sum, node, lcase );
     recompileEntitiesInMenu(l);
    }

    Fres=0.;
    for(i=0; i<set[setNr].anz_n; i++)
    {
        Fres+=lcase[l].dat[e][set[setNr].node[i]];
    }
    printf("\n Fres:%lf\n", Fres);
  }

  else if(compare(string, "energy", 6)==6)                         
  {                                                             
    handle = fopen ("energy.out", "w");
    sscanf(string, "%s %s %d %d", buf1, buf2, &lmin, &lmax);
    printf("calculate Energy = SUM(FORC*DISP) for nodes in set:%s between dataset %d and %d\n", buf2, lmin, lmax);
    setNr=getSetNr(buf2);
    suml_ini=sum->l;
    energy=0.;
    for(i=0; i<set[setNr].anz_n; i++)
    {
      nodnr=set[setNr].node[i];
      vd[0]=vd[1]=vd[2]=0;
      v1[0]=v1[1]=v1[2]=0;
      a1[0]=a1[1]=a1[2]=0;
      t1=0.;
      dispFlag=forcFlag=0;
      for(l=0; l<suml_ini; l++) if((l+1>=lmin)&&(l+1<=lmax))                              
      {                                                           
        if (compare(lcase[l].name,"DISP",4)==4)
        { 
          /* check if the data of the specified lcase (Dataset) are already available */
          if (!lcase[l].loaded)
          {
           if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
           {
             printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
             return;
           }
           calcDatasets( l, sum, node, lcase );
           recompileEntitiesInMenu(l);
          }

          dispFlag=1;
          // printf("DISP of LC[%d]: %s \n", l, lcase[l].name);
          v2[0]=lcase[l].dat[0][nodnr];
          v2[1]=lcase[l].dat[1][nodnr];
          v2[2]=lcase[l].dat[2][nodnr];
          v_result(v1,v2,vn);
	  // printf("disp of node:%d %f %f %f\n", nodnr, vn[0], vn[1], vn[2]);
          vd[0]=(v1[0]+v2[0])*.5; 
          vd[1]=(v1[1]+v2[1])*.5; 
          vd[2]=(v1[2]+v2[2])*.5;
          v1[0]=v2[0];
          v1[1]=v2[1];
          v1[2]=v2[2];
          t2=lcase[l].value;
          dt=(t2+t1)*.5;
          t1=t2;
	}
        if (compare(lcase[l].name,"FORC",4)==4)
        {
          /* check if the data of the specified lcase (Dataset) are already available */
          if (!lcase[l].loaded)
          {
           if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
           {
             printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
             return;
           }
           calcDatasets( l, sum, node, lcase );
           recompileEntitiesInMenu(l);
          }

          forcFlag=1;
          // printf("FORC of LC[%d]: %s \n", l, lcase[l].name);
          a2[0]=lcase[l].dat[0][nodnr]; 
          a2[1]=lcase[l].dat[1][nodnr]; 
          a2[2]=lcase[l].dat[2][nodnr]; 
          vF[0]=(a1[0]+a2[0])*.5; 
          vF[1]=(a1[1]+a2[1])*.5; 
          vF[2]=(a1[2]+a2[2])*.5; 
	     printf("forc of node:%d %f %f %f\n", nodnr, a1[0], a1[1], a1[2]);
	     printf("forc of node:%d %f %f %f\n", nodnr, a2[0], a2[1], a2[2]);
	     printf("forc of node:%d %f %f %f\n", nodnr, vF[0], vF[1], vF[2]);
          a1[0]=a2[0];
          a1[1]=a2[1];
          a1[2]=a2[2];
	}
        if ((dispFlag)&&(forcFlag))
	{
          dispFlag=forcFlag=0;
          energy+=v_sprod(vF,vn);
          //energy+=vF[2]*vn[2];
          printf("calc energy:%lf\n", energy);
          fprintf(handle, "%d %lf dav: %lf %lf %lf dd: %lf %lf %lf F: %lf %lf %lf energy: %lf\n",l+1,dt , vd[0],vd[1],vd[2], vn[0],vn[1],vn[2], vF[0],vF[1],vF[2], energy);
	}
      }
    }
    fclose(handle);
  }

  else if(compare(string, "average", 7)==7)                         
  {                                                             
    sscanf(string, "%s %s %d %d", buf1, buf2, &lmin, &lmax);
    printf("search:%s average all entities between dataset %d and %d\n", buf2, lmin, lmax);
    suml_ini=sum->l;
    for(l=0; l<suml_ini; l++)                                     
    {                                                           
      if ((compare(lcase[l].name,buf2,strlen(buf2))==strlen(buf2))&&(l+1>=lmin)&&(l+1<=lmax))
      {
        /* add all entities in a new dataset and divide by the nr of lcases */

        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }

        if(sum_lc==0)
        {
          /* create a new dataset */
          if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (sum->l+2) * sizeof(Datasets))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
  
          if( (compare( lcase[l].name, "STRESS", 6) == 6)||(compare( lcase[l].name, "STRAIN", 6) == 6)||( compare( lcase[l].name, "TOSTRAIN", 6) == 6)||( compare( lcase[l].name, "MESTRAIN", 6) == 6)||( compare( lcase[l].name, "ELSTRAIN", 6) == 6)||( compare( lcase[l].name, "ZZS", 3) == 3)) lcase[sum->l].ncomps=6;
          else if ( lcase[l].ictype[0] == 2) lcase[sum->l].ncomps=3;
          else if ( lcase[l].ictype[0]== 12) lcase[sum->l].ncomps=6;  
          else lcase[sum->l].ncomps = lcase[l].ncomps;
          lcase[sum->l].irtype = lcase[l].irtype;
          lcase[sum->l].npheader  = 0 ;
          lcase[sum->l].value  = 0. ;
          strcpy(lcase[sum->l].name,lcase[l].name) ;
          sprintf( lcase[sum->l].dataset_name,"AV:%d:%d",lmin,lmax);
          strcpy(lcase[sum->l].dataset_text,"");
          strcpy(lcase[sum->l].analysis_name,lcase[l].analysis_name);
          lcase[sum->l].step_number=lcase[sum->l-1].step_number+1;
          lcase[sum->l].analysis_type=lcase[l].analysis_type;
	  lcase[sum->l].loaded = 1;
          lcase[sum->l].fileptr = NULL;
    
          if ( (lcase[sum->l].nmax = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].nmin = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].max = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].min = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].dat = (float **)malloc( lcase[sum->l].ncomps * sizeof(float *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].compName = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].icname = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            if ( (lcase[sum->l].dat[i] = (float *)malloc( (sum->nmax+1) * sizeof(float))) == NULL )
              printf("\n\n ERROR: malloc failure\n\n" );	               
            if ( (lcase[sum->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            if ( (lcase[sum->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            lcase[sum->l].max[i]=-MAX_INTEGER;
            lcase[sum->l].min[i]=MAX_INTEGER;
            for(n=0; n<=sum->nmax; n++)
              lcase[sum->l].dat[i][n]=0.;
          }
          if ( (lcase[sum->l].menu = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].ictype = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].icind1 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].icind2 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].iexist = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            lcase[sum->l].menu[i] = lcase[l].menu[i];
            lcase[sum->l].ictype[i] = lcase[l].ictype[i];
            lcase[sum->l].icind1[i] = lcase[l].icind1[i];
            lcase[sum->l].icind2[i] = lcase[l].icind2[i];
            lcase[sum->l].iexist[i] = lcase[l].iexist[i];
            strcpy( lcase[sum->l].compName[i], lcase[l].compName[i]);
          }
          sum->l++;
        }

        for(i=0; i<lcase[suml_ini].ncomps; i++)
        {
          for(n=0; n<sum->n; n++)
          {
            lcase[suml_ini].dat[i][node[n].nr]+=lcase[l].dat[i][node[n].nr];
          }
        }

        sum_lc++;

        printf(" %d read dataset:%d %s %lf\n",sum_lc, l+1, lcase[l].name, lcase[l].value);

      }
    }
    for(i=0; i<lcase[suml_ini].ncomps; i++)
    {
      for(n=0; n<sum->n; n++)
      {
        lcase[suml_ini].dat[i][node[n].nr]/=sum_lc;
        if (lcase[suml_ini].dat[i][node[n].nr] >  lcase[suml_ini].max[i])
        {  lcase[suml_ini].max[i]=lcase[suml_ini].dat[i][node[n].nr]; lcase[suml_ini].nmax[i]=node[n].nr;}
        if (lcase[suml_ini].dat[i][node[n].nr] <  lcase[suml_ini].min[i])
        {  lcase[suml_ini].min[i]=lcase[suml_ini].dat[i][node[n].nr]; lcase[suml_ini].nmin[i]=node[n].nr;}
      }
    }
    calcDatasets( suml_ini, sum, node, lcase );
    recompileEntitiesInMenu(suml_ini);
  }
  else if(compare(string, "strain", 6)==6)
  {
    int sum_l, sum_a=0, lct;
    double nu, Et, Gt;
    double at[100],anu[100], aEt[100];

    // 1st ds, last ds, file:t,nu,e or nu,E
    strcpy(buf1,"STRESS");
    sscanf(string, "%*s %d %d %s", &lmin, &lmax, buf2);
    // file or values?
    handle = fopen (buf2, "r");
    if ( handle== NULL )
    {
      length=sscanf(buf2,"%lf,%lf",&nu, &Et);
      if(!length) { printf ("\nThe input file \"%s\" could not be opened.\n\n", buf2); return; }
    }
    else
    {
      printf ("\n%s opened, read material propperties (E,nu,t)\n\n",buf2);
      i=0;
      do
      {
        length = frecord( handle, rec_str);
        if (rec_str[length] == (char)EOF) break;
        else rec_str[length] =(char)0;
        if (!length) break;
        sscanf( rec_str, "%lf,%lf,%lf", &aEt[i], &anu[i], &at[i]);
        printf( "%lf %lf %lf\n", at[i], anu[i], aEt[i]);
        i++;
      }while(length);
      if(i<100) sum_a=i; else { printf ("\ntoo much data in %s\n\n", buf2); return; }
    }

    if(sum->l<lmax) lmax=sum->l;
    suml_ini=sum->l;
    strcpy(buf2,"NDTEMP");

    for(l=lmin-1; l<lmax; l++)
    {
      if (compare(lcase[l].name,buf1,strlen(buf1))==strlen(buf1))
      {
        printf("calc strain for ds:%d %s\n",l+1,lcase[l].name);
        /* add entity en in a new dataset and divide by the nr of datasets */

        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1)
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1);
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }

        /* eventually search the related dataset */
        lc=l;
        if(lc) { for (lct=lc-1; lct>=0; lct--) { if(lcase[lct].step_number!=lcase[lc].step_number) break; } lct++; }
        else lct=1;
        while((lct<sum->l)&&(lcase[lct].step_number==lcase[lc].step_number))
        {
          if (compare(lcase[lct].name,buf2,strlen(buf2))==strlen(buf2))
          {
            if (!lcase[lct].loaded)
            {
              if( pre_readfrdblock(copiedNodeSets , lct, sum, node, lcase )==-1)
              {
                printf(" ERROR in nodalDataset: Could not read data for Dataset:%d\n", lct+1);
                return;
              }
              calcDatasets( lct, sum, node, lcase );
              recompileEntitiesInMenu(lct);
            }
            break;
          }
          lct++;
        }
        if(lct==sum->l)
        {
          printf (" related temperature dataset does not exist\n");
          return;
        }
        printf("  related temps from ds:%d %s\n",lct+1,lcase[lct].name);

        for(i=0; i<6; i++) lcase[l].compName[i][0]='E';
        sprintf(lcase[l].name,"STRAIN") ;
        lcase[l].ncomps = 6;
        lcase[l].max[i]=-MAX_INTEGER;
        lcase[l].min[i]=MAX_INTEGER;
        sum_l=l;

        /*
           Poisson's Ratio
            nu=(Et/2Gt) -1
            G=E/2(nu+1)
        */
        for(n=0; n<sum->n; n++) if(!node[node[n].nr].pflag)
        {
          nu = intpol( at,  anu, sum_a, lcase[lct].dat[0][node[n].nr]);
          Et = intpol( at,  aEt, sum_a, lcase[lct].dat[0][node[n].nr]);
          Gt=Et/2./(nu+1.);
          for(i=0; i<6; i++) p[i]=lcase[l].dat[i][node[n].nr];
          lcase[sum_l].dat[0][node[n].nr]=(p[0]-nu*(p[1]+p[2]))/Et;
          lcase[sum_l].dat[1][node[n].nr]=(p[1]-nu*(p[2]+p[0]))/Et;
          lcase[sum_l].dat[2][node[n].nr]=(p[2]-nu*(p[0]+p[1]))/Et;
          lcase[sum_l].dat[3][node[n].nr]=p[3]/Gt/2.;
          lcase[sum_l].dat[4][node[n].nr]=p[4]/Gt/2.;
          lcase[sum_l].dat[5][node[n].nr]=p[5]/Gt/2.;
        }

        for(i=0; i<6; i++)
        {
          for(n=0; n<sum->n; n++)
          {
            if (lcase[sum_l].dat[i][node[n].nr] >  lcase[sum_l].max[i])
            {  lcase[sum_l].max[i]=lcase[sum_l].dat[i][node[n].nr]; lcase[sum_l].nmax[i]=node[n].nr;}
            if (lcase[sum_l].dat[i][node[n].nr] <  lcase[sum_l].min[i])
            {  lcase[sum_l].min[i]=lcase[sum_l].dat[i][node[n].nr]; lcase[sum_l].nmin[i]=node[n].nr;}
          }
        }
        calcDatasets( sum_l, sum, node, lcase );
        recompileEntitiesInMenu(sum_l);
      }
    }
  }
  else if(compare(string, "maxVals", 7)==7)                         
  {                                                             
    handle = fopen ("maxVals.out", "w");
    if (handle==NULL)
    {
      printf (" ERROR: The input file \"%s\" could not be opened.\n\n", "maxVals.out");
      return;
    }
    sscanf(string, "%s %s", buf1, buf2);
    printf("search:%s write in maxVals.out:\n Dataset Value Max(0) Min(0) Max(1) Min(1) ..(entities)\n", buf2);
    for(l=0; l<sum->l; l++)                                     
    {                                                           
      if (compare(lcase[l].name,buf2,strlen(buf2))==strlen(buf2))
      {
        printf("%d %lf\n",l+1, lcase[l].value);
        fprintf(handle, "%d %lf ",l+1, lcase[l].value);

        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }
        for(i=0; i<lcase[l].ncomps; i++)
        {
          fprintf(handle, " %lf %lf", lcase[l].max[i], lcase[l].min[i]);
        }
        fprintf(handle, "\n");
      }
    }
    fclose(handle);
  }

  else if(compareStrings(string, "hydro")>0)                         
  {                                                             
    /* calculate the hydrostatic stress and the deviator */     
    for(l=0; l<sum->l; l++)                                     
    {                                                           
      /* use only stresses */
      if (compare(lcase[l].name,"STRESS",6)==6)
      {
        printf(" calc hydrostatic stress and deviator for LC[%d]: %s \n", l, lcase[l].name); 

        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }
  
        /* create a new dataset */
        if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (sum->l+1) * sizeof(Datasets))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );

        lcase[sum->l].ncomps = 4;
        lcase[sum->l].irtype = 1;
        lcase[sum->l].npheader  = 0 ;
        lcase[sum->l].value  = lcase[l].value ;
  
        if ( (lcase[sum->l].nmax = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].nmin = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].max = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].min = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].dat = (float **)malloc( lcase[sum->l].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].compName = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icname = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        for(i=0; i<lcase[sum->l].ncomps; i++)
        {
          if ( (lcase[sum->l].dat[i] = (float *)malloc( (sum->nmax+1) * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );	               
          if ( (lcase[sum->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcase[sum->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          lcase[sum->l].max[i]=-MAX_INTEGER;
          lcase[sum->l].min[i]=MAX_INTEGER;
        }
        if ( (lcase[sum->l].menu = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].ictype = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icind1 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icind2 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].iexist = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
  
        for(i=0; i<lcase[sum->l].ncomps; i++)
        {
          lcase[sum->l].menu[i] = 1;
          lcase[sum->l].ictype[i] = 1;
          lcase[sum->l].icind1[i] = i+1;
          lcase[sum->l].icind2[i] = 0;
          lcase[sum->l].iexist[i] = 0;
        }
        sprintf( lcase[sum->l].name, "HYD&DEV");
        strcpy( lcase[sum->l].compName[0], "SM");
        strcpy( lcase[sum->l].compName[1], "SXX-SM");
        strcpy( lcase[sum->l].compName[2], "SYY-SM");
        strcpy( lcase[sum->l].compName[3], "SZZ-SM");
  
        for(n=0; n<sum->n; n++)
        {
          /* calc average Princ Stress sigm */
          lcase[sum->l].dat[0][node[n].nr]=
            (lcase[l].dat[8][node[n].nr]+lcase[l].dat[9][node[n].nr]+lcase[l].dat[10][node[n].nr])/3.;
         
          /* calc the deviator */
          for(i=0; i<3; i++)
            lcase[sum->l].dat[i+1][node[n].nr]=lcase[l].dat[i][node[n].nr]-lcase[sum->l].dat[0][node[n].nr];

          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            if (lcase[sum->l].dat[i][node[n].nr] >  lcase[sum->l].max[i])
            {  lcase[sum->l].max[i]=lcase[sum->l].dat[i][node[n].nr]; lcase[sum->l].nmax[i]=node[n].nr; }
            if (lcase[sum->l].dat[i][node[n].nr] <  lcase[sum->l].min[i])
            {  lcase[sum->l].min[i]=lcase[sum->l].dat[i][node[n].nr]; lcase[sum->l].nmin[i]=node[n].nr; }
          }
        }
        sum->l++;
      }
    }
  }

  else if(compare(string, "alpha", 5)==5)                         
  {                                                             
    sscanf(string, "%s %s %lf", buf1, buf2, &tref);
    printf("|%s| set:|%s| |%lf|\n", buf1, buf2,  tref);

    setNr=getSetNr(buf2);
    if (setNr<0)
    {
      printf (" alpha: set:%s does not exist\n", buf2);
      return;
    }

    /* calculate the heat-transver-coefficient alpha */     
    for(l=0; l<sum->l; l++)                                     
    {                                                           
      /* use only duns-results */
      if (compare(lcase[l].name,"duns",4)==4)
      {
        printf(" calc the heat-transver-coefficient alpha for LC[%d]: %s \n", l, lcase[l].name); 
  
        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }

        /* create a new dataset */
        if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (sum->l+1) * sizeof(Datasets))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );

        lcase[sum->l].ncomps = 7;
        lcase[sum->l].irtype = 1;
        lcase[sum->l].npheader  = 0 ;
        lcase[sum->l].value  = lcase[l].value ;
  
        if ( (lcase[sum->l].nmax = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].nmin = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].max = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].min = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].dat = (float **)malloc( lcase[sum->l].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].compName = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icname = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        for(i=0; i<lcase[sum->l].ncomps; i++)
        {
          if ( (lcase[sum->l].dat[i] = (float *)malloc( (sum->nmax+1) * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );	               
          if ( (lcase[sum->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcase[sum->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          lcase[sum->l].max[i]=-MAX_INTEGER;
          lcase[sum->l].min[i]=MAX_INTEGER;
        }
        if ( (lcase[sum->l].menu = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].ictype = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icind1 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icind2 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].iexist = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
  
        for(i=0; i<lcase[sum->l].ncomps; i++)
        {
          lcase[sum->l].menu[i] = 1;
          lcase[sum->l].ictype[i] = 1;
          lcase[sum->l].icind1[i] = i+1;
          lcase[sum->l].icind2[i] = 0;
          lcase[sum->l].iexist[i] = 0;
        }
        sprintf( lcase[sum->l].name, "ALPHA");
        strcpy( lcase[sum->l].compName[0], "alpha");
        strcpy( lcase[sum->l].compName[1], "dh   ");
        strcpy( lcase[sum->l].compName[2], "dt   ");
        strcpy( lcase[sum->l].compName[3], "dv   ");
        strcpy( lcase[sum->l].compName[4], "tau   ");
        strcpy( lcase[sum->l].compName[5], "yplus");
        strcpy( lcase[sum->l].compName[6], "wl ");
  
        for(i=0; i<lcase[sum->l].ncomps; i++) for(n=0; n<sum->n; n++) lcase[sum->l].dat[i][n]=0.;

        for(f=0; f<set[setNr].anz_f; f++)
        {
          /* calc the heigh dh of the element at the face nodes */
          e=face[set[setNr].face[f]].elem_nr;
          for(n=0; n<4; n++)
          {
            nod1=face[f].nod[n];

            /* search the opposide node in the free-stream */
            for(en=0; en<8; en++)
            {
              /* ((ni[j]==1)||(ni[j]==2)||(ni[j]==3)||(ni[j]==4)) f[0] */
              if (face[f].nr==0)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  nod2=e_enqire[e].nod[en+4];
                  break;
                }
              }
              /* ((ni[j]==5)||(ni[j]==8)||(ni[j]==7)||(ni[j]==6)) f[1] */
              if (face[f].nr==1)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  nod2=e_enqire[e].nod[en-4];
                  break;
                }
              }
              /* ((ni[j]==1)||(ni[j]==5)||(ni[j]==6)||(ni[j]==2)) f[2] */
              if (face[f].nr==2)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==0) nod2=e_enqire[e].nod[3];
                  if(en==1) nod2=e_enqire[e].nod[2];
                  if(en==5) nod2=e_enqire[e].nod[6];
                  if(en==4) nod2=e_enqire[e].nod[7];
                  break;
                }
              }
              /* ((ni[j]==2)||(ni[j]==6)||(ni[j]==7)||(ni[j]==3)) f[3] */
              if (face[f].nr==3)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==1) nod2=e_enqire[e].nod[0];
                  if(en==5) nod2=e_enqire[e].nod[4];
                  if(en==6) nod2=e_enqire[e].nod[7];
                  if(en==2) nod2=e_enqire[e].nod[3];
                  break;
                }
              }
              /* ((ni[j]==3)||(ni[j]==7)||(ni[j]==8)||(ni[j]==4)) f[4] */
              if (face[f].nr==4)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==2) nod2=e_enqire[e].nod[1];
                  if(en==6) nod2=e_enqire[e].nod[5];
                  if(en==7) nod2=e_enqire[e].nod[4];
                  if(en==3) nod2=e_enqire[e].nod[0];
                  break;
                }
              }
              /* ((ni[j]==4)||(ni[j]==8)||(ni[j]==5)||(ni[j]==1)) f[5] */
              if (face[f].nr==5)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==3) nod2=e_enqire[e].nod[2];
                  if(en==7) nod2=e_enqire[e].nod[6];
                  if(en==4) nod2=e_enqire[e].nod[5];
                  if(en==0) nod2=e_enqire[e].nod[1];
                  break;
                }
              }
            }
            v_result(&node[nod1].nx, &node[nod2].nx, p01);
            dh=v_betrag(p01);

            /* get the pres p and temps ts (stream) tf (face) and the thermal conductivity wl */
            pf=lcase[l].dat[0][nod1];
            ps=lcase[l].dat[0][nod2];
            tf=lcase[l].dat[1][nod1];
            ts=lcase[l].dat[1][nod2];
            wl=(lcase[l].dat[6][nod1]+lcase[l].dat[6][nod2])*.5;
            /* calc the heat-transfer-coefficient alpha */

            /* tangential velocities */
            vf=(lcase[l].dat[5][nod1]+lcase[l].dat[5][nod1])*.5;
            vs=(lcase[l].dat[5][nod2]+lcase[l].dat[5][nod2])*.5;
            dv=vs-vf;
            rho=(pf+ps)/R/(ts+tf);
            tau=nue*rho *(dv/dh);
            yplus=dh*sqrt(tau*rho)/(nue*rho);

            lcase[sum->l].dat[0][nod1]=wl/dh*(ts-tf)/(tref-tf);
            lcase[sum->l].dat[1][nod1]=dh;
            lcase[sum->l].dat[2][nod1]=ts-tf;
            lcase[sum->l].dat[3][nod1]=dv;
            lcase[sum->l].dat[4][nod1]=tau;
            lcase[sum->l].dat[5][nod1]=yplus;
            lcase[sum->l].dat[6][nod1]=wl;
          }
        }

        for(n=0; n<sum->n; n++)
        {
          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            if (lcase[sum->l].dat[i][node[n].nr] >  lcase[sum->l].max[i])
            {  lcase[sum->l].max[i]=lcase[sum->l].dat[i][node[n].nr]; lcase[sum->l].nmax[i]=node[n].nr; }
            if (lcase[sum->l].dat[i][node[n].nr] <  lcase[sum->l].min[i])
            {  lcase[sum->l].min[i]=lcase[sum->l].dat[i][node[n].nr]; lcase[sum->l].nmin[i]=node[n].nr; }
          }
        }
        sum->l++;
        sum->olc++;
      }
    }
  }

  else if(compare(string, "lift", 4)==4)                         
  {                                                             
    sscanf(string, "%s %s", buf1, buf2);
    printf("|%s| set:|%s|\n", buf1, buf2);

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" lift: set:%s does not exist\n", buf1);
      return;
    }

    profSet=getSetNr(buf2);
    if (profSet<0)
    {
      printf (" lift: set:%s does not exist\n", buf2);
      return;
    }

    /* calculate  lift and drag */     
    for(l=0; l<sum->l; l++)                                     
    {                                                           
      /* use only duns-results */
      if (compare(lcase[l].name,"duns",4)==4)
      {
        printf(" calc lift and drag for LC[%d]: %s \n", l, lcase[l].name); 
  
        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }
  
        /* create a new dataset */
        if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (sum->l+1) * sizeof(Datasets))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );

        lcase[sum->l].ncomps = 7;
        lcase[sum->l].irtype = 1;
        lcase[sum->l].npheader  = 0 ;
        lcase[sum->l].value  = lcase[l].value ;
  
        if ( (lcase[sum->l].nmax = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].nmin = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].max = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].min = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].dat = (float **)malloc( lcase[sum->l].ncomps * sizeof(float *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].compName = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icname = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        for(i=0; i<lcase[sum->l].ncomps; i++)
        {
          if ( (lcase[sum->l].dat[i] = (float *)malloc( (sum->nmax+1) * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );	               
          if ( (lcase[sum->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          if ( (lcase[sum->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
             printf("\n\n ERROR: malloc failed\n\n" );
          lcase[sum->l].max[i]=-MAX_INTEGER;
          lcase[sum->l].min[i]=MAX_INTEGER;
        }
        if ( (lcase[sum->l].menu = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].ictype = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icind1 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].icind2 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
        if ( (lcase[sum->l].iexist = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
          printf("\n\n ERROR: malloc failure\n\n" );
  
        for(i=0; i<lcase[sum->l].ncomps; i++)
        {
          lcase[sum->l].menu[i] = 1;
          lcase[sum->l].ictype[i] = 1;
          lcase[sum->l].icind1[i] = i+1;
          lcase[sum->l].icind2[i] = 0;
          lcase[sum->l].iexist[i] = 0;
        }
        sprintf( lcase[sum->l].name, "LIFT");
        strcpy( lcase[sum->l].compName[0], "ax   ");
        strcpy( lcase[sum->l].compName[1], "ay   ");
        strcpy( lcase[sum->l].compName[2], "az   ");
        strcpy( lcase[sum->l].compName[3], "A    ");
        strcpy( lcase[sum->l].compName[4], "tau  ");
        strcpy( lcase[sum->l].compName[5], "     ");
        strcpy( lcase[sum->l].compName[6], "     ");
  
        for(i=0; i<lcase[sum->l].ncomps; i++) for(n=0; n<sum->n; n++) lcase[sum->l].dat[i][n]=0.;

        for(f=0; f<set[profSet].anz_f; f++)
        {
          e=face[f].elem_nr;

          /* calc the area per node in xyz, each node uses 0.25*area */
          v_result( &node[face[f].nod[0]].nx, &node[face[f].nod[1]].nx, v1);
          v_result( &node[face[f].nod[0]].nx, &node[face[f].nod[3]].nx, v2);
          v_prod( v1, v2, a1 );
          v_result( &node[face[f].nod[2]].nx, &node[face[f].nod[3]].nx, v1);
          v_result( &node[face[f].nod[2]].nx, &node[face[f].nod[1]].nx, v2);
          v_prod( v1, v2, a2 );
          lcase[sum->l].dat[0][face[f].nod[0]]=(a1[0]+a2[0])*.25;
          lcase[sum->l].dat[1][face[f].nod[0]]=(a1[1]+a2[1])*.25;
          lcase[sum->l].dat[2][face[f].nod[0]]=(a1[2]+a2[2])*.25;

          /* calc the heigh dh of the element at the face nodes */
          for(n=0; n<4; n++)
          {
            nod1=face[set[setNr].face[f]].nod[n];

            /* search the opposide node in the free-stream */
            for(en=0; en<8; en++)
            {
              /* ((ni[j]==1)||(ni[j]==2)||(ni[j]==3)||(ni[j]==4)) f[0] */
              if (face[f].nr==0)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  nod2=e_enqire[e].nod[en+4];
                  break;
                }
              }
              /* ((ni[j]==5)||(ni[j]==8)||(ni[j]==7)||(ni[j]==6)) f[1] */
              if (face[f].nr==1)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  nod2=e_enqire[e].nod[en-4];
                  break;
                }
              }
              /* ((ni[j]==1)||(ni[j]==5)||(ni[j]==6)||(ni[j]==2)) f[2] */
              if (face[f].nr==2)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==0) nod2=e_enqire[e].nod[3];
                  if(en==1) nod2=e_enqire[e].nod[2];
                  if(en==5) nod2=e_enqire[e].nod[6];
                  if(en==4) nod2=e_enqire[e].nod[7];
                  break;
                }
              }
              /* ((ni[j]==2)||(ni[j]==6)||(ni[j]==7)||(ni[j]==3)) f[3] */
              if (face[f].nr==3)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==1) nod2=e_enqire[e].nod[0];
                  if(en==5) nod2=e_enqire[e].nod[4];
                  if(en==6) nod2=e_enqire[e].nod[7];
                  if(en==2) nod2=e_enqire[e].nod[3];
                  break;
                }
              }
              /* ((ni[j]==3)||(ni[j]==7)||(ni[j]==8)||(ni[j]==4)) f[4] */
              if (face[f].nr==4)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==2) nod2=e_enqire[e].nod[1];
                  if(en==6) nod2=e_enqire[e].nod[5];
                  if(en==7) nod2=e_enqire[e].nod[4];
                  if(en==3) nod2=e_enqire[e].nod[0];
                  break;
                }
              }
              /* ((ni[j]==4)||(ni[j]==8)||(ni[j]==5)||(ni[j]==1)) f[5] */
              if (face[f].nr==5)
              {
                if(nod1==e_enqire[e].nod[en])
                {
                  if(en==3) nod2=e_enqire[e].nod[2];
                  if(en==7) nod2=e_enqire[e].nod[6];
                  if(en==4) nod2=e_enqire[e].nod[5];
                  if(en==0) nod2=e_enqire[e].nod[1];
                  break;
                }
              }
            }
            v_result(&node[nod1].nx, &node[nod2].nx, p01);
            dh=v_betrag(p01);

            /* get the pres p and temps ts (stream) tf (face) and the thermal conductivity wl */
            pf=lcase[l].dat[0][nod1];
            ps=lcase[l].dat[0][nod2];
            tf=lcase[l].dat[1][nod1];
            ts=lcase[l].dat[1][nod2];
            wl=(lcase[l].dat[6][nod1]+lcase[l].dat[6][nod2])*.5;
            /* calc the heat-transfer-coefficient alpha */

            /* tangential velocities */
            vf=(lcase[l].dat[5][nod1]+lcase[l].dat[5][nod1])*.5;
            vs=(lcase[l].dat[5][nod2]+lcase[l].dat[5][nod2])*.5;
            dv=vs-vf;
            rho=(pf+ps)/R/(ts+tf);
            tau=nue*rho *(dv/dh);
            yplus=dh*sqrt(tau*rho)/(nue*rho);

            lcase[sum->l].dat[3][nod1]+=lcase[sum->l].dat[0][nod1]*pf;
            lcase[sum->l].dat[4][nod1]+=lcase[sum->l].dat[1][nod1]*pf;
            lcase[sum->l].dat[5][nod1]+=lcase[sum->l].dat[2][nod1]*pf;
            lcase[sum->l].dat[6][nod1]=wl;
          }
        }

        for(n=0; n<sum->n; n++)
        {
          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            if (lcase[sum->l].dat[i][node[n].nr] >  lcase[sum->l].max[i])
            {  lcase[sum->l].max[i]=lcase[sum->l].dat[i][node[n].nr]; lcase[sum->l].nmax[i]=node[n].nr; }
            if (lcase[sum->l].dat[i][node[n].nr] <  lcase[sum->l].min[i])
            {  lcase[sum->l].min[i]=lcase[sum->l].dat[i][node[n].nr]; lcase[sum->l].nmin[i]=node[n].nr; }
          }
        }
        sum->l++;
        sum->olc++;
      }
    }
  }

  else if(compare(string, "htc", 3)==3)                         
  {
    int lc,lct, lt=-1;
    double  rauh1, rauh2, lambda1, lambda2, hrd_m, C, lambda_eff;
    double  const_fluid, lambda_fluid=0.06, X=0.,Y;
                                                           
    sscanf(string, "%*s %s %d %d %lf %lf %lf %lf %lf", buf1, &l, &e, &rauh1, &rauh2, &lambda1, &lambda2, &hrd_m );
    printf("string:%s| |%s| |%d|  |%d| %lf %lf %lf %lf %lf\n", string,buf1, l,e, rauh1, rauh2, lambda1, lambda2, hrd_m);
    l--; e--;

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" set:%s does not exist\n", buf1);
      return;
    }
    if(l>=sum->l)
    {
      printf (" dataset:%d does not exist\n", l+1);
      return;
    }
    completeSet(buf1, "do") ;

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[l].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
     {
       printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
       return;
     }
     calcDatasets( l, sum, node, lcase );
     recompileEntitiesInMenu(l);
    }

    if(e>=lcase[l].ncomps)
    {
      printf (" entity:%d does not exist\n", e+1);
      return;
    }
  
        /* search for related temps */
    lc=l;     
    if(lc) { for (lct=lc-1; lct>=0; lct--) { if(lcase[lct].step_number!=lcase[lc].step_number) break; } lct++; }
    else lct=1;
    while((lct<sum->l)&&(lcase[lct].step_number==lcase[lc].step_number))
    {
      if( (compare( lcase[lct].name, "NDTEMP", 6) == 6)||( compare( lcase[lct].name, "TEMP", 4) == 4)||( compare( lcase[lct].name, "TT3D", 4) == 4))
      {
        if (!lcase[lct].loaded)
        {
          if( pre_readfrdblock(copiedNodeSets , lct, sum, node, lcase )==-1) 
          {
            printf(" ERROR in nodalDataset: Could not read data for Dataset:%d\n", lct+1); 
            return;
          }
          calcDatasets( lct, sum, node, lcase );
          recompileEntitiesInMenu(lct);
        }
        lt=lct;
        break;
      }
      lct++;
    } 

    /* create a new dataset */
    if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (sum->l+1) * sizeof(Datasets))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );

    lcase[sum->l].ncomps = 1;
    lcase[sum->l].irtype = 1;
    lcase[sum->l].npheader  = 0 ;
    lcase[sum->l].value  = lcase[l].value ;
    sprintf( lcase[sum->l].name, "HTC     ");
          sprintf( lcase[sum->l].dataset_name," ");
          strcpy(lcase[sum->l].dataset_text," ");
          strcpy(lcase[sum->l].analysis_name,lcase[l].analysis_name);
          lcase[sum->l].step_number=lcase[l].step_number;
          lcase[sum->l].analysis_type=lcase[l].analysis_type;
	  lcase[sum->l].loaded = 1;
          lcase[sum->l].fileptr = NULL;
  
    if ( (lcase[sum->l].nmax = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].nmin = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].max = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].min = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].dat = (float **)malloc( lcase[sum->l].ncomps * sizeof(float *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].compName = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].icname = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    for(i=0; i<lcase[sum->l].ncomps; i++)
    {
      if ( (lcase[sum->l].dat[i] = (float *)malloc( (sum->nmax+1) * sizeof(float))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );	               
      if ( (lcase[sum->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
         printf("\n\n ERROR: malloc failed\n\n" );
      if ( (lcase[sum->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
         printf("\n\n ERROR: malloc failed\n\n" );
      lcase[sum->l].max[i]=-MAX_INTEGER;
      lcase[sum->l].min[i]=MAX_INTEGER;
    }
    if ( (lcase[sum->l].menu = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].ictype = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].icind1 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].icind2 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    if ( (lcase[sum->l].iexist = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
  
    for(i=0; i<lcase[sum->l].ncomps; i++)
    {
      lcase[sum->l].menu[i] = 1;
      lcase[sum->l].ictype[i] = 1;
      lcase[sum->l].icind1[i] = i+1;
      lcase[sum->l].icind2[i] = 0;
      lcase[sum->l].iexist[i] = 0;
    }
    strcpy( lcase[sum->l].compName[0], "htc   ");
  
    for(i=0; i<lcase[sum->l].ncomps; i++) for(n=0; n<sum->n; n++) lcase[sum->l].dat[i][n]=0.;

    /* calc some basic constants */
    if((rauh1+rauh2)>8.5e-6) C=1.;
    else if((rauh1+rauh2)<3.06e-6) C=(4.3e-6/(rauh1+rauh2));
    else C=pow((8.5e-6/(rauh1+rauh2)),0.33);

    lambda_eff=2*lambda1*lambda2/(lambda1+lambda2);

    if(lt>-1) printf(" found temp ds:%d\n", lt+1);
    else
    {
      printf(" found no temp ds, use now 900K\n");
      X=7*(rauh1+rauh2)/(4.6 *9.6e-8 *(900./293.));
    }

    const_fluid=lambda_fluid/(7*(rauh1+rauh2));

    /* go over all nodes and calc htc according to formula from Dr. Reile (11.11.2010) */
    for(i=0; i<set[setNr].anz_n; i++)
    {
      n=set[setNr].node[i];
      if(lcase[l].dat[e][n]<0.)
      {
        if(lt>-1) X=7*(rauh1+rauh2)/(4.6 *9.6e-8 *(lcase[lct].dat[0][n]/293.));
        Y= 10./3. +10./X +4/X/X -4*(1./X/X/X +3./X/X +2./X) *log(1+X);
        lcase[sum->l].dat[0][n]= const_fluid *Y + 8000.*lambda_eff*pow(( lcase[l].dat[e][n]*-1.e6  *C/hrd_m),0.86);
        if (lcase[sum->l].dat[0][n] >  lcase[sum->l].max[0])
        {  lcase[sum->l].max[0]=lcase[sum->l].dat[0][n]; lcase[sum->l].nmax[0]=n; }
        if (lcase[sum->l].dat[0][n] <  lcase[sum->l].min[0])
        {  lcase[sum->l].min[0]=lcase[sum->l].dat[0][n]; lcase[sum->l].nmin[0]=n; }
      }
    }

    sum->l++;
    sum->olc++;
  }

  else if(compare(string, "calcFres", 5)==5)                         
  {                                                             
    sscanf(string, "%*s %s %d %d", buf1, &l, &e);
    printf("set:%s ds:%d e:%d\n", buf1, l,e);
    l--; e--;

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" calcFres: set:%s does not exist\n", buf1);
      return;
    }
    if(l>=sum->l)
    {
      printf (" calcFres: dataset:%d does not exist\n", l+1);
      return;
    }
    if(e>=lcase[l].ncomps)
    {
      printf (" calcFres: entity:%d does not exist\n", e+1);
      return;
    }

    /* go over all faces, calc area-vectors and multiply with av-pressure */
    area=0.;
    Fres=0.;
    vFres[0]=0.;
    vFres[1]=0.;
    vFres[2]=0.;
    for(i=0; i<set[setNr].anz_f; i++)
    {
      f=set[setNr].face[i];
      if(face[f].type==10)
      {
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[0]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[4]].nx, v2);
        v_prod( v1, v2, vn );
        v_scalf(&lcase[l].dat[e][face[f].nod[0]],vn,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[4]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[1]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[4]],v3,vF);
        v_add( vF,evF,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[1]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[5]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[1]],v3,vF);
        v_add( vF,evF,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[5]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[2]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[5]],v3,vF);
        v_add( vF,evF,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[2]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[6]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[2]],v3,vF);
        v_add( vF,evF,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[6]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[3]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[6]],v3,vF);
        v_add( vF,evF,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[3]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[7]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[3]],v3,vF);
        v_add( vF,evF,evF);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[7]].nx, v1);
        v_result( &node[face[f].nod[8]].nx, &node[face[f].nod[0]].nx, v2);
        v_prod( v1, v2, v3 );
        v_add( vn,v3,vn);
        v_scalf(&lcase[l].dat[e][face[f].nod[7]],v3,vF);
        v_add( vF,evF,evF);

        factor=0.5;                 ;
        v_scal(&factor,vn,vn);
        earea=v_betrag(vn);
        area+=earea;

        v_scal(&factor,evF,evF);
        eFres=v_betrag(evF);
        v_add(evF,vFres,vFres);
        Fres=v_betrag(vFres);

        printf("  face:%d type:%d eArea:%lf eFres:%lf\n", f, face[f].type, earea, eFres);
      }
      else
      {
        printf("ERROR: face:%d.type:%d not known\n", f, face[f].type);
      }
    }
    printf("\n area:%lf Fres:%lf (%lf %lf %lf)\n",area, Fres,vFres[0],vFres[1],vFres[2]);
  }
  else if(compare(string, "fuss", 4)==4)
  {
     sscanf(string, "%*s %d %lf", &anzS, &absAE);
     if((anzS <= 0) || (absAE < 1)){
        printf("\n");
        printf("Please call fuss as follows:\n");
        printf("\n");
        printf("call fuss 86 10.63\n");
        printf("\n");
        printf("Where 86 is the amount of blades\n");
        printf("and 10.63 is the distance from the \ncenter of the disk to the AE plane.\n");
        printf("\n");
     }
     else{
        fuss(&point, &line, &set, anzS, absAE);
     }
  }                         

  else if(compare(string, "statistics", 7)==7)                         
  { 
    int lc,lct;
    static int *lt=NULL;
    double vmax, vmin;

    buf2[0]=e=0;
    vmax=vmin=0.;
    //   statistics <common-DATASET-name> <entity-nr> [<time>]|[<common-DATASET-name> <entity-nr> <minval> <maxval>]
    length=sscanf(string, "%*s %s %d %s %d %lf %lf", buf1, &en, buf2, &e, &vmin, &vmax);

    printf("search:%s\n", buf1);
    suml_ini=sum->l;
    if(length>3) printf("and regard only values between %f and %f of related ds:%s\n", vmin, vmax, buf2);
    if ( (lt = (int *)realloc(lt, suml_ini * sizeof(int))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );

    for(l=0; l<suml_ini; l++)                                    
    {
      lt[l]=-1;
      if((length==3)&&(strlen(buf2))&&(lcase[l].value!=atof(buf2))) continue;
      if (compare(lcase[l].name,buf1,strlen(buf1))==strlen(buf1))
      {
        /* add entity en in a new dataset and divide by the nr of datasets */

        /* check if the data of the specified lcase (Dataset) are already available */
        if (!lcase[l].loaded)
        {
         if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
         {
           printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
           return;
         }
         calcDatasets( l, sum, node, lcase );
         recompileEntitiesInMenu(l);
        }

        if(en>lcase[l].ncomps)
        {
          printf (" entity:%d does not exist\n", en);
          return;
        }

        /* eventually search the related dataset */
        if(length>3)
        {
          lc=l;     
          if(lc) { for (lct=lc-1; lct>=0; lct--) { if(lcase[lct].step_number!=lcase[lc].step_number) break; } lct++; }
          else lct=1;
          while((lct<sum->l)&&(lcase[lct].step_number==lcase[lc].step_number))
          {
            if (compare(lcase[lct].name,buf2,strlen(buf2))==strlen(buf2))
            {
              if (!lcase[lct].loaded)
              {
                if( pre_readfrdblock(copiedNodeSets , lct, sum, node, lcase )==-1) 
                {
                  printf(" ERROR in nodalDataset: Could not read data for Dataset:%d\n", lct+1); 
                  return;
                }
                calcDatasets( lct, sum, node, lcase );
                recompileEntitiesInMenu(lct);
              }
              lt[l]=lct;
              break;
            }
            lct++;
          } 
          if(lt[l]==-1)
          {
            printf (" related dataset:%s does not exist\n", buf2);
            return;
          }
      
          if(e>lcase[lt[l]].ncomps)
          {
            printf (" entity:%d does not exist\n", e);
            return;
          }
	}


        if(sum_lc==0)
        {
          /* create a new dataset */
          if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (sum->l+2) * sizeof(Datasets))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
  
          lcase[sum->l].ncomps = 9;
          lcase[sum->l].irtype = lcase[l].irtype;
          lcase[sum->l].npheader  = 0 ;
          if(strlen(buf2)) lcase[sum->l].value  = atof(buf2) ; else lcase[sum->l].value  =0.;
          //strcpy(lcase[sum->l].name,lcase[l].name) ;
          strcpy(lcase[sum->l].name,lcase[l].compName[en-1]) ;
          sprintf( lcase[sum->l].dataset_name,"STATISTIC");
          strcpy(lcase[sum->l].dataset_text,"");
          strcpy(lcase[sum->l].analysis_name,lcase[l].analysis_name);
          lcase[sum->l].step_number=lcase[sum->l-1].step_number+1;
          lcase[sum->l].analysis_type=0;
	  lcase[sum->l].loaded = 1;
          lcase[sum->l].fileptr = NULL;
    
          if ( (lcase[sum->l].nmax = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].nmin = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].max = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].min = (float *)malloc( lcase[sum->l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].dat = (float **)malloc( lcase[sum->l].ncomps * sizeof(float *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].compName = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].icname = (char **)malloc( lcase[sum->l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            if ( (lcase[sum->l].dat[i] = (float *)calloc( (sum->nmax+1), sizeof(float))) == NULL )
              printf("\n\n ERROR: malloc failure\n\n" );	               
            if ( (lcase[sum->l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            if ( (lcase[sum->l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            lcase[sum->l].max[i]=-MAX_INTEGER;
            lcase[sum->l].min[i]=MAX_INTEGER;
           }
          if ( (lcase[sum->l].menu = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].ictype = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].icind1 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].icind2 = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[sum->l].iexist = (int *)malloc( lcase[sum->l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[sum->l].ncomps; i++)
          {
            lcase[sum->l].menu[i] = 1;
            lcase[sum->l].ictype[i] = 1;
            lcase[sum->l].icind1[i] = 1;
            lcase[sum->l].icind2[i] = 0;
            lcase[sum->l].iexist[i] = 0;
          }
          strcpy( lcase[sum->l].compName[0], "Number");
          strcpy( lcase[sum->l].compName[1], "average");
          strcpy( lcase[sum->l].compName[2], "sigma");
          strcpy( lcase[sum->l].compName[3], "av+2sigma");
          strcpy( lcase[sum->l].compName[4], "av+3sigma");
          strcpy( lcase[sum->l].compName[5], "max");
          strcpy( lcase[sum->l].compName[6], "min");
          strcpy( lcase[sum->l].compName[7], "dsmax");
          strcpy( lcase[sum->l].compName[8], "dsmin");
          sum->l++;
        }

        sum_lc++;

        for(n=0; n<sum->n; n++)
        {
          if(lt[l]>-1)
	  {
            if((lcase[lt[l]].dat[e-1][node[n].nr]<vmin)||(lcase[lt[l]].dat[e-1][node[n].nr]>vmax)) { /* printf(" %lf %lf %lf\n", lcase[lt[l]].dat[e-1][node[n].nr],vmin,vmax);*/  continue;}
	    // printf("%d %d %d %d n:%d v:%lf v2:%lf\n", l,en,lt[l],e,node[n].nr, lcase[l].dat[en-1][node[n].nr],lcase[lt[l]].dat[e-1][node[n].nr]); 
	  }
	  //else printf("%d %d n:%d v:%lf\n", l,en,node[n].nr, lcase[l].dat[en-1][node[n].nr]); 
          lcase[suml_ini].dat[0][node[n].nr]++;
          lcase[suml_ini].dat[1][node[n].nr]+=lcase[l].dat[en-1][node[n].nr];
        }


        printf(" %d read dataset:%d %s %lf\n",sum_lc, l+1, lcase[l].name, lcase[l].value);
      }
    }

    // x_av
    for(n=0; n<sum->n; n++)
    {
      lcase[suml_ini].dat[1][node[n].nr]/=lcase[suml_ini].dat[0][node[n].nr];
      lcase[suml_ini].dat[5][node[n].nr]=-MAX_FLOAT;
      lcase[suml_ini].dat[6][node[n].nr]=MAX_FLOAT;
    }

    for(l=0; l<suml_ini; l++)                                    
    {
      if((length==3)&&(strlen(buf2))&&(lcase[l].value!=atof(buf2))) continue;                                                            
      if (compare(lcase[l].name,buf1,strlen(buf1))==strlen(buf1))
      {
        for(n=0; n<sum->n; n++)
        {
          if(lt[l]>-1)
	  {
            if((lcase[lt[l]].dat[e-1][node[n].nr]<vmin)||(lcase[lt[l]].dat[e-1][node[n].nr]>vmax)) { /* printf(" %lf %lf %lf\n", lcase[lt[l]].dat[e-1][node[n].nr],vmin,vmax);*/  continue;}
	  }

          // sum (xi-x_av)**2
          lcase[suml_ini].dat[2][node[n].nr]+=((double)lcase[l].dat[en-1][node[n].nr]-(double)lcase[suml_ini].dat[1][node[n].nr])*((double)lcase[l].dat[en-1][node[n].nr]-(double)lcase[suml_ini].dat[1][node[n].nr]);

          // max and min
          if(lcase[suml_ini].dat[5][node[n].nr]<lcase[l].dat[en-1][node[n].nr])
	  {
            lcase[suml_ini].dat[5][node[n].nr]=lcase[l].dat[en-1][node[n].nr];
            lcase[suml_ini].dat[7][node[n].nr]=l+1;
	  }
          if(lcase[suml_ini].dat[6][node[n].nr]>lcase[l].dat[en-1][node[n].nr])
	  {
            lcase[suml_ini].dat[6][node[n].nr]=lcase[l].dat[en-1][node[n].nr];
            lcase[suml_ini].dat[8][node[n].nr]=l+1;
	  }
        }
      }
    }

    for(n=0; n<sum->n; n++)
    {
      lcase[suml_ini].dat[2][node[n].nr]= sqrt( (double)lcase[suml_ini].dat[2][node[n].nr] / (double)(sum_lc-1) );

      if(lcase[suml_ini].dat[1][node[n].nr]>0.) lcase[suml_ini].dat[3][node[n].nr]= lcase[suml_ini].dat[1][node[n].nr]+2.*lcase[suml_ini].dat[2][node[n].nr];
      else                                      lcase[suml_ini].dat[3][node[n].nr]= lcase[suml_ini].dat[1][node[n].nr]-2.*lcase[suml_ini].dat[2][node[n].nr];
      if(lcase[suml_ini].dat[1][node[n].nr]>0.) lcase[suml_ini].dat[4][node[n].nr]= lcase[suml_ini].dat[1][node[n].nr]+3.*lcase[suml_ini].dat[2][node[n].nr];
      else                                      lcase[suml_ini].dat[4][node[n].nr]= lcase[suml_ini].dat[1][node[n].nr]-3.*lcase[suml_ini].dat[2][node[n].nr];

      for(i=0; i<lcase[suml_ini].ncomps; i++)
      {
        if (lcase[suml_ini].dat[i][node[n].nr] >  lcase[suml_ini].max[i])
        {  lcase[suml_ini].max[i]=lcase[suml_ini].dat[i][node[n].nr]; lcase[suml_ini].nmax[i]=node[n].nr;}
        if (lcase[suml_ini].dat[i][node[n].nr] <  lcase[suml_ini].min[i])
        {  lcase[suml_ini].min[i]=lcase[suml_ini].dat[i][node[n].nr]; lcase[suml_ini].nmin[i]=node[n].nr;}
      }
    }
    calcDatasets( suml_ini, sum, node, lcase );
    recompileEntitiesInMenu(suml_ini);
  }

  else if(compare(string, "value", 5)==5)                         
  { 
    l=e=1;                                                            
    sscanf(string, "%*s %s %lf %d %d", buf1, &val, &l, &e);

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" ERROR: set:%s does not exist\n", buf1);
      return;
    }
    l--; e--;

    if(l>=sum->l)
    {
      /* create a new dataset */
      if(sum->l==0) lcase=NULL;
      if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (l+1) * sizeof(Datasets))) == NULL )
        printf("\n\n ERROR: malloc failure\n\n" );
      printf("\n create ds: %d with entity: %d\n\n", l+1, e+1);
  
          lcase[l].ncomps = e+1;
          lcase[l].irtype = 1;
          lcase[l].npheader  = 0 ;
          if(strlen(buf2)) lcase[l].value  = atof(buf2) ; else lcase[l].value  =0.;
          //strcpy(lcase[l].name,lcase[l].name) ;
          strcpy(lcase[l].name,"call") ;
          sprintf( lcase[l].dataset_name,"value");
          strcpy(lcase[l].dataset_text,"");
          strcpy(lcase[l].analysis_name,"");
          lcase[l].step_number=lcase[sum->l-1].step_number+1;
          lcase[l].analysis_type=0;
	  lcase[l].loaded = 1;
          lcase[l].fileptr = NULL;
    
          if ( (lcase[l].nmax = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].nmin = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].max = (float *)malloc( lcase[l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].min = (float *)malloc( lcase[l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].dat = (float **)malloc( lcase[l].ncomps * sizeof(float *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].compName = (char **)malloc( lcase[l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].icname = (char **)malloc( lcase[l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[l].ncomps; i++)
          {
            if ( (lcase[l].dat[i] = (float *)calloc( (sum->nmax+1), sizeof(float))) == NULL )
              printf("\n\n ERROR: malloc failure\n\n" );	               
            if ( (lcase[l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            if ( (lcase[l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            lcase[l].max[i]=-MAX_INTEGER;
            lcase[l].min[i]=MAX_INTEGER;
            for(n=0; n<=sum->nmax; n++)
              lcase[l].dat[i][n]=0.;
          }
          if ( (lcase[l].menu = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].ictype = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].icind1 = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].icind2 = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].iexist = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[l].ncomps; i++)
          {
            lcase[l].menu[i] = 1;
            lcase[l].ictype[i] = 1;
            lcase[l].icind1[i] = 1;
            lcase[l].icind2[i] = 0;
            lcase[l].iexist[i] = 0;
            strcpy( lcase[l].compName[i], "nn");
          }
          sum->l++;

    }


    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[l].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
     {
       printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
       return;
     }
     calcDatasets( l, sum, node, lcase );
     recompileEntitiesInMenu(l);
    }

    for(n=0; n<set[setNr].anz_n; n++)
    {
      lcase[l].dat[e][set[setNr].node[n]]+=val;
    }

    for(n=0; n<sum->n; n++)
    {
      if (lcase[l].dat[e][node[n].nr] >  lcase[l].max[e])
      {  lcase[l].max[e]=lcase[l].dat[e][node[n].nr]; lcase[l].nmax[e]=node[n].nr;}
      if (lcase[l].dat[e][node[n].nr] <  lcase[l].min[e])
      {  lcase[l].min[e]=lcase[l].dat[e][node[n].nr]; lcase[l].nmin[e]=node[n].nr;}
    }

    calcDatasets( l, sum, node, lcase );
    recompileEntitiesInMenu(l);
  }

  else if(compare(string, "smooth", 5)==5)                         
  { 
    l=e=1;                                                            
    sscanf(string, "%*s %s %d %d", buf1, &l, &e);

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" ERROR: set:%s does not exist\n", buf1);
      return;
    }
    l--; e--;

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[l].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
     {
       printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
       return;
     }
     calcDatasets( l, sum, node, lcase );
     recompileEntitiesInMenu(l);
    }

    /* create a buffer for averaged nodal values */
    if( (averageValue=(double *)calloc( (set[setNr].anz_n),sizeof(double) ) )==NULL) 
      printf("\n\n ERROR: malloc failure\n\n" );

    for(k=0; k<set[setNr].anz_n; k++)
    {
      f=0;
      /* search the connected faces */
      for(i=0; i<sum->f; i++)
      {
        if (face[i].type == 7) n = 3;  /* TRI3  */
        else if (face[i].type == 8) n = 6;  /* TRI6  */
        else if (face[i].type == 9) n = 4;  /* QUAD4 */
        else if (face[i].type == 10) n = 8; /* QUAD8 */
        else if (face[i].type == 11) n = 2; /* beam2 */
        else if (face[i].type == 12) n = 3; /* beam3 */
        else n=0;
        for (j=0; j<n; j++)
        {
          if(face[i].nod[j]==set[setNr].node[k])
  	  {
            /* found a connected face, store all nodes */
            for (j=0; j<n; j++)
            {
              if((connectedNode= (int *)realloc( (int *)connectedNode, (f+1) *sizeof(int)))==NULL )
                printf("\n\n ERROR: realloc failed\n\n");
              connectedNode[f++]=face[i].nod[j];
	    }
            break;
  	  }
        }
      }

      /* average the values */
      //for (j=0; j<f; j++) printf("%d n:%d %f\n",j+1, connectedNode[j], lcase[l].dat[e][connectedNode[j]]);
      for (j=0; j<f; j++) averageValue[k]+=lcase[l].dat[e][connectedNode[j]];
      averageValue[k]/=f;
      //printf("f:%d n:%d v:%f\n\n", f, set[setNr].node[k],  averageValue[k]);
    }

    /* store the aveaged values in the original array */
    for(n=0; n<set[setNr].anz_n; n++)
    {
      lcase[l].dat[e][set[setNr].node[n]]=averageValue[n];
    }
    free(connectedNode);
    free(averageValue);

    for(n=0; n<sum->n; n++)
    {
      if (lcase[l].dat[e][node[n].nr] >  lcase[l].max[e])
      {  lcase[l].max[e]=lcase[l].dat[e][node[n].nr]; lcase[l].nmax[e]=node[n].nr;}
      if (lcase[l].dat[e][node[n].nr] <  lcase[l].min[e])
      {  lcase[l].min[e]=lcase[l].dat[e][node[n].nr]; lcase[l].nmin[e]=node[n].nr;}
    }

  }

  else if(compare(string, "worstValues", 10)==10)                         
  { 
    ds=e=1;                                                            
    sscanf(string, "%*s %d %d", &ds, &e);
    printf("ds:%d\n",ds);
    if(ds<=0)
    {
      ds=sum->l+ds;
      printf(" use ds:%d from %d\n",ds, sum->l);
    }
    ds--; e--;

    /* check the ds if its a statistics ds and determine the data type */
    if( compare(lcase[ds].dataset_name,"STATISTIC", 8) ==8)
    {
      if( compare(lcase[ds].compName[e],"dsmax", 3) !=3)
      {
        printf("ERROR: e:%d is not dsmax or dsmin\n\n", e+1);
        return;
      }
      /* check if the data of the specified lcase (Dataset) are already available */
      if (!lcase[ds].loaded)
      {
       if( pre_readfrdblock(copiedNodeSets , ds, sum, node, lcase )==-1) 
       {
         printf("ERROR in userfunction: Could not read data for Dataset:%d\n", ds+1); 
         return;
       }
       calcDatasets( ds, sum, node, lcase );
       recompileEntitiesInMenu(ds);
      }

      /* get data type */
      lmax=lcase[ds].dat[e][lcase[ds].nmax[e]]-1;
      if(lcase[lmax].ictype[0]==1) ncomps=1;
      if(lcase[lmax].ictype[0]==2) ncomps=3;
      if(lcase[lmax].ictype[0]==4) ncomps=6;
      if(lcase[lmax].ictype[0]==12) ncomps=12;
    }
    else
    {
      printf("ERROR: ds:%d is not a STATISTIC dataset\n\n", ds+1);
      return;
    }

    /* create a new dataset */
    if(sum->l==0) lcase=NULL;
    l=sum->l;
    if ( (lcase = (Datasets *)realloc((Datasets *)lcase, (l+1) * sizeof(Datasets))) == NULL )
      printf("\n\n ERROR: malloc failure\n\n" );
    printf("\n create ds: %d with %d components\n\n", l+1, ncomps);
  
          lcase[l].ncomps = ncomps;
          lcase[l].irtype = lcase[lmax].irtype;
          lcase[l].npheader  = 0 ;
          lcase[l].value  = lcase[lmax].value;
          strcpy(lcase[l].name,lcase[lmax].name) ;
          sprintf( lcase[l].dataset_name,lcase[lmax].dataset_name);
          strcpy(lcase[l].dataset_text,lcase[lmax].dataset_text);
          strcpy(lcase[l].analysis_name,lcase[lmax].analysis_name);
          lcase[l].step_number=lcase[lmax].step_number;
          lcase[l].analysis_type=lcase[lmax].analysis_type;
	  lcase[l].loaded = 1;
          lcase[l].fileptr = NULL;
    
          if ( (lcase[l].nmax = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].nmin = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].max = (float *)malloc( lcase[l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].min = (float *)malloc( lcase[l].ncomps * sizeof(float))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].dat = (float **)malloc( lcase[l].ncomps * sizeof(float *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].compName = (char **)malloc( lcase[l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].icname = (char **)malloc( lcase[l].ncomps * sizeof(char *))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[l].ncomps; i++)
          {
            if ( (lcase[l].dat[i] = (float *)calloc( (sum->nmax+1), sizeof(float))) == NULL )
              printf("\n\n ERROR: malloc failure\n\n" );	               
            if ( (lcase[l].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            if ( (lcase[l].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
               printf("\n\n ERROR: malloc failed\n\n" );
            lcase[l].max[i]=-MAX_INTEGER;
            lcase[l].min[i]=MAX_INTEGER;
          }
          if ( (lcase[l].menu = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].ictype = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].icind1 = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].icind2 = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          if ( (lcase[l].iexist = (int *)malloc( lcase[l].ncomps * sizeof(int))) == NULL )
            printf("\n\n ERROR: malloc failure\n\n" );
          for(i=0; i<lcase[l].ncomps; i++)
          {
            lcase[l].menu[i] = 1;
            lcase[l].ictype[i] = lcase[lmax].ictype[i];
            lcase[l].icind1[i] = lcase[lmax].icind1[i];
            lcase[l].icind2[i] = lcase[lmax].icind2[i];
            lcase[l].iexist[i] = lcase[lmax].iexist[i];
            strcpy( lcase[l].compName[i],lcase[lmax].compName[i]);
          }
          sum->l++;


    for(k=0; k<lcase[l].ncomps; k++)
    {
      for(n=0; n<sum->n; n++)
      {
        lmax=lcase[ds].dat[e][node[n].nr]-1;
        lcase[l].dat[k][node[n].nr]=lcase[lmax].dat[k][node[n].nr];
      }

      if (lcase[l].dat[k][node[n].nr] >  lcase[l].max[k])
      {  lcase[l].max[k]=lcase[l].dat[k][node[n].nr]; lcase[l].nmax[k]=node[n].nr;}
      if (lcase[l].dat[k][node[n].nr] <  lcase[l].min[k])
      {  lcase[l].min[k]=lcase[l].dat[k][node[n].nr]; lcase[l].nmin[k]=node[n].nr;}
    }

    calcDatasets( l, sum, node, lcase );
    recompileEntitiesInMenu(l);
  }

  else if(compare(string, "writeDS", 6)==6)                         
  { 
    length=sscanf(string, "%*s %s %d %d %d %d %d %d %d", buf1, &ds, &enti[0], &enti[1], &enti[2], &enti[3], &enti[4], &enti[5]);
    printf("set:%s\n", buf1);
    printf("ds:%d\n",ds);

    setNr=getSetNr(buf1);
    if (setNr<0)
    {
      printf (" ERROR: set:%s does not exist\n", buf1);
      return;
    }

    if(ds<=0)
    {
      ds=sum->l-1+ds;
      printf("ds:%d %d\n",ds, sum->l-1);
    }

    sprintf(buf2,"ds%d.txt",ds);
    handle = fopen (buf2, "w");
    if ( handle== NULL )  { printf ("\nThe file \"%s\" could not be opened.\n\n", buf2); return; }
    else  printf ("\n%s opened\n\n",buf2);

    ds--;
    for(i=0; i<12; i++)  enti[i]--;

    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[ds].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , ds, sum, node, lcase )==-1) 
     {
       printf("ERROR in userfunction: Could not read data for Dataset:%d\n", ds+1); 
       return;
     }
     calcDatasets( ds, sum, node, lcase );
     recompileEntitiesInMenu(ds);
    }

    fprintf(handle,"nodeNr");
    for(j=0; j<length-2; j++) fprintf(handle," %s", lcase[ds].compName[enti[j]]);
    fprintf(handle,"\n");
    for(i=0; i<set[setNr].anz_n; i++)
    {
      n=set[setNr].node[i];
      printf("%d", n);
      for(j=0; j<length-2; j++) printf(" %f", lcase[ds].dat[enti[j]][n]);
      printf("\n");
      fprintf(handle,"%d", n);
      for(j=0; j<length-2; j++) fprintf(handle," %f", lcase[ds].dat[enti[j]][n]);
      fprintf(handle,"\n");
    }
    fclose(handle);
  }
  else printf(" ERROR userfunction:%s not known\n", string);

}


void calcFshear(char *name, int lc, double factor, double fstat, double mue, Summen *sum )
{
  int i,j, k, nodnr, setNr, setcopy;
  double fx, fy, fz, ax, ay, az, fres, dfi, fi, alfa, fnorm, fshear;
  double fj[3], efj[3], efn[3]={0.706898, 0.707315, 0.000036};
  int *sum_n=NULL;
  Nodes *norm=NULL;
  double fres_max=0., fnorm_max=0., fshear_max=0.;
  double fres_sum[400], fnorm_sum[400], fshear_sum[400];
  char  datout[MAX_LINE_LENGTH];
  FILE *handle;

  sprintf(datout,"%s_%d.out", name,lc+1);
  handle = fopen (datout, "w");
  if ( handle== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n", datout); return; }
  else  printf ("\n%s opened\n\n",datout);

  if(sum->l<lc) { printf("dataset not known:%d\n",lc); return; }
  lc--;

  setNr=getSetNr(name);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", name);
    return;
  }

  /* determine the normal based on all connected faces */
  setcopy=pre_seta( "+norm", "i", 0);
  if (setcopy<0)
  {
    errMsg (" ERROR: set:+norm could not be created\n" );
    return;
  }
  for (i=0; i<set[setNr].anz_n; i++)
  {
    seta( setcopy, "n", set[setNr].node[i] );
  }
  completeSet("+norm", "up") ;
  completeSet("+norm", "do") ;
  getNodeNormalen(&sum_n, &norm, setcopy, sum, face);
  delSet("+norm");


  fres_max= fnorm_max= fshear_max=-MAX_FLOAT;
  for(j=0; j<360; j++) fres_sum[j]=fnorm_sum[j]=fshear_sum[j]=0.;

  printf("lc:%d name:%s lc-comps:%d sum_n:%d\n", lc, lcase[lc].name, lcase[lc].ncomps, set[setNr].anz_n);
  /* check if the data of the specified lcase (Dataset) are already available */
  if (!lcase[lc].loaded)
  {
   if( pre_readfrdblock(copiedNodeSets , lc, sum, node, lcase )==-1) 
   {
     printf("ERROR in userfunction: Could not read data for Dataset:%d\n", lc+1); 
     return;
   }
   calcDatasets( lc, sum, node, lcase );
   recompileEntitiesInMenu(lc);
  }

  for(i=0; i<set[setNr].anz_n; i++)
  {
    nodnr=set[setNr].node[i];
    printf("node:%d \n", nodnr);

    efn[0]=norm[nodnr].nx;
    efn[1]=norm[nodnr].ny;
    efn[2]=norm[nodnr].nz;
    
    printf("norm:%f %f %f\n", efn[0], efn[1], efn[2]);

    fx=lcase[lc].dat[0][nodnr];
    fy=lcase[lc].dat[1][nodnr];
    fz=lcase[lc].dat[2][nodnr];

    if(lcase[lc].ncomps>=6)
    {
      ax=lcase[lc].dat[3][nodnr]*PI/180;
      ay=lcase[lc].dat[4][nodnr]*PI/180;
      az=lcase[lc].dat[5][nodnr]*PI/180;
  
      dfi=PI/180.;   
      for(j=0; j<360; j++)
      {
        fi=dfi*j;
        fj[0]=fx*cos(fi+ax);
        fj[1]=fy*cos(fi+ay);
        fj[2]=fz*cos(fi+az);
        fres=0.;
        for(k=0; k<3; k++) fres+=fj[k]*fj[k]; fres=sqrt(fres);
        v_norm(fj, efj);
        alfa=acos(v_sprod(efj, efn));
        fshear=fres*sin(alfa);
        fnorm=fres*cos(alfa);
        fres_max=  max(abs(fres),fres_max); 
        fnorm_max= max(abs(fnorm),fnorm_max); 
        fshear_max=max(abs(fshear),fshear_max);
        fres_sum[j]+=  fres;
        fnorm_sum[j]+=  fnorm;
        fshear_sum[j]+=  fshear;

        printf("fi:%f Fres:%f xyz: %f %f %f angle:%f ayx:%f Fnorm:%f Fshear:%f\n", fi*180./PI, fres, fj[0]/fres, fj[1]/fres, fj[2]/fres, (alfa*180./PI), atan(fj[0]/fj[1])*180./PI, fnorm, fshear);
        //printf("fi: %f Fnorm(dyn*FR+stat)*mue: %f Fshear*FR: %f\n", fi*180./PI, (fnorm*factor+fstat)*mue, fshear*factor);
      }
    }
    else
    {
      fj[0]=fx;
      fj[1]=fy;
      fj[2]=fz;
      fres=0.;
      for(k=0; k<3; k++) fres+=fj[k]*fj[k]; fres=sqrt(fres);
      v_norm(fj, efj);
      alfa=acos(v_sprod(efj, efn));
      fshear=fres*sin(alfa);
      fnorm=fres*cos(alfa);
      fres_max=  max(abs(fres),fres_max); 
      fnorm_max= max(abs(fnorm),fnorm_max); 
      fshear_max=max(abs(fshear),fshear_max);
      fres_sum[0]+=  fres;
      fnorm_sum[0]+=  fnorm;
      fshear_sum[0]+=  fshear;
      printf("Fnres:%f Fnorm:%f Fshear:%f alfa:%f\n", fres, fnorm, fshear, alfa*180./PI);
    }
  }

  if(lcase[lc].ncomps>=6)
  {
    for(j=0; j<360; j++)
    {
      fi=dfi*j;
      printf("fi: %f Fnres:%f Fnorm:%f Fshear:%f Fnorm(dyn*FR+stat)*mue: %f Fshear*FR: %f\n", fi*180./PI, fres_sum[j], fnorm_sum[j], fshear_sum[j], (fnorm_sum[j]*factor+fstat)*mue, fshear_sum[j]*factor);
      fprintf(handle, "%f %f %f\n", fi*180./PI, (fnorm_sum[j]*factor+fstat)*mue, fshear_sum[j]*factor);
    }
    printf("Max: Fnres:%f Fnorm:%f Fshear:%f Fnorm(dyn*FR+stat)*mue: %f Fshear*FR: %f\n", fres_max, fnorm_max, fshear_max,(fnorm_max*factor+fstat)*mue, fshear_max*factor );
  }
  else
  {
    printf("Sum: Fnres:%f Fnorm:%f Fshear:%f\n", fres_sum[0], fnorm_sum[0], fshear_sum[0]);
    printf("Max: Fnres:%f Fnorm:%f Fshear:%f\n", fres_max, fnorm_max, fshear_max);
  }
  fclose(handle);
}                         



void dispratio(Summen *sum, char *string)
{
  int i,l,n,length;
  int lc, nd;
  int setNr1, setNr2, system=0, dof=0;
  char setname1[MAX_LINE_LENGTH], setname2[MAX_LINE_LENGTH], typ[MAX_LINE_LENGTH], datout[MAX_LINE_LENGTH];
  FILE *handle;
  double dx,dy,dz, max_val=0., disp=0., alfa, dalfa, pi180;
  double max_val_tot=0., max_val_local=0.;
  Datasets  lcbuf[0];
  double csab[6];
  int cycsymFlag=0;

  length=sscanf(string, "%*s %s %s %s", setname1, setname2, typ);

  if(length>=2)      printf("set1:%s set1:%s typ:%s \n", setname1, setname2, typ);
  else
  {
    printf("ERROR, not enough parameters\n");
    return;
  }

  setNr1=getSetNr(setname1);
  if (setNr1<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname1);
    return;
  }
  setNr2=getSetNr(setname2);
  if (setNr2<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname2);
    return;
  }

  if(typ[0]=='c') system=1;
  dof=atoi(&typ[1]);
  printf("system:%d dof:%d\n",system,dof);
    
  pi180=PI/180.;
  dalfa=3.*pi180;

  sprintf(datout,"dispratio.csv");
  handle = fopen (datout, "w");
  if ( handle== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n", datout); return; }
  else  printf ("\n%s opened\n\n",datout);
  fprintf(handle,"ds, nd, freq, disptot, loc, loc/tot\n");

  if ( (lcbuf[0].dat = (float **)malloc( (3) * sizeof(float *))) == NULL )  printf("\n\n ERROR: malloc failure\n\n" );
  for(i=0; i<3; i++)
  {
    if ( (lcbuf[0].dat[i] = (float *)malloc( (1) * sizeof(float))) == NULL ) printf("\n\n ERROR: malloc failure\n\n" );	               
  }

  for(lc=0; lc<sum->l; lc++)
  {
    if(( lcase[lc].ictype[0] == 2)||(lcase[lc].ictype[0]== 12)) /* check first comp if its a vector (DISP, VELO etc.) */
    {
      /* check if the data of the specified lcase (Dataset) are already available */
      if (!lcase[lc].loaded)
      {
        if( pre_readfrdblock(copiedNodeSets , lc, sum, node, lcase )==-1) 
        {
          printf("ERROR in userfunction: Could not read data for Dataset:%d\n", lc+1); 
          return;
        }
        calcDatasets( lc, sum, node, lcase );
        recompileEntitiesInMenu(lc);
      }

      /* determine the nodal diameter */
      for(i=0;i<lcase[lc].npheader; i++)
      {
        if(compare(&lcase[lc].pheader[i][5],"PHID", 4)==4)
        {
          sscanf(lcase[lc].pheader[i],"%*s %d", &nd);
          break;
        }
      }

      /* determine the axis */
      for(i=0;i<lcase[lc].npheader; i++)
      {
        if(compare(&lcase[lc].pheader[i][5],"PAX", 3)==3)
        {
          sscanf(lcase[lc].pheader[i],"%*s %lf%lf%lf%lf%lf%lf", &csab[0], &csab[1], &csab[2], &csab[3], &csab[4], &csab[5]);
          //printf("%s\naxis:%f %f %f      %f %f %f\n",lcase[lc].pheader[i], csab[0], csab[1], csab[2], csab[3], csab[4], csab[5]);
          cycsymFlag=1;
          break;
        }
      }

      /* calculate total displacement  */
      max_val_tot=0.;
      for (n=0; n<set[setNr1].anz_n; n++ )
      {
        i=set[setNr1].node[n];
        if (lcase[lc].ictype[0]== 12)
	{
          max_val=alfa=0.;
          for ( l=0; l<60; l++ )
          {
            alfa+= dalfa;
            dx=(lcase[lc].dat[0][i]* cos(alfa+pi180*lcase[lc].dat[3][i]));
            dy=(lcase[lc].dat[1][i]* cos(alfa+pi180*lcase[lc].dat[4][i]));
            dz=(lcase[lc].dat[2][i]* cos(alfa+pi180*lcase[lc].dat[5][i]));
            disp=dx*dx+dy*dy+dz*dz;
            if(disp>max_val) max_val=disp;
          }
	}
        else if (lcase[lc].ictype[0]== 2)
	{
          max_val=sqrt(
          (lcase[lc].dat[0][i]*lcase[lc].dat[0][i]) +
          (lcase[lc].dat[1][i]*lcase[lc].dat[1][i]) +
          (lcase[lc].dat[2][i]*lcase[lc].dat[2][i]) );
	}
        else
	{
          max_val=0.; 
	}
        if(max_val>max_val_tot) max_val_tot=max_val;
      }
      max_val_tot=sqrt(max_val_tot);
      //printf("maximum deflection:%f\n", max_val_tot);

      /* calculate local displacement of the requested dof  */
      max_val_local=0.;
      for (n=0; n<set[setNr2].anz_n; n++ )
      {
        i=set[setNr2].node[n];
        if ((lcase[lc].ictype[0]== 12)&&(cycsymFlag==1))
	{
          max_val=alfa=0.;
          for ( l=0; l<60; l++ )
          {
            alfa+= dalfa;
            dx=(lcase[lc].dat[0][i]* cos(alfa+pi180*lcase[lc].dat[3][i]));
            dy=(lcase[lc].dat[1][i]* cos(alfa+pi180*lcase[lc].dat[4][i]));
            dz=(lcase[lc].dat[2][i]* cos(alfa+pi180*lcase[lc].dat[5][i]));
            if(!dof) disp=dx*dx+dy*dy+dz*dz;
            else if(system)
	    {
              lcbuf[0].dat[0][0]=dx;
              lcbuf[0].dat[1][0]=dy;
              lcbuf[0].dat[2][0]=dz;
	      //printf("alfa: %f xyz: %f %f %f ",alfa,dx,dy,dz);
              cartcyl(csab, 0, &node[i].nx, lcbuf, 0, 'd' ); 
              dx=lcbuf[0].dat[0][0];
              dy=lcbuf[0].dat[1][0];
              dz=lcbuf[0].dat[2][0];
	      //printf("rtz: %f %f %f\n",dx,dy,dz);
              switch(dof)
	      {
                case 1:
                  disp=dx*dx;
                break;
                case 2:
                  disp=dy*dy;
                break;
                case 3:
                  disp=dz*dz;
                break;
              }
            }
            else
	    {
              switch(dof)
	      {
                case 1:
                  disp=dx*dx;
                break;
                case 2:
                  disp=dy*dy;
                break;
                case 3:
                  disp=dz*dz;
                break;
              }
            }

            if(disp>max_val) max_val=disp;
          }
	}
        else if ((lcase[lc].ictype[0]== 2)&&(cycsymFlag==0)&&(system==0))
	{
          dx=lcase[lc].dat[0][i];
          dy=lcase[lc].dat[1][i];
          dz=lcase[lc].dat[2][i];
          if(!dof) disp=dx*dx+dy*dy+dz*dz;
          else
	  {
            switch(dof)
	    {
              case 1:
                disp=dx*dx;
              break;
              case 2:
                disp=dy*dy;
              break;
              case 3:
                disp=dz*dz;
              break;
            }
          }
          max_val=disp;
	}
        else
	{
          printf("ERROR: ds:%d type not valid\n", lc+1);
          max_val=0.; 
	}
        if(max_val>max_val_local) max_val_local=max_val;
      }
      max_val_local=sqrt(max_val_local);
      //printf("local deflection:%f\n", max_val_local);

      /* write data of that mode: */
      printf("ds:%d %s freq:%f disp tot:%f loc:%f loc/tot:%f\n", lc+1, lcase[lc].dataset_text, lcase[lc].value, max_val_tot, max_val_local, max_val_local/max_val_tot);
      fprintf(handle,"%d, %d, %f, %f, %f, %f\n", lc+1, nd, lcase[lc].value, max_val_tot, max_val_local, max_val_local/max_val_tot);
    }
  }
  fclose(handle);
  for(i=0; i<3; i++) free(lcbuf[0].dat[i]); free(lcbuf[0].dat);
}                         



void tableNSMS(Summen *sum, char *string)
{
  int i,n,length;
  int ds1,ds2,setNr,c[3];
  char setname[MAX_LINE_LENGTH], datout[MAX_LINE_LENGTH];
  FILE *handle;

  length=sscanf(string, "%*s %s %d %d", setname, &ds1, &ds2);
  ds1--; ds2--;

  if(length>=2)      printf("set:%s disp from ds:%s at freq:%f\n", setname, lcase[ds1].name, lcase[ds1].value);
  else
  {
    printf("ERROR, not enough parameters\n");
    return;
  }
  if(ds1>=sum->l)
  {
    printf("ERROR, dataset out of range\n");
    return;
  }
  if(length>2) if(ds2>=sum->l)
  {
    printf("ERROR, dataset out of range\n");
    return;
  }
  if(lcase[ds1].analysis_type!=2)
  {
    printf("ERROR, dataset not of type frequency\n");
    return;
  }
  if(length>2) if(lcase[ds2].analysis_type!=2)
  {
    printf("ERROR, dataset not of type frequency\n");
    return;
  }

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }

  /* check if the data of the specified lcase (Dataset) are already available */
  if (!lcase[ds1].loaded)
  {
   if( pre_readfrdblock(copiedNodeSets , ds1, sum, node, lcase )==-1) 
   {
     printf("ERROR in userfunction: Could not read data for Dataset:%d\n", ds1+1); 
     return;
   }
   calcDatasets( ds1, sum, node, lcase );
   recompileEntitiesInMenu(ds1);
  }
  if(length>2)
  {
    /* check if the data of the specified lcase (Dataset) are already available */
    if (!lcase[ds2].loaded)
    {
     if( pre_readfrdblock(copiedNodeSets , ds2, sum, node, lcase )==-1) 
     {
       printf("ERROR in userfunction: Could not read data for Dataset:%d\n", ds2+1); 
       return;
     }
     calcDatasets( ds2, sum, node, lcase );
     recompileEntitiesInMenu(ds2);
    }
  }

  sprintf(datout,"%s_%d.out", setname,ds1+1);
  handle = fopen (datout, "w");
  if ( handle== NULL )  { printf ("\nThe input file \"%s\" could not be opened.\n\n", datout); return; }
  else  printf ("\n%s opened\n\n",datout);

  printf("%s freq:%f\n", lcase[ds1].dataset_text, lcase[ds1].value);
  fprintf(handle,"%s freq:%f\n", lcase[ds1].dataset_text, lcase[ds1].value);

  for(i=0; i<3; i++) c[i]=i;
  if(lcase[ds1].ncomps>4) for(i=0; i<3; i++) c[i]+=3;
  if(length==2)
  {
    printf("      node          x            y           z          %s    %s    %s\n", lcase[ds1].compName[c[0]], lcase[ds1].compName[c[1]], lcase[ds1].compName[c[2]]);
    fprintf(handle,"      node          x            y           z          %s    %s    %s\n", lcase[ds1].compName[c[0]], lcase[ds1].compName[c[1]], lcase[ds1].compName[c[2]]);
    for(i=0; i<set[setNr].anz_n; i++)
    {
      n=set[setNr].node[i];
      printf("%10d %+12.3f %+12.3f %+12.3f  %+12.4e %+12.4e %+12.4e\n", n, node[n].nx, node[n].ny, node[n].nz, lcase[ds1].dat[c[0]][n], lcase[ds1].dat[c[1]][n], lcase[ds1].dat[c[2]][n]);
      fprintf(handle,"%10d %+12.3f %+12.3f %+12.3f  %+12.4e %+12.4e %+12.4e\n", n, node[n].nx, node[n].ny, node[n].nz, lcase[ds1].dat[c[0]][n], lcase[ds1].dat[c[1]][n], lcase[ds1].dat[c[2]][n]);
    }
  }
  else if(length==3)
  {
            printf("      node          x            y           z          R:%s    %s    %s      I:%s    %s    %s\n", lcase[ds1].compName[c[0]], lcase[ds1].compName[c[1]], lcase[ds1].compName[c[2]], lcase[ds2].compName[c[0]], lcase[ds2].compName[c[1]], lcase[ds2].compName[c[2]]);
    fprintf(handle,"      node          x            y           z          R:%s    %s    %s      I:%s    %s    %s\n", lcase[ds1].compName[c[0]], lcase[ds1].compName[c[1]], lcase[ds1].compName[c[2]], lcase[ds2].compName[c[0]], lcase[ds2].compName[c[1]], lcase[ds2].compName[c[2]] );
    for(i=0; i<set[setNr].anz_n; i++)
    {
      n=set[setNr].node[i];
      printf("%10d %+12.3f %+12.3f %+12.3f  %+12.4e %+12.4e %+12.4e  %+12.4e %+12.4e %+12.4e\n", n, node[n].nx, node[n].ny, node[n].nz, lcase[ds1].dat[c[0]][n], lcase[ds1].dat[c[1]][n], lcase[ds1].dat[c[2]][n], lcase[ds2].dat[c[0]][n], lcase[ds2].dat[c[1]][n], lcase[ds2].dat[c[2]][n]);
      fprintf(handle,"%10d %+12.3f %+12.3f %+12.3f  %+12.4e %+12.4e %+12.4e  %+12.4e %+12.4e %+12.4e\n", n, node[n].nx, node[n].ny, node[n].nz, lcase[ds1].dat[c[0]][n], lcase[ds1].dat[c[1]][n], lcase[ds1].dat[c[2]][n], lcase[ds2].dat[c[0]][n], lcase[ds2].dat[c[1]][n], lcase[ds2].dat[c[2]][n]);
    }
  }
  fprintf(handle,"\n\n");
  fclose(handle);
}                         


void checkrbe2(char *name)
{
  int i,e,setNr;

  setNr=getSetNr(name);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", name);
    return;
  }

  for(i=0; i<set[setNr].anz_e; i++)
  {
    e=set[setNr].elem[i];
    if(e_enqire[e].type==11)
    {
      if(node[e_enqire[e].nod[0]].nx!=node[e_enqire[e].nod[1]].nx) printf("warning: rbe %d n[%d]x %f != n[%d]x %f\n",e,e_enqire[e].nod[0],node[e_enqire[e].nod[0]].nx,e_enqire[e].nod[1],node[e_enqire[e].nod[1]].nx );
      if(node[e_enqire[e].nod[0]].ny!=node[e_enqire[e].nod[1]].ny) printf("warning: rbe %d n[%d]y %f != n[%d]y %f\n",e,e_enqire[e].nod[0],node[e_enqire[e].nod[0]].ny,e_enqire[e].nod[1],node[e_enqire[e].nod[1]].ny );
      if(node[e_enqire[e].nod[0]].nz!=node[e_enqire[e].nod[1]].nz) printf("warning: rbe %d n[%d]z %f != n[%d]z %f\n",e,e_enqire[e].nod[0],node[e_enqire[e].nod[0]].nz,e_enqire[e].nod[1],node[e_enqire[e].nod[1]].nz );
    }
  }
  printf("done\n");
}


double calcMass(char *setname, Summen *sum)
{
  int   i,j,l,n;
  int   nr, setNr;
  int   istat[3]={0,0,0};
  double volu=0., mass=0., vole, x[20],y[20],z[20];
  double  xcge, ycge, zcge;
  int  ds_ps, ds_ts;
  double ps, ts, rho, Rl=287.1;

  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return(-1);
  }

  ds_ps=ds_ts=-1;
  for(l=0; l<sum->l; l++)                              
  {                                                           
    if ((compare(lcase[l].name,"PS",2)==2)||(compare(lcase[l].name,"PR",2)==2))
    { 
      /* check if the data of the specified lcase (Dataset) are already available */
      if (!lcase[l].loaded)
      {
       if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
       {
         printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
         return(-1);
       }
       calcDatasets( l, sum, node, lcase );
       recompileEntitiesInMenu(l);
      }
      ds_ps=l;
    }
    if ((compare(lcase[l].name,"TS",2)==2)||(compare(lcase[l].name,"TE",2)==2))
    { 
      /* check if the data of the specified lcase (Dataset) are already available */
      if (!lcase[l].loaded)
      {
       if( pre_readfrdblock(copiedNodeSets , l, sum, node, lcase )==-1) 
       {
         printf("ERROR in userfunction: Could not read data for Dataset:%d\n", l+1); 
         return(-1);
       }
       calcDatasets( l, sum, node, lcase );
       recompileEntitiesInMenu(l);
      }
      ds_ts=l;
    }
    if((ds_ps!=-1)&&(ds_ts!=-1))
    {
      mass=volu=0.;
      for(i=0; i<set[setNr].anz_e; i++)
      {
        nr=set[setNr].elem[i];
        if((e_enqire[nr].type==1)||(e_enqire[nr].type==4))
        {
          for(j=0; j<8; j++)
          {
            x[j]=node[e_enqire[nr].nod[j]].nx;
            y[j]=node[e_enqire[nr].nod[j]].ny;
            z[j]=node[e_enqire[nr].nod[j]].nz;
          }
          hexaeder_(&nr, istat, x,y,z, &vole, &xcge, &ycge, &zcge );
        }
        else if((e_enqire[nr].type==2)||(e_enqire[nr].type==5))
        {
          /* calc as collapsed hex */
          for(j=0; j<3; j++)
          {
            x[j]=node[e_enqire[nr].nod[j]].nx;
            y[j]=node[e_enqire[nr].nod[j]].ny;
            z[j]=node[e_enqire[nr].nod[j]].nz;
          }
          x[j]=node[e_enqire[nr].nod[2]].nx;
          y[j]=node[e_enqire[nr].nod[2]].ny;
          z[j]=node[e_enqire[nr].nod[2]].nz;
          for(j=3; j<6; j++)
          {
            x[j+1]=node[e_enqire[nr].nod[j]].nx;
            y[j+1]=node[e_enqire[nr].nod[j]].ny;
            z[j+1]=node[e_enqire[nr].nod[j]].nz;
          }
          x[7]=node[e_enqire[nr].nod[5]].nx;
          y[7]=node[e_enqire[nr].nod[5]].ny;
          z[7]=node[e_enqire[nr].nod[5]].nz;
          hexaeder_(&nr, istat, x,y,z, &vole, &xcge, &ycge, &zcge );
        }
        else if((e_enqire[nr].type==3)||(e_enqire[nr].type==6))
        {
          for(j=0; j<4; j++)
          {
            x[j]=node[e_enqire[nr].nod[j]].nx;
            y[j]=node[e_enqire[nr].nod[j]].ny;
            z[j]=node[e_enqire[nr].nod[j]].nz;
          }
          tetraeder_(&nr, istat, x,y,z, &vole, &xcge, &ycge, &zcge );
        }
        else
        {
          printf("ERROR: type:%d of elem:%d not known. Interrupt\n",e_enqire[nr].type,nr); 
          return(-1.);
        }
      
        /* determine the average node-value */
        if(sum->l)
        {
          if (e_enqire[nr].type == 1) n = 8;  /* HEXA8 */
          else if (e_enqire[nr].type == 2) n = 6;  /* PENTA6 */
          else if (e_enqire[nr].type == 3) n = 4;  /* TET4 */
          else if (e_enqire[nr].type == 4) n = 20; /* HEX20 */
          else if (e_enqire[nr].type == 5) n = 15; /* PENTA15 */
          else if (e_enqire[nr].type == 6) n = 10; /* TET10 */
          else if (e_enqire[nr].type == 7) n = 3;  /* TRI3  */
          else if (e_enqire[nr].type == 8) n = 6;  /* TRI6  */
          else if (e_enqire[nr].type == 9) n = 4;  /* QUAD4 */
          else if (e_enqire[nr].type == 10) n = 8; /* QUAD8 */
          else if (e_enqire[nr].type == 11) n = 2; /* BEAM */
          else if (e_enqire[nr].type == 12) n = 3; /* BEAM3 */
          else n=0;
          rho=0.;
          for (j=0; j<n; j++)
          {
            ps=lcase[ds_ps].dat[0][e_enqire[nr].nod[j]];
            ts=lcase[ds_ts].dat[0][e_enqire[nr].nod[j]];
            rho+=ps/ts/Rl;
          }
          mass+=rho/n*(vole);
          volu+=vole;
        }
      }
      printf(" mass: %e volu: %e\n", mass, volu);
     
      ds_ps=ds_ts=-1;
    }
  }
  return(0);
}


#define     TESTNOD     0
//#define     THETA_STEPS 90.         /* steps in 360 degree (90==4 deg)*/
#define     THETA_STEPS 45.         /* steps in 360 degree (90==4 deg)*/

void addModes(char *string, Summen   *sum)
{
  int   i, j, n, th1,th2, evalnr=0;
  int  dsnew, dss[2][2], nprint=0;
  double scal1,scal2;
  double max_amp, amp, dth, dthgrd;
  double s1[6], s2[6], p[3], a1[3], a2[3], a3[3];
  double theta, worst_th1, worst_th2, ctl, stl;
  char buffer[MAX_LINE_LENGTH];
  FILE *handle=NULL;

  sscanf(string, "%*s %d %d %lf %d %d %lf %d", &dss[0][0], &dss[0][1], &scal1, &dss[1][0], &dss[1][1], &scal2, &nprint);

  printf("add modes %d %d to %d %d\n", dss[0][0], dss[0][1], dss[1][0], dss[1][1]);
  sprintf(buffer, "%d,%d+%d,%d", dss[0][0], dss[0][1], dss[1][0], dss[1][1]);

  if(nprint)
  {
    handle = fopen ("addModes.out", "w");
    if ( handle== NULL )  { printf ("\nThe result file \"%s\" could not be opened.\n\n", "addModes.out"); return; }
    else  printf ("\n%s opened\n\n","addModes.out");
  }

  /* check if the data of the specified lcase (Dataset) are already available */
  for (i=0; i<2; i++)
  {
    for (j=0; j<2; j++)
    {
      dss[i][j]--; 
      if (!lcase[dss[i][j]].loaded)
      {
       if( pre_readfrdblock(copiedNodeSets , dss[i][j], sum, node, lcase )==-1) 
       {
         printf("ERROR in userfunction: Could not read data for Dataset:%d\n", dss[i][j]+1); 
         return;
       }
       calcDatasets( dss[i][j], sum, node, lcase );
       recompileEntitiesInMenu(dss[i][j]);
      }
    }
  }

  dsnew=sum->l;
  sum->l++;

    /* STRESSES */
  lcase[dsnew].loaded=1;
  lcase[dsnew].npheader=lcase[dss[0][0]].npheader;
  lcase[dsnew].pheader=lcase[dss[0][0]].pheader;
  strcpy(lcase[dsnew].name, lcase[dss[0][0]].name);
  lcase[dsnew].ncomps=6;
  lcase[dsnew].irtype=lcase[dss[0][0]].irtype;
  lcase[dsnew].value=0;
  lcase[dsnew].analysis_type=lcase[dss[0][0]].analysis_type;
  lcase[dsnew].step_number=lcase[sum->l-1].step_number+1;
  strcpy(lcase[dsnew].analysis_name,lcase[dss[0][0]].analysis_name);
  //strcpy(lcase[dsnew].dataset_text,lcase[dss[0][0]].dataset_text);
  //strcpy(lcase[dsnew].dataset_name,lcase[dss[0][0]].dataset_name);
  strcpy(lcase[dsnew].dataset_text,buffer);
  strcpy(lcase[dsnew].dataset_name,"addModes");
  printf("analysis_name %s dataset_text:%s dataset_name %s\n",lcase[dsnew].analysis_name,lcase[dsnew].dataset_text,lcase[dsnew].dataset_name);

  if ( (lcase[dsnew].nmax = (int *)malloc( lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure1\n\n" );
  if ( (lcase[dsnew].nmin = (int *)malloc(  lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure2\n\n" );
  if ( (lcase[dsnew].max = (float *)malloc( lcase[dsnew].ncomps * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failure3\n\n" );
  if ( (lcase[dsnew].min = (float *)malloc( lcase[dsnew].ncomps * sizeof(float))) == NULL )
    printf("\n\n ERROR: malloc failure4\n\n" );
  if ( (lcase[dsnew].dat = (float **)malloc( lcase[dsnew].ncomps * sizeof(float *))) == NULL )
    printf("\n\n ERROR: malloc failure5\n\n" );
  if ( (lcase[dsnew].compName = (char **)malloc( lcase[dsnew].ncomps * sizeof(char *))) == NULL )
    printf("\n\n ERROR: malloc failure6\n\n" );
  if ( (lcase[dsnew].icname = (char **)malloc( lcase[dsnew].ncomps * sizeof(char *))) == NULL )
    printf("\n\n ERROR: malloc failure7\n\n" );
  if ( (lcase[dsnew].ictype = (int *)malloc( lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure8\n\n" );
  if ( (lcase[dsnew].icind1 = (int *)malloc( lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure9\n\n" );
  if ( (lcase[dsnew].icind2 = (int *)malloc( lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure10\n\n" );
  if ( (lcase[dsnew].iexist = (int *)malloc( lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure11\n\n" );
  if ( (lcase[dsnew].menu = (int *)malloc(lcase[dsnew].ncomps * sizeof(int))) == NULL )
    printf("\n\n ERROR: malloc failure12\n\n" );

  for(i=0; i<lcase[dsnew].ncomps; i++)
  {
    if ( (lcase[dsnew].dat[i] = (float *)malloc( (sum->nmax+1) * sizeof(float))) == NULL )
      printf("\n\n ERROR: malloc failure12\n\n" );	               
    if ( (lcase[dsnew].compName[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
      printf("\n\n ERROR: malloc failed13\n\n" );
    if ( (lcase[dsnew].icname[i] = (char *)malloc( MAX_LINE_LENGTH * sizeof(char))) == NULL )
      printf("\n\n ERROR: malloc failed14\n\n" );
    lcase[dsnew].menu[i] = 1;
    lcase[dsnew].max[i]=lcase[dss[0][0]].max[i];
    lcase[dsnew].min[i]=lcase[dss[0][0]].min[i];
    lcase[dsnew].ictype[i] = lcase[dss[0][0]].ictype[i];
    lcase[dsnew].icind1[i] = lcase[dss[0][0]].icind1[i];
    lcase[dsnew].icind2[i] = lcase[dss[0][0]].icind2[i];
    lcase[dsnew].iexist[i] = lcase[dss[0][0]].iexist[i];
    strcpy ( lcase[dsnew].compName[i], lcase[dss[0][0]].compName[i]);
  }


      dth=PI/THETA_STEPS;
      dthgrd=dth*180./ PI;

      /* search worst theta regarding stress */
      for (i=0; i<sum->n; i++ )
      {
	printf("search max for i:%d from %d\n", i, sum->n);
        max_amp=-1.; worst_th1=-1.; worst_th2=-1.;

        /* run over PI */
        for (th1=0; th1<=THETA_STEPS; th1++)
        {
          theta=dth*th1-PI*.25;
          ctl=cos(theta)*scal1;
          stl=-sin(theta)*scal1;
          for (j=0; j<6; j++)
            s1[j]=  ctl*lcase[dss[0][0]].dat[j][node[i].nr]-stl*lcase[dss[0][1]].dat[j][node[i].nr];

          /* run over 2PI */
          for (th2=0; th2<=THETA_STEPS*2; th2++)
	    //th2=0;
          {
            theta=dth*th2-PI*.25;
            ctl=cos(theta)*scal2;
            stl=-sin(theta)*scal2;
            for (j=0; j<6; j++)
              s2[j]=  s1[j] + ctl*lcase[dss[1][0]].dat[j][node[i].nr]-stl*lcase[dss[1][1]].dat[j][node[i].nr];

            /* calculate the worst principal stress */
            /* wenn flag=0  x[0]  >  x[1]  >  x[2]                              */
            /* wenn flag=1 |x[0]| > |x[1]| > |x[2]|                             */
          
            j=calcPrinc( s2, p, a1, a2, a3, 1 );
            if ( j == -9999 )
            {
              printf("Warning: at node:%d\n", node[i].nr );
            }
            else if ( j == -2 )
            {
              printf("Warning: at node:%d, value of maxPrinc set to 0.\n", node[i].nr );
              p[0] = 0.;
            }
            amp=p[0];
          
            if (amp*amp>=max_amp) { max_amp=amp*amp; worst_th1=th1;  worst_th2=th2; }
            if((nprint)&&(node[i].nr==nprint))
	    {
              //printf("th1:%d th2:%d theta:%f amp:%f max_amp:%f worst_theta:%f\n",th1, th2, theta, amp,sqrt(max_amp),worst_th1,worst_th2);
              fprintf(handle,"%d th1: %f th2: %f theta: %f amp: %f max_amp: %f worst_theta1: %f worst_theta2: %f\n",evalnr,dthgrd*th1, dthgrd*th2, theta, amp,sqrt(max_amp),worst_th1,worst_th2);
              evalnr++;
	    }
	  }

        }

        /* check if a worst-sector was found */
        if(worst_th1==-1)
        {
          printf("ERROR: no worst sector could be found. Talk with the programmer (K. Wittig).\n");
          exit(-1);
        }

        /* store the results for this sector */
        worst_th1=dth*worst_th1-PI*.25;
        worst_th2=dth*worst_th2-PI*.25;
        for (j=0; j<lcase[dsnew].ncomps; j++)
        {
          lcase[dsnew].dat[j][node[i].nr] = scal1*(cos(worst_th1)*lcase[dss[0][0]].dat[j][node[i].nr]+sin(worst_th1)*lcase[dss[0][1]].dat[j][node[i].nr]) + scal2*(cos(worst_th2)*lcase[dss[1][0]].dat[j][node[i].nr]+sin(worst_th2)*lcase[dss[1][1]].dat[j][node[i].nr]); 
        }
      }


  /* determine max and min for the new dataset */
  for (n=0; n<sum->n; n++ )
  {
    for(i=0; i<lcase[dsnew].ncomps; i++)
    {
      if (lcase[dsnew].dat[i][node[n].nr] >  lcase[dsnew].max[i])
      {  lcase[dsnew].max[i]=lcase[dsnew].dat[i][node[n].nr]; lcase[dsnew].nmax[i]=node[n].nr;}
      if (lcase[dsnew].dat[i][node[n].nr] <  lcase[dsnew].min[i])
      {  lcase[dsnew].min[i]=lcase[dsnew].dat[i][node[n].nr]; lcase[dsnew].nmin[i]=node[n].nr;}
    }
  }
  calcDatasets( dsnew, sum, node, lcase );
  recompileEntitiesInMenu(dsnew);
  if(nprint) fclose (handle);
}




void sendFacesNodes( char *setname )
{
  int   setNr;
  int  length, i,j,k,n;
  char prognam[MAX_LINE_LENGTH];
  FILE *handle;

  strcpy ( prognam, setname);
  length= strlen ( setname );
  setNr=getSetNr(setname);
  if (setNr<0)
  {
    printf (" ERROR: set:%s does not exist\n", setname);
    return;
  }
  strcpy (&prognam[length], ".efn");

  if(!set[setNr].anz_f)
  {
    printf(" WARNING: Found no faces in set:%s\n",set[setNr].name); 
    return;
  }

  /* write the faces in abaqus-format */
  handle = fopen (prognam, "w");
  if ( handle== NULL )
  {
    printf ("\nThe input file %s could not be opened.\n\n", prognam); 
    return;
  }
  fprintf(handle, "** elem,face,nodes based on %s\n", setname);
  for (k=0; k<set[setNr].anz_f; k++ )
  {
    i=set[setNr].face[k];
    fprintf(handle, "%d,S%d", face[i].elem_nr, face[i].nr+1);
    if (face[i].type == 7) n = 3;  /* TRI3  */
    else if (face[i].type == 8) n = 6;  /* TRI6  */
    else if (face[i].type == 9) n = 4;  /* QUAD4 */
    else if (face[i].type == 10) n = 8; /* QUAD8 */
    else if (face[i].type == 11) n = 2; /* beam2 */
    else if (face[i].type == 12) n = 3; /* beam3 */
    else n=0;
    for (j=0; j<n; j++) fprintf(handle, ",%d", face[i].nod[j]);
    fprintf(handle, "\n");
  }
  fclose(handle);

  printf (" ready\n");
}
