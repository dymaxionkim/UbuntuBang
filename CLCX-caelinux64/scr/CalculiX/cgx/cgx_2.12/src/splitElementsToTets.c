#include <extUtil.h>


int splitElementsToTets(int anz_e, Nodes *node, Elements  *elem, Tetraeder **ptet)
{
  int i,j,k,e,t=0;
  //int hextet[]=  {0,2,5,7, 0,5,2,1, 2,7,0,3, 0,7,5,4, 2,5,7,6 }; // inside out
  int hextet[]=  {7,2,5,0, 1,5,2,0, 3,7,0,2, 4,7,5,0, 6,5,7,2 };
  /* 8 *4 */
  int tet10tet[]={4,1,5,8, 5,2,6,9, 6,0,4,7, 7,8,9,3,  4,5,6,7, 4,5,7,8, 6,7,5,9, 8,9,5,7 };
  /* 5 *4, no midside nodes */
  /* int hex20tet[]=  {7,2,5,0, 1,5,2,0, 3,7,0,2, 4,7,5,0, 6,5,7,2 }; */
  /* 18 *4, bad shapes */
  /*int hex20tet[]={8,16,18,4, 8,18,10,3, 4,8,3,18, 4,12,3,8, 12,11,3,8, 12,0,11,8, 4,3,15,18, 4,15,19,18, 19,15,7,18,
    10,18,16,6, 10,16,8,1, 6,10,1,16, 6,14,1,10, 14,9,1,10, 14,2,9,10, 6,1,13,16, 6,13,17,16, 17,13,5,16 }; */
  /* 24 *4, based on netgen, inner tets run over the full length of the element which causes undesired results */
  /*int hex20tet[]={4 ,12 ,19 ,16,  8 ,12 ,11 ,0,  8 ,16 ,13 ,18,  8 ,9 ,13 ,1,  10 ,8 ,12 ,11,  10 ,11 ,12 ,15,
  10 ,15 ,12 ,18,  10 ,18 ,12 ,8,  10 ,9 ,2 ,14,  11 ,3 ,15 ,10,  15 ,18 ,19 ,12,  18 ,12 ,8 ,16,  19 ,12 ,18 ,16,
  19 ,18 ,15 ,7,  13 ,8 ,10 ,9,  13 ,16 ,17 ,18,  13 ,18 ,10 ,8,  13 ,18 ,17 ,14,  13 ,9 ,10 ,14,  13 ,14 ,10 ,18,
  17 ,16 ,13 ,5,  17 ,18 ,6 ,14 }; */
  /* 44 *4, incl. midside nodes */
  int hex20tet[]={22,24,20,23, 20,25,22,23, 20,24,22,21, 22,25,20,21,  24,20,23,11, 24,23,22,11, 20,24,21,9, 24,22,21,9, 
                  23,20,25,19, 23,25,22,19, 21,25,20,17, 21,22,25,17,  23,12,0,20, 23,3,15,22, 1,13,21,20, 21,14,2,22, 
                  4,12,23,20, 7,23,15,22, 13,5,21,20, 14,21,6,22,      0,11,23,20, 11,3,23,22, 1,9,21,20, 2,9,21,22,
                  19,4,23,20, 19,23,7,22, 17,5,21,20, 6,21,17,22,      0,24,11,20, 11,24,3,22, 24,1,9,20, 24,9,2,22, 
                  25,19,4,20, 25,7,19,22, 17,5,25,20, 17,25,6,22,      8,20,24,0,  24,10,22,3, 20,8,24,1, 22,24,10,2, 
                  16,20,25,4, 18,25,22,7, 16,20,25,5, 18,25,22,6 };
  int petet[]=   {0,1,2,4, 5,3,0,4, 0,4,2,5 };
  /* 15 *4 */
  int pe15tet[]=   { 6,1,7,10, 13,4,12,10, 13,12,6,10, 6,7,13,10, 9,8,6,0, 7,8,11,2, 14,13,11,5, 9,12,14,3, 9,13,14,12, 9,13,11,14, 7,6,9,8, 9,13,7,11, 13,12,9,7, 12,6,9,7, 9,8,11,2 };

  Tetraeder *tet=NULL;
  double v12[3], v13[3], v14[3], vn[3];

  for (e=0; e<anz_e; e++)
  {
    switch(elem[e].type)
    {
      case 1:
      if ( (tet = (Tetraeder *)realloc((Tetraeder *)tet, (t+5) * sizeof(Tetraeder))) == NULL )
        printf(" ERROR: realloc in elemToTet()\n\n") ;
      for (j=0; j<5; j++)
      {
        tet[t].e=e;
        for (k=0; k<3; k++) tet[t].cg[k]=0.;
        for (k=0; k<4; k++)
        {
          tet[t].n[k] = elem[e].nod[hextet[4*j+k]];
         /* simple cg = sum of coordinates */
          tet[t].cg[0]+= node[tet[t].n[k]].nx;
          tet[t].cg[1]+= node[tet[t].n[k]].ny;
          tet[t].cg[2]+= node[tet[t].n[k]].nz;
        }
        for (k=0; k<3; k++) tet[t].cg[k]/=4.;
        t++;
      }
      break;

      case 2:
      if ( (tet = (Tetraeder *)realloc((Tetraeder *)tet, (t+3) * sizeof(Tetraeder))) == NULL )
        printf(" ERROR: realloc in elemToTet()\n\n") ;
      for (j=0; j<3; j++)
      {
        tet[t].e=e;
        for (k=0; k<3; k++) tet[t].cg[k]=0.;
        for (k=0; k<4; k++)
        {
          tet[t].n[k] = elem[e].nod[petet[4*j+k]];
         /* simple cg = sum of coordinates */
          tet[t].cg[0]+= node[tet[t].n[k]].nx;
          tet[t].cg[1]+= node[tet[t].n[k]].ny;
          tet[t].cg[2]+= node[tet[t].n[k]].nz;
        }
        for (k=0; k<3; k++) tet[t].cg[k]/=4.;
        t++;
      }
      break;

      case 3:
      if ( (tet = (Tetraeder *)realloc((Tetraeder *)tet, (t+1) * sizeof(Tetraeder))) == NULL )
        printf(" ERROR: realloc in elemToTet()\n\n") ;
      for (k=0; k<3; k++) tet[t].cg[k]=0.;
      for (k=0; k<4; k++)
      {
        tet[t].e=e;
        tet[t].n[k] = elem[e].nod[k];
        /* simple cg = sum of coordinates */
        tet[t].cg[0]+= node[tet[t].n[k]].nx;
        tet[t].cg[1]+= node[tet[t].n[k]].ny;
        tet[t].cg[2]+= node[tet[t].n[k]].nz;
      }
      for (k=0; k<3; k++) tet[t].cg[k]/=4.;
      t++;
      break;

      case 4:
        /* 5,18,24,44 */
        if ( (tet = (Tetraeder *)realloc((Tetraeder *)tet, (t+44) * sizeof(Tetraeder))) == NULL )
          printf(" ERROR: realloc in elemToTet()\n\n") ;
        for (j=0; j<44; j++)
        {
          tet[t].e=e;
          for (k=0; k<3; k++) tet[t].cg[k]=0.;
          for (k=0; k<4; k++)
          {
            tet[t].n[k] = elem[e].nod[hex20tet[4*j+k]];
            tet[t].cg[0]+= node[tet[t].n[k]].nx;
            tet[t].cg[1]+= node[tet[t].n[k]].ny;
            tet[t].cg[2]+= node[tet[t].n[k]].nz;
          }
          for (k=0; k<3; k++) tet[t].cg[k]/=4.;
          t++;
	}
      break;

      case 5:
        if ( (tet = (Tetraeder *)realloc((Tetraeder *)tet, (t+15) * sizeof(Tetraeder))) == NULL )
          printf(" ERROR: realloc in elemToTet()\n\n") ;
        for (j=0; j<15; j++)
        {
          tet[t].e=e;
          for (k=0; k<3; k++) tet[t].cg[k]=0.;
          for (k=0; k<4; k++)
          {
            tet[t].n[k] = elem[e].nod[pe15tet[4*j+k]];
            tet[t].cg[0]+= node[tet[t].n[k]].nx;
            tet[t].cg[1]+= node[tet[t].n[k]].ny;
            tet[t].cg[2]+= node[tet[t].n[k]].nz;
          }
          for (k=0; k<3; k++) tet[t].cg[k]/=4.;
          t++;
	}
      break;

      case 6:
      if ( (tet = (Tetraeder *)realloc((Tetraeder *)tet, (t+8) * sizeof(Tetraeder))) == NULL )
        printf(" ERROR: realloc in elemToTet()\n\n") ;
      for (j=0; j<8; j++)
      {
        tet[t].e=e;
        for (k=0; k<3; k++) tet[t].cg[k]=0.;
        for (k=0; k<4; k++)
        {
          tet[t].n[k] = elem[e].nod[tet10tet[4*j+k]];
          /* simple cg = sum of coordinates */
          tet[t].cg[0]+= node[tet[t].n[k]].nx;
          tet[t].cg[1]+= node[tet[t].n[k]].ny;
          tet[t].cg[2]+= node[tet[t].n[k]].nz;
        }
        for (k=0; k<3; k++) tet[t].cg[k]/=4.;
        t++;
      }
      break;
  
      printf(" ERROR: Elem type:%d not supported\n", elem[e].type);      
    }
  }

  /* volu = 1/6 * a x b * c */
  for (i=0; i<t; i++)
  {
    v_result( &node[tet[i].n[0]].nx, &node[tet[i].n[1]].nx, v12);   
    v_result( &node[tet[i].n[0]].nx, &node[tet[i].n[2]].nx, v13);   
    v_result( &node[tet[i].n[0]].nx, &node[tet[i].n[3]].nx, v14);   
    v_prod(v12,v13,vn);
    tet[i].v=v_sprod(vn,v14)/6.;
    if(tet[i].v<0.) { tet[i].v=-tet[i].v; k=tet[i].n[1]; tet[i].n[1]=tet[i].n[2]; tet[i].n[2]=k; } 
    //printf("%d vol:%e vprod:%e %e %e sprod:%e\n", i, tet[i].v, vn[0],vn[1],vn[2], v_sprod(vn,v14) );
  }                                                  

  *ptet=tet;
  return(t);
}
