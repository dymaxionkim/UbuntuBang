#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <cgx.h>


extern Sets *set;
extern Points *point;
extern Lines *line;

#define KONTAKTFLAECHENWINKEL 45
#define KFWINKELTOLERANZ 2
#define findPointToleranz 0.02

int fuss (Points **point_f, Lines **line_f, Sets **set_f, int anzSchaufel, double abstandAE);

void midLine(double *mPoint, double y1, double y2, double z1, double z2);
// calculate midpoint between two points
void findStartpoint(Points **point_f, Lines **line_f, int Lin[], int *p01, int *p02, double *kfAng, int *skS, double bS, double skAng, int flag);
// search for the 2 point that define the bearing surface
void gSchnittP(double *sp, double m1, double m2, double b1, double b2);
// calculate intersection of two straight lines
void gKreisSP(double *sp, double m, double b, double ym, double zm, double R);
// calculate intersection of a straight lines and a circle
void calcHN1(Points **point_f, int *pDH1, int pt1, int pt2, double bSKL , double bCS, double angSK, double angFS, double angL);
// calc point HN1
int genPointS(Points **point_f, int pt1, double ang, double absAE, char str[]);
// generate a point
int findPoint(Points **point_f, Lines **line_f, int Lin[], int pos, char str[]);
// search for the max of min distance between radius/straight line and center line
int findMin (Points **point_f);
// find the point with the minimum z value
double yAxAb(double y, double z, double m);
// calculate the axis intercept
double angleTan (double y2, double y1, double z2, double z1);
// calculate the angle using the TAN formulation
double calcRadius (double y2, double y1, double z2, double z1);
// calculate the distance between two points


int fuss (Points **point_f, Lines **line_f, Sets **set_f, int anzSchaufel, double abstandAE){
 struct fussValue{
  double DF_1;
  double EF_1;
  double HF_1;
  double PF_1;
  double RF_1;
  double GF_1;
  double DDF_1;
  double AF_1;
  double CF_1;
  double XF_1;
  double YF_1;
  double DF_2;
  double EF_2;
  double HF_2;
  double PF_2;
  double RF_2;
  double GF_2;
  double DDF_2;
  double AF_2;
  double CF_2;
  double XF_2;
  double YF_2;
 };

 struct scheibeValue{
  double DN_1;
  double EN_1;
  double HN_1;
  double PN_1;
  double RN_1;
  double DDN_1;
  double DN_2;
  double EN_2;
  double HN_2;
  double PN_2;
  double RN_2;
  double DDN_2;
  double BN;
  double RM;
  double a;
  double b;
  double Dum;
 };
 
 struct fussValue fV;
 struct scheibeValue sV;
 
 FILE *fr;
 int setNr=0; // set number of the set all
 int pNum=0; // control variable to store the point number of the point with the smallest z value 
 int p1=-1, p2=-1; // control variable to store a point number
 int endP1=-1, endP2=-1; // endpoint of the bearing surface
 int l1=-1, l2=-1; // control variable to store a line number 
 int i, run; // control variable
 int rCount, pCount, lCount; // count the radius, points and lines from the contour of interest
 int boolSK = -1; // boolean variable: boolSK = 1: skelett line exists; boolSK = 0: skelett line does not exist;
 int setNum = -1; // control varialbe to store the set number of the various steps
 int midPoint[2]; // point number of the two midpoints of the bearing surface
 int spSK[2]; // point number of the two intercept point of the skelett line and the bearing surface
 int pXF1= -1, pDF2 = -1, pBN = -1, p2DDN2 = -1; // point number of the point XF1, DF2, BN and P2DDN2.
 int lScheibe; // line number of the center line of the disk
 int skelett; // line number of the skelett line
 int disk, root, footPara, dummy; // set number of the set disk, root, footPara and dummy
 int sLines[250], fLines[250]; // line number of the disk and root contour, respectively
 int pDN1HN1[2]; // point number to calculate the distance DN1 and HN1
 double sRadius[20], fRadius[20]; // radii of all circles of the disk and root contour, respectively
 double skAngle = -1.0; // skelett angle
 double skkfAngle = -1.0; // angle define by the midpoint of the bearing surfaces
 double kfAngleF[4]; // angle of [0]: bearing surface 1; [1]: surface not in contact 1 ; [2]: bearing surface 2; [3]: surface not in contact 2 (for root)
 double kfAngleS[4];// angle of [0]: bearing surface 1; [1]: surface not in contact 1 ; [2]: bearing surface 2; [3]: surface not in contact 2 (for disk)
 double angle; // = skAngle or skkfAngle depending what angle is used for the evaluation 
 double angleFS; // = 180/Schaufelanzahl
 double midP[2]; // coordinate (y, z) of the midpoint between two points
 double SP[2]; // coordinate (y, z) of the intercept point of two lines
 double fMinZ = -1.0, sMinZ = -1.0; // smallest z value for disk and root point, respectively
 double b, b1, bSK = -1.0; // axis intercept; bSK axis intercept of the skelett line 
 double Pi; // Pi value
 double ang1; // control variable to store an angle
 double Radius; // radius
 double mult; // this variale is not used. There is a function to muliply all point with a constant value (inch->cm or vice versa)
 double beta; // angle beta in the fuss definition (fuss_input_deck.xls file)
 char str1[10]; // contains point and line names
 char datei[50]; // filename

 fV.DF_1 = 0.0;
 fV.DF_2 = 0.0;
 fV.EF_1 = 0.0;
 fV.EF_2 = 0.0;
 fV.HF_1 = 0.0;
 fV.HF_2 = 0.0;
 fV.PF_1 = 0.0;
 fV.PF_2 = 0.0;
 fV.RF_1 = 0.0;
 fV.RF_2 = 0.0;
 fV.GF_1 = 0.0;
 fV.GF_2 = 0.0;
 fV.DDF_1 = 0.0;
 fV.DDF_2 = 0.0;
 fV.AF_1 = 0.0;
 fV.AF_2 = 0.0;
 fV.CF_1 = 0.0;
 fV.CF_2 = 0.0;
 fV.XF_1 = 0.0;
 fV.XF_2 = 0.0;
 fV.YF_1 = 0.0;
 fV.YF_2 = 0.0;
 sV.DN_1 = 0.0;
 sV.DN_2 = 0.0;
 sV.EN_1 = 0.0;
 sV.EN_2 = 0.0;
 sV.HN_1 = 0.0;
 sV.HN_2 = 0.0;
 sV.PN_1 = 0.0;
 sV.PN_2 = 0.0;
 sV.RN_1 = 0.0;
 sV.RN_2 = 0.0;
 sV.DDN_1 = 0.0;
 sV.DDN_2 = 0.0;
 sV.BN = 0.0;
 sV.RM = 0.0;
 sV.a = 0.0;
 sV.b = 0.0;
 sV.Dum  = 0.0;

 strcpy(datei,"output_fuss.txt");
 mult = 1;
 Pi = 4.0 * atan(1.0);
//  abstandAE = 10.63;
//  anzSchaufel = 86;
 angleFS = 180/anzSchaufel;

//  Points *point;
//  Lines  *line;
//  Lcmb   *lcmb;
//  Sets   *set;

//  point = *point_f;
//  line = *line_f;
//  lcmb = *lcmb_f;
//  set = *set_f;

 skelett = pre_seta("skelett", "i", 0);
 if (skelett<0){
    errMsg (" ERROR: Set Skelett could not be created\n" );
    return EXIT_FAILURE;
 }

 disk = pre_seta("disk", "i", 0);
 if (disk<0){
    errMsg (" ERROR: Set disk could not be created\n" );
    return EXIT_FAILURE;
 }

 root = pre_seta("root", "i", 0);
 if (root<0){
    errMsg (" ERROR: Set root could not be created\n" );
    return EXIT_FAILURE;
 }
  
 footPara = pre_seta("footPara", "i", 0);
 if (root<0){
    errMsg (" ERROR: Set root could not be created\n" );
    return EXIT_FAILURE;
 }

 dummy = pre_seta("dummy", "i", 0);
 if (root<0){
    errMsg (" ERROR: Set root could not be created\n" );
    return EXIT_FAILURE;
 }
  
//  printf("Das steht in skelett drin: %d\n",skelett);
//  printf("Das steht in disk drin: %d\n",disk);
//  printf("Das steht in root drin: %d\n",root);
//  if(mult != 1){
//     multPoints(&point, &set, mult);
//  }
//  
 setNr=getSetNr("all");
 /*if(mult != 1){
  for(i=0; i < set[setNr].anz_p; i++){
    if(point[i].name!=(char *)NULL){
        point[i].px = point[i].px * mult;
        point[i].py = point[i].py * mult;
        point[i].pz = point[i].pz * mult;
     }
  }
 }*/
 
 
 pNum = findMin(&point);
//  printf("Das erste Minimum: %s \n", point[pNum].name);
 //printf("Bin nach findMin\n");
 if(pNum == -1){printf("ERROR: Point is locate above the AE line !\n\n"); return EXIT_FAILURE;}
 //
 //printf("Anzahl der Linien: %d\n", set[setNr].anz_l);
 //
 //
 /////////////////////////////
 //  check for skelett line
 /////////////////////////////
 //
 for(i=0; i<set[setNr].anz_l; i++){
//   printf("count i: %d\n", i);
//   printf("pNum: %d   Name: %s \n", pNum, point[pNum].name);
//   printf("line[i].p1: %d   Name: %s\n", line[i].p1, point[line[i].p1].name);
//   printf("line[i].p2: %d   Name: %s\n", line[i].p2, point[line[i].p2].name);
//   printf("count i: %d\n", i);
  if((pNum == line[i].p1) || (pNum == line[i].p2)){
    if((point[line[i].p1].py == 0) && (point[line[i].p2].py < 0) && (point[line[i].p1].pz != point[line[i].p2].pz)){
      bSK = point[line[i].p1].pz;
      pNum = line[i].p2;
     }
    else if((point[line[i].p1].py <0) && (point[line[i].p2].py == 0) && (point[line[i].p1].pz != point[line[i].p2].pz)){
      bSK = point[line[i].p2].pz;
      pNum = line[i].p1;
     }
    else {continue;}
//     printf("Linien Typ: %c\n", line[i].typ);
//     printf("Radiuspunkt: %d\n", line[i].trk);
    if(line[i].typ == 'a'){
//       printf("Name des dritten Punktes: %s\n", point[line[i].trk].name);
      printf("\nWARNING: No Skelett line found! Skelett angle is going to calculate using the midpoints of the bearing surfaces.\n");
      printf("It is not mandatory that the Skelett line is cut the bearing surfaces in their midpoints!! PLEASE DOUBLE CHECK!!\n\n");
      if(pNum == line[i].p1){
         pNum = line[i].p2;
      }
      else{pNum = line[i].p1;}
      boolSK = 0;
      skAngle = -1.0;
      bSK =-1.0;
//       printf("Das ist i in Sklettlinefunktion: %d\n",i);
//       printf("Name der Line in Sklett: %s\n", line[i].name);
      break;
//    IMPORTANT!! If no skelett line is found the variable i will be used in the next while loop
//
     }
    else if(line[i].typ == ' '){
      boolSK = 1;
      line[i].name = "Skelettline";
      seta(skelett, "l", set[setNr].line[i]);
      completeSet("skelett", "do");
      skAngle = angleTan(point[line[i].p2].py, point[line[i].p1].py, point[line[i].p2].pz, point[line[i].p1].pz);
      bSK = (yAxAb(point[line[i].p1].py, point[line[i].p1].pz, -tan(Pi*(90-skAngle)/180)));
//       printf("Sklettwinkel: %f\n", skAngle);
      point[line[i].p1].name = "SK1";
      point[line[i].p2].name = "SK2";
      break;
     }
    else {printf("\nERROR: This function doesnt work neither with spline nor with nurbs or anything else!! Sorry.\n"); return EXIT_FAILURE;}
  }
  if(i == (set[setNr].anz_l-1)){
    point[pNum].name = "Min";
    pNum = findMin(&point);
//     printf("das neue Minimum in Sklett: %s\n",point[pNum].name);
    i = 0;
  }
 }

 rCount = 1;
 lCount = 1;
 pCount = 1;
//  i = 0;
//
//
//
//////////////////////////////////////// 
//  reading lines from disk and root
////////////////////////////////////////
//
 if(boolSK == 1){
   pNum = findMin(&point);
 }
 

 run = 1;
//    i = 0;
   while(run < 3){
      if(run == 1){strcpy(str1,"D"); setNum = disk;}
      if(run == 2){strcpy(str1,"R"); setNum = root;}
//       printf("Das zweite Minimum: %s \n", point[pNum].name);
//       printf("run: %d\n", run);
      while((i < set[setNr].anz_l)){
//         if((line[i].name!=(char *)NULL) && (run == 2)){
//            printf("i:%d\n",i);
//            printf("Name der Line: %s\n", line[i].name);
//            printf("line[i].p1: %d   Name: %s\n", line[i].p1, point[line[i].p1].name);
//            printf("line[i].p2: %d   Name: %s\n", line[i].p2, point[line[i].p2].name);
//         }
        if(((line[i].name!=(char *)NULL)) && ((pNum == line[i].p1) || (pNum == line[i].p2)) && ((compare(line[i].name, "#", 1)==1))){
          if((pNum == line[i].p1) && (point[line[i].p2].py < 0)){
//              printf("pNum1: %d\n", pNum);
//              printf("Das ist pMum1: %s \n", point[pNum].name);
             if((run==1) && (point[line[i].p1].pz != point[line[i].p2].pz)){
                pNum = line[i].p2;
                seta(setNum, "l", set[setNr].line[i]);
             }
             else if(run == 2){
                pNum = line[i].p2;
                seta(setNum, "l", set[setNr].line[i]);
//                 printf("pNum2: %d\n", pNum);
//                 printf("Das ist pMum2: %s \n", point[pNum].name);
             }
             else{i++; continue;}
          }
          else if((point[line[i].p1].py <0) && (pNum == line[i].p2) ){
             if((run == 1) && (point[line[i].p1].pz != point[line[i].p2].pz)){
                pNum = line[i].p1;
                seta(setNum, "l", set[setNr].line[i]);
             }
             else if(run == 2){
                pNum = line[i].p1;
                seta(setNum, "l", set[setNr].line[i]);
//                 printf("pNum2: %d\n", pNum);
//                 printf("Das ist pMum2: %s \n", point[pNum].name);
             }
             else{i++; continue;}
          }
          else {i++; continue;}
          if(line[i].typ == 'a'){
                  if(run == 1){
                     sRadius[rCount] = calcRadius(point[line[i].p2].py, point[line[i].p1].py, point[line[i].p2].pz, point[line[i].p1].pz);
                     sRadius[0] = rCount;
                     sLines[lCount] = i;
                     sLines[0] = lCount;
//                      printf("Radius%d: %f\n", rCount, sRadius[rCount]);
                  }
                  if(run == 2){
                     fRadius[rCount] = calcRadius(point[line[i].p2].py, point[line[i].p1].py, point[line[i].p2].pz, point[line[i].p1].pz);
                     fRadius[0] = rCount;
                     fLines[lCount] = i;
                     fLines[0]= lCount;
                  }
//                   printf("pCount1: %d\n", pCount);
                  sprintf(line[i].name, "%sL%d", str1, lCount);
//                   printf("Name P1 vorher: %s\n", point[line[i].p1].name);
//                   printf("pCount2: %d\n", pCount);
//                    printf("point[line[i].p1].name %s\n", point[line[i].p1].name);
//                    printf("point[line[i].p2].name %s\n", point[line[i].p2].name);
                  sprintf(point[line[i].p1].name, "%sP%d", str1, pCount);
                  //printf("pCount3: %d\n", pCount);
//                   printf("point[line[i].p1].name %s\n", point[line[i].p1].name);
                  sprintf(point[line[i].p2].name, "%sP%d", str1, pCount+1);
//                    printf("point[line[i].p2].name %s\n", point[line[i].p2].name);
                  //printf("pCount4: %d\n", pCount);
                  sprintf(point[line[i].trk].name, "%sR%d", str1, rCount);
                  i=0;
                  lCount++;
                  rCount++;
                  pCount = pCount + 2;
                  //printf("pCount5: %d\n", pCount);
          }
          else if(line[i].typ == ' '){
                  if(run == 1){
                     sLines[lCount] = i;
                     sLines[0] = lCount;
                  }
                  if(run == 2){
                     fLines[lCount] = i;
                     fLines[0] = lCount;
                  }
//                   printf("pNum: ausgabe%d\n", pNum);
//                   printf("Das ist pMum ausgabe: %s \n", point[pNum].name);
                  sprintf(line[i].name, "%sL%d", str1, lCount);
                  sprintf(point[line[i].p1].name, "%sP%d", str1, pCount);
                  sprintf(point[line[i].p2].name, "%sP%d", str1, pCount+1);
                  i=0;
                  lCount++;
                  pCount = pCount + 2;
          }
        }
        else {i++;}
      }
      run += 1;
      rCount = 1;
      lCount = 1;
      pCount = 0;
      i=0;
      pNum = findMin(&point);
//       printf("das neue Minimum am ende: %s\n",point[pNum].name);
//       printf("run: %d\n", run);
   }
//    printf("Anzahl der Linien: %d\n",set[setNr].anz_l);
 //
 //

 completeSet("disk", "do");
 completeSet("root", "do");

 //
//
  /////////////////////////////////////////////////////////////////////////////////
 // determine the "skelett" angle from the 2 midpoints of the bearing surfaces
 /////////////////////////////////////////////////////////////////////////////////////
 //
 //printf("das steht in bSK drin: %f\n", bSK);
 //printf("das steht in spSK drin: %f\n", bSK);
 findStartpoint(&point, &line, fLines, &p1, &p2, kfAngleF, spSK, bSK, skAngle, 0);
 findStartpoint(&point, &line, sLines, &endP1, &endP2, kfAngleS, spSK, bSK, skAngle, 1);

 //printf("Das sind die Winkel: 1: %f ; 2: %f ; 3:%f; 4:%f\n",kfAngleS[0],kfAngleS[1],kfAngleS[2],kfAngleS[3]);
 //printf("Das sind die Winkel: 1: %f ; 2: %f ; 3:%f; 4:%f\n",kfAngleF[0],kfAngleF[1],kfAngleF[2],kfAngleF[3]);
 // Jetzt stehen in p1,... die Punktnummern drin!
 //if(p1 == -1){printf("Das hat nicht geklappt!!\n");}
 //printf("Name des Punktes p1: %s\n", point[p1].name);
 //printf("Name des Punktes p2: %s\n", point[p2].name);
 //printf("Name des Punktes p1: %s\n", point[p3].name);
 //printf("Name des Punktes p2: %s\n", point[p4].name);
  
 midLine(midP, point[p1].py, point[endP1].py, point[p1].pz, point[endP1].pz);
 //midP[0] = Y-Koordinate; midP[1]=Z-Koordinate des Mittelpunktes
 //printf("Das steht in midP drin: %f\n", midP[0]);
 //printf("Das steht in midP drin: %f\n", midP[1]);

 midPoint[0] = pnt("MP1", 0.0, midP[0], midP[1], 0);
  //printf("das ist midPoint: %d\n", midPoint[0]);
  //printf("und sein Name: %s\n", point[midPoint[0]].name);
 midLine(midP, point[p2].py, point[endP2].py, point[p2].pz, point[endP2].pz);
 midPoint[1] = pnt("MP2", 0.0, midP[0], midP[1], 0);
 p1 = pnt("PAE", 0.0, 0.0, point[midPoint[0]].pz, 0);
 p2 = pnt("PGF1", 0.0, 0.0, point[midPoint[1]].pz, 0);
 l1 = line_i("LGF2", p1, midPoint[0], -1, 4, 1.0, ' ');
 seta(footPara, "l", set[setNr].line[l1]);
 l1 = line_i("LGF1", p2, midPoint[1], -1, 4, 1.0, ' ');
 seta(footPara, "l", set[setNr].line[l1]);
 fV.GF_1 = abs(point[midPoint[1]].py);
 fV.GF_2 = abs(point[midPoint[0]].py);
 fV.YF_1 = 2*calcRadius(point[midPoint[1]].py, point[endP2].py, point[midPoint[1]].pz, point[endP2].pz);
 fV.YF_2 = 2*calcRadius(point[midPoint[0]].py, point[endP1].py, point[midPoint[0]].pz, point[endP1].pz);
//  printf("fV.GF_1: %f und fV.GF_2: %f \n", fV.GF_1, fV.GF_1);

 skkfAngle = angleTan(point[midPoint[1]].py, point[midPoint[0]].py, point[midPoint[1]].pz, point[midPoint[0]].pz);
 //printf("Angle between the Midpoints: %f\n", skkfAngle);
//
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Comparison of the skelett angle and the angle calculated from the midpoints of the bearing surfaces
//////////////////////////////////////////////////////////////////////////////////////////////////////////////
 if(boolSK == 1){
    if(skAngle == skkfAngle){
       angle = skAngle;
    }
    else{
       printf("\nWARNING: Skelett angle %f and the angle calculated from the bearing surface midpoints %f are not equal!\n", skAngle, skkfAngle);
       printf("For the further calculations the angle calculated from the bearing surface midpoints is used!!!\n\n");
       angle = skkfAngle;
       boolSK = 0; //Damit wird im Folgende bewirkt, dass die Mittelpunkte der Kontaktflaechen zur Bestimmung von AF_1 und AF_2 benutzt werden.
//        do{
//          printf("Please type \"2\" for the Skelett angle?\n");
//          printf("Please type \"7\" for the angle from the bearing surface midpoints?\n");
//          scanf ("%d",&i);
//        }while((i == 2) || (i == 7));
//        if(i == 2){
//           printf("The Skelett angle is used for further calculations.\n");
//           angle = skAngle;
//           boolsk = 1;
     }
 }
 else{
   printf("The angle calculated from the bearing surface midpoints is used for further calculations.\n");
   boolSK = 0; //Damit wird im Folgenden bewirkt, dass die Mittelpunkte der Kontaktflaechen zur Bestimmung von AF_1 und AF_2 benutzt werden.
   angle = skkfAngle;
   bSK = (yAxAb(point[midPoint[1]].py, point[midPoint[1]].pz, -tan(Pi*(90-skkfAngle)/180)));
   p1 = pnt("SK1", 0.0, 0.0, bSK, 0);
   if (abstandAE > 100){
      p2 = pnt("SK2", 0.0, ((25.4-bSK)/-tan(Pi*(90-skkfAngle)/180)), 25.4, 0);
   }
   else{
      p2 = pnt("SK2", 0.0, ((1-bSK)/-tan(Pi*(90-skkfAngle)/180)), 1, 0);
   }
   l1 = line_i("Skelett", p1, p2, -1, 4, 1.0, ' ');
   seta(footPara, "l", set[setNr].line[l1]);
 }
       
 printf("Skelett angle: %f\n",angle);
// angle = skAngle;
// boolSK = 0;
//  printf("Der Skelettwinkel %f und der Winkel %f\n", skAngle, angle);
 beta = 90 - kfAngleF[0] - angle;
  //
  //
  //
 //
 //////////////////////////////////////
 //  determine the root values
 ////////////////////////////////////
  //
  //
 run = 0;
 for(i=1; i <= fLines[0]; i++){
   if(i==1){
      if(point[line[fLines[i]].p1].py == 0) {
         fMinZ = point[line[fLines[i]].p1].pz;
         fV.DDF_2 = abs(point[line[fLines[i]].p2].py);
      }
      else{
         fMinZ = point[line[fLines[i]].p2].pz;
         fV.DDF_2 = abs(point[line[fLines[i]].p1].py);
      }
      //printf("fMinZ: %f\n",fMinZ);
      p1 = pnt("P1DDF2", 0.0, point[line[fLines[i]].p1].py, point[line[fLines[i]].p1].pz, 0);
      p2 = pnt("P2DDF2", 0.0, point[line[fLines[i]].p2].py, point[line[fLines[i]].p2].pz, 0);
      l1 = line_i("LDDF2", p1, p2, -1, 4, 1.0, ' ');
      seta(footPara, "l", set[setNr].line[l1]);
      p1 = pnt("P1XF2", 0.0, point[endP1].py, point[endP1].pz, 0);
//       printf("Angle: %f\n", angle);
//       printf("Steigung: %f\n", -tan(Pi*(90-angle)/180));
//       printf("Achsenabschnitt: %f\n", yAxAb(point[endP1].py, point[endP1].pz, -tan(Pi*(90-angle)/180)));
//       printf("fMinZ %f\n",fMinZ);
//       printf("yAxAb(point[endP1].py, point[endP1].pz, -tan(Pi*(90-angle)/180) %f\n", yAxAb(point[endP1].py, point[endP1].pz, -tan(Pi*(90-angle)/180));
//       printf("tan(Pi/180*angle) %f\n",tan(Pi/180*angle));
//       printf("(point[endP1].pz - fMinZ)*tan(Pi/180*angle) %f\n",(point[endP1].pz - fMinZ)*tan(Pi/180*angle));
      b = (yAxAb(point[endP1].py, point[endP1].pz, -tan(Pi*(90-angle)/180)));
      p2 = pnt("P2XF2", 0.0, ((fMinZ-b)/-tan(Pi*(90-angle)/180)), fMinZ, 0);
      //p2 = pnt("P2FX2", 0.0, (point[endP1].py+((point[endP1].pz - fMinZ)*tan(Pi/180*angle))), fMinZ, 0);
      l1 = line_i("LXF2", p1, p2, -1, 4, 1.0, ' ');
      seta(footPara, "l", set[setNr].line[l1]);
      fV.XF_2 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
   }
   else if(i == 2){
      if(point[line[fLines[i]].p1].pz > point[line[fLines[i]].p2].pz) {
         fV.EF_2 = abs(point[line[fLines[i]].p1].py);
         p1 = pnt("P1EF2", 0.0, 0.0, point[line[fLines[i]].p1].pz, 0);
         p2 = pnt("P2EF2", 0.0, point[line[fLines[i]].p1].py, point[line[fLines[i]].p1].pz, 0);
      }
      else{
         fV.EF_2 = abs(point[line[fLines[i]].p2].py);
         p1 = pnt("P1EF2", 0.0, 0.0, point[line[fLines[i]].p2].pz, 0);
         p2 = pnt("P2EF2", 0.0, point[line[fLines[i]].p2].py, point[line[fLines[i]].p2].pz, 0);
      }
      fV.PF_2 = abs(point[p1].pz - fMinZ);
      l1 = line_i("LEF2", p1, p2, -1, 4, 1.0, ' ');
      seta(footPara, "l", set[setNr].line[l1]);
      p2 = pnt("PPF2", 0.0, 0.0, fMinZ, 0);
      l1 = line_i("LPF2", p1, p2, -1, 4, 1.0, ' ');
      seta(footPara, "l", set[setNr].line[l1]);
   }
   
   else if(line[fLines[i]].typ == ' '){
      ang1 = angleTan(point[line[fLines[i]].p2].pz, point[line[fLines[i]].p1].pz, point[line[fLines[i]].p2].py, point[line[fLines[i]].p1].py);
      if((ang1 == kfAngleF[0]) && (run == 0)){
         run = 1;
         strcpy(str1,"P1DDF1DF2");
         p1 = findPoint(&point, &line, fLines, i+1, str1);
         pDF2 = p1;
         p2 = pnt("P2DDF1DF2", 0.0, 0.0, point[p1].pz, 0);
         l1 = line_i("LDDF1DF2", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         //printf("Else Bedingung Linienname %s: \n", line[i].name);
         //printf("Else BedingungLinienname +1 %s: \n", line[i+1].name);
         b = (yAxAb(point[pDF2].py, point[pDF2].pz, -tan(Pi*(90-angle)/180)));
         p2 = pnt("P2CF2", 0.0, ((fMinZ-b)/-tan(Pi*(90-angle)/180)), fMinZ, 0);
         l1 = line_i("LCF2", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         fV.CF_2 = calcRadius(point[p2].py, point[pDF2].py, point[p2].pz, point[pDF2].pz);
         midLine(midP, point[line[l1].p1].py, point[line[l1].p2].py, point[line[l1].p1].pz, point[line[l1].p2].pz);
         p1 = pnt("P1AF2", 0.0, midP[0], midP[1], 0);
         if(boolSK ==1){
//             printf("Name von SP2 %s\n", point[spSK[1]].name);
//             printf("Nummer von SP1 %d und SP2 %d\n", spSK[0], spSK[1]);
            b = (yAxAb(point[spSK[0]].py, point[spSK[0]].pz, -tan(Pi*(kfAngleF[0])/180)));
            p2 = pnt("SP3", 0.0, ((fMinZ-b)/-tan(Pi*(kfAngleF[0])/180)), fMinZ, 0);
            l2 = line_i("LSP1", spSK[0], p2, -1, 4, 1.0, ' ');
            seta(footPara, "l", set[setNr].line[l2]);
            b1 = (yAxAb(point[p1].py, point[p1].pz, tan(Pi*(kfAngleF[0])/180)));
//             printf("das ist b %f und b1 %f.\n", b, b1);
            gSchnittP(SP, -tan(Pi*(kfAngleF[0])/180) , tan(Pi*(kfAngleF[0])/180), b, b1);
            p2 = pnt("P2AF2", 0.0, SP[0], SP[1], 0);
            l1 = line_i("LAF2", p1, p2, -1, 4, 1.0, ' ');
            seta(footPara, "l", set[setNr].line[l1]);
            fV.AF_2 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
         }
         else{
            b = (yAxAb(point[midPoint[0]].py, point[midPoint[0]].pz, -tan(Pi*(kfAngleF[0])/180)));
            p2 = pnt("SP3", 0.0, ((fMinZ-b)/-tan(Pi*(kfAngleF[0])/180)), fMinZ, 0);
            l2 = line_i("LSP1", midPoint[0], p2, -1, 4, 1.0, ' ');
            seta(footPara, "l", set[setNr].line[l2]);
            b1 = (yAxAb(point[p1].py, point[p1].pz, tan(Pi*(kfAngleF[0])/180)));
//             printf("das ist b %f und b1 %f.\n", b, b1);
            gSchnittP(SP, -tan(Pi*(kfAngleF[0])/180) , tan(Pi*(kfAngleF[0])/180), b, b1);
            p2 = pnt("P2AF2", 0.0, SP[0], SP[1], 0);
            l1 = line_i("LAF2", p1, p2, -1, 4, 1.0, ' ');
            seta(footPara, "l", set[setNr].line[l1]);
            fV.AF_2 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
         }
         p1 = pnt("P1HF2", 0.0, 0.0, point[pDF2].pz, 0);
         p2 = pnt("P2HF2", 0.0, 0.0, -(abs(fMinZ)-fV.PF_2), 0);
         l1 = line_i("LHF2", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         fV.DDF_1 = abs(point[pDF2].py);
         fV.DF_2 = fV.DDF_1;
         fV.HF_2 = abs(point[pDF2].pz) + abs(fMinZ) - fV.PF_2;
      }
      else if(ang1 == kfAngleF[1]){
         pXF1 = line[fLines[i]].p1;
         strcpy(str1,"P1EF1");
         p1 = findPoint(&point, &line, fLines, i+1, str1);
         p2 = pnt("P2EF1", 0.0, 0.0, point[p1].pz, 0);
         l1 = line_i("LEF1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         fV.EF_1 = abs(point[p1].py);
         fV.PF_1 = abs(point[p1].pz) + abs(fMinZ) - fV.PF_2 - fV.HF_2;
         p1 = pnt("P2PF1", 0.0, 0.0, (point[p2].pz - fV.PF_1), 0);
         l1 = line_i("LPF1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
      }
      else if(ang1 == kfAngleF[2]){
         strcpy(str1,"P1DF1");
         p1 = findPoint(&point, &line, fLines, i+1, str1);
         p2 = pnt("P2DF1", 0.0, 0.0, point[p1].pz, 0);
         l1 = line_i("LDF1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         fV.DF_1 = abs(point[p1].py);
         fV.HF_1 = abs(point[p1].pz) + abs(fMinZ) - fV.PF_2 - fV.HF_2 - fV.PF_1;
         p1 = pnt("P2HF1", 0.0, 0.0, (point[p2].pz - fV.HF_1), 0);
         l1 = line_i("LHF1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         p1 = pnt("P1XF1", 0.0, point[endP2].py, point[endP2].pz, 0);
         b = (yAxAb(point[endP2].py, point[endP2].pz, -tan(Pi*(90-angle)/180)));
         b1 = (yAxAb(point[pXF1].py, point[pXF1].pz, -tan(Pi*(kfAngleF[1])/180)));
         gSchnittP(SP, -tan(Pi*(90-angle)/180), -tan(Pi*(kfAngleF[1])/180),  b, b1);
//          printf("das ist b %f und b1 %f.\n", b, b1);
         p2 = pnt("P2XF1", 0.0, SP[0], SP[1], 0);
         l1 = line_i("LXF1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         fV.XF_1 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
         p1 = pnt("P1CF1", 0.0, point[pDF2].py, point[pDF2].pz, 0);
         b = (yAxAb(point[pDF2].py, point[pDF2].pz, -tan(Pi*(90-angle)/180)));
         Radius = calcRadius(point[line[fLines[i+1]].trk].py, point[line[fLines[i+1]].p1].py, point[line[fLines[i+1]].trk].pz, point[line[fLines[i+1]].p1].pz);
//          printf("Radius: %f\n", Radius);
//          printf("Name pos +1: %s\n", line[fLines[i+1]].name);
         gKreisSP(SP, -tan(Pi*(90-angle)/180), b, point[line[fLines[i+1]].trk].py, point[line[fLines[i+1]].trk].pz, Radius);
         p2 = pnt("P2CF1", 0.0, SP[0], SP[1], 0);
         l1 = line_i("LCF1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         fV.CF_1 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);;
         midLine(midP, point[line[l1].p1].py, point[line[l1].p2].py, point[line[l1].p1].pz, point[line[l1].p2].pz);
         p1 = pnt("P1AF1", 0.0, midP[0], midP[1], 0);
         if(boolSK ==1){
  //             printf("Name von SP2 %s\n", point[spSK[1]].name);
  //             printf("Nummer von SP1 %d und SP2 %d\n", spSK[0], spSK[1]);
              b = (yAxAb(point[spSK[1]].py, point[spSK[1]].pz, -tan(Pi*(kfAngleF[2])/180)));
              p2 = pnt("SP4", 0.0, ((point[pDF2].pz-b)/-tan(Pi*(kfAngleF[2])/180)), point[pDF2].pz, 0);
              l2 = line_i("LSP2", spSK[1], p2, -1, 4, 1.0, ' ');
              seta(footPara, "l", set[setNr].line[l2]);
              b1 = (yAxAb(point[p1].py, point[p1].pz, tan(Pi*(kfAngleF[2])/180)));
  //             printf("das ist b %f und b1 %f.\n", b, b1);
              gSchnittP(SP, -tan(Pi*(kfAngleF[2])/180) , tan(Pi*(kfAngleF[2])/180), b, b1);
              p2 = pnt("P2AF1", 0.0, SP[0], SP[1], 0);
              l1 = line_i("LAF1", p1, p2, -1, 4, 1.0, ' ');
              seta(footPara, "l", set[setNr].line[l1]);
              fV.AF_1 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
           }
           else{
              b = (yAxAb(point[midPoint[1]].py, point[midPoint[1]].pz, -tan(Pi*(kfAngleF[2])/180)));
              p2 = pnt("SP4", 0.0, ((point[pDF2].pz-b)/-tan(Pi*(kfAngleF[2])/180)), point[pDF2].pz, 0);
              l2 = line_i("LSP2", midPoint[1], p2, -1, 4, 1.0, ' ');
              seta(footPara, "l", set[setNr].line[l2]);
              b1 = (yAxAb(point[p1].py, point[p1].pz, tan(Pi*(kfAngleF[2])/180)));
  //             printf("das ist b %f und b1 %f.\n", b, b1);
              gSchnittP(SP, -tan(Pi*(kfAngleF[2])/180) , tan(Pi*(kfAngleF[2])/180), b, b1);
              p2 = pnt("P2AF1", 0.0, SP[0], SP[1], 0);
              l1 = line_i("LAF1", p1, p2, -1, 4, 1.0, ' ');
              seta(footPara, "l", set[setNr].line[l1]);
              fV.AF_1 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
           }
      }
   }
 }
 fV.RF_1 = abs(abstandAE) + abs(point[pDF2].pz) + fV.PF_1;
 fV.RF_2 = fV.RF_1 -fV.PF_1 - fV.HF_2;

//
// 
//
//////////////////////////////////
//  determine the disk value
///////////////////////////////////
//
 ang1 = angleTan(point[line[sLines[sLines[0]]].p2].pz, point[line[sLines[sLines[0]]].p1].pz, point[line[sLines[sLines[0]]].p2].py, point[line[sLines[sLines[0]]].p1].py);
 b = (yAxAb(point[line[sLines[sLines[0]]].p2].py, point[line[sLines[sLines[0]]].p2].pz, -tan(Pi*(ang1)/180)));
 b1 = -abs(abstandAE);
 gSchnittP(SP, -tan(Pi*(ang1)/180) , -tan(Pi*(90-angleFS)/180), b, b1);
//  printf("abstand ae: %f\n", abstandAE);
//  printf("b %f und b1 %f\n", b, b1);
//  printf("SP %f und SP1 %f\n", SP[0], SP[1]);
//  printf("ang1 %f\n", ang1);
//  printf("winkel %f\n", kfAngleS[3]);
//  printf("Name LineS: %s\n", line[sLines[sLines[0]-1]].name);
//  printf("Name LineF: %s\n", line[fLines[fLines[0]-1]].name);
//  printf("Anzahl der LinenS: %d\n", sLines[13]);
//  printf("Anzahl der LinenF: %d\n", fLines[14]);
//  printf("Name LineS: %s\n", line[sLines[13]].name);
//  printf("Name LineF: %s\n", line[fLines[14]].name);
//  printf("P2 z: %f\n", point[line[sLines[sLines[0]]].p2].pz);
//  printf("P1 z: %f\n", point[line[sLines[sLines[0]]].p1].pz);
//  printf("P2 y: %f\n", point[line[sLines[sLines[0]]].p2].py);
//  printf("P1 y: %f\n", point[line[sLines[sLines[0]]].p1].py);
 p1 = pnt("P1MPS", 0, 0, -abs(abstandAE), 0);
 p2 = pnt("P2MPS", 0, SP[0], SP[1], 0);
 lScheibe = line_i("Scheibe", p1, p2, -1, 4, 1.0, ' ');
 seta(footPara, "l", set[setNr].line[lScheibe]);

 run = 0;
 for(i=1; i <= sLines[0]; i++){
   if(i==1){
      Radius = calcRadius(point[line[sLines[i]].trk].py, point[line[sLines[i]].p1].py, point[line[sLines[i]].trk].pz, point[line[sLines[i]].p1].pz);
      gKreisSP(SP, -tan(Pi*(90-angle)/180), bSK, point[line[sLines[i]].trk].py, point[line[sLines[i]].trk].pz, Radius);
      p1 = pnt("P1BN", 0, SP[0], SP[1],0);
      p2 = genPointS(&point, p1, angleFS, abstandAE, "P2BN");
      pBN = p2;
//       b = (yAxAb(point[p1].py, point[p1].pz, tan(Pi*(angleFS)/180)));
//       gSchnittP(SP, tan(Pi*(angleFS)/180) , -tan(Pi*(90-angleFS)/180), b, -abs(abstandAE));
//       p2 = pnt("P2BN", 0, SP[0], SP[1],0);
      l1 = line_i("LBN", p1, p2, -1, 4, 1.0, ' ');
      seta(footPara, "l", set[setNr].line[l1]);
      sMinZ = point[p2].pz;
      sV.BN = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
      sV.RM = calcRadius(point[p2].py, 0, point[p2].pz, -abs(abstandAE));
//       printf("RM: %f\n", sV.RM);
      p1 = findPoint(&point, &line, sLines, i+1, "P1DDN2");
      p2 = genPointS(&point, p1, angleFS, abstandAE, "P2DDN2");
      p2DDN2 = p2;
      l1 = line_i("LDDN2", p1, p2, -1, 4, 1.0, ' ');
      seta(footPara, "l", set[setNr].line[l1]);
      sV.DDN_2 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
      sV.Dum = calcRadius(point[p2].py, point[pBN].py, point[p2].pz, point[pBN].pz);
   }
   else if(line[sLines[i]].typ == ' '){
      ang1 = angleTan(point[line[sLines[i]].p2].pz, point[line[sLines[i]].p1].pz, point[line[sLines[i]].p2].py, point[line[sLines[i]].p1].py);
      if((ang1 == kfAngleS[0]) && (run == 0)){
         run = 1;
         p1 = findPoint(&point, &line, sLines, i+1, "P1EN2");
         p2 = genPointS(&point, p1, angleFS, abstandAE, "P2EN2");
         l1 = line_i("LEN2", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         sV.EN_2 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
         sV.PN_2 = calcRadius(point[p2].py, point[pBN].py, point[p2].pz, point[pBN].pz) - sV.Dum;
         sV.RN_2 = sV.RM + sV.Dum + sV.PN_2;
      }
      else if(ang1 == kfAngleS[1]){
         p1 = findPoint(&point, &line, sLines, i+1, "P1DDN1");
         p2 = genPointS(&point, p1, angleFS, abstandAE, "P2DDN1");
         l1 = line_i("LDDN1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         sV.DDN_1 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
         sV.DN_2 = sV.DDN_1;
         sV.HN_2 = calcRadius(point[p2].py, point[pBN].py, point[p2].pz, point[pBN].pz) - sV.Dum - sV.PN_2;
      }
      else if(ang1 == kfAngleS[2]){
         p1 = findPoint(&point, &line, sLines, i+1, "P1EN1");
         p2 = genPointS(&point, p1, angleFS, abstandAE, "P2EN1");
         l1 = line_i("LEN1", p1, p2, -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
         sV.EN_1 = calcRadius(point[p2].py, point[p1].py, point[p2].pz, point[p1].pz);
         sV.PN_1 = calcRadius(point[p2].py, point[pBN].py, point[p2].pz, point[pBN].pz) - sV.Dum - sV.PN_2 - sV.HN_2;
         sV.RN_1 = sV.RN_2 + sV.HN_2 + sV.PN_1;
      }
      else if(ang1 == kfAngleS[3]){
         b = (yAxAb(point[line[sLines[i]].p2].py, point[line[sLines[i]].p2].pz, -tan(Pi*(ang1)/180)));
//          printf("b: %f\n", b);
         gSchnittP(SP, -tan(Pi*(90-angle)/180) , -tan(Pi*(ang1)/180), bSK, b);
         p1 = pnt("P2HN2",0 , SP[0], SP[1], 0);
         calcHN1(&point, pDN1HN1, p1, line[lScheibe].p2, bSK, -abs(abstandAE), angle, angleFS, ang1);
         // pDN1HN1[0] is the point number of PHN1 and pDN1HN1[1] is the point number of PSSK
         sV.DN_1 = calcRadius(point[pDN1HN1[0]].py, point[pDN1HN1[1]].py, point[pDN1HN1[0]].pz, point[pDN1HN1[1]].pz);
         sV.HN_1 = calcRadius(point[pDN1HN1[0]].py, point[pBN].py, point[pDN1HN1[0]].pz, point[pBN].pz) - sV.Dum - sV.PN_2 - sV.HN_2 - sV.PN_1;
         l1 = line_i("LDN1", pDN1HN1[0], pDN1HN1[1], -1, 4, 1.0, ' ');
         seta(footPara, "l", set[setNr].line[l1]);
      }
   }
 }

//////////////////////////////////
// Calculate the values a and b
//////////////////////////////////
 b = (yAxAb(point[midPoint[1]].py, point[midPoint[1]].pz, -tan(Pi*(kfAngleS[2])/180)));
 b1 = (yAxAb(point[p2DDN2].py, point[p2DDN2].pz, tan(Pi*(kfAngleS[2])/180)));
 gSchnittP(SP,-tan(Pi*(kfAngleS[2])/180), tan(Pi*(kfAngleS[2])/180), b, b1);
 p1 = pnt("P1_a", 0, SP[0], SP[1], 0);
 l1 = line_i("L_a", p1, p2DDN2, -1, 4, 1.0, ' ');
 seta(footPara, "l", set[setNr].line[l1]);
 sV.a = calcRadius(point[p2DDN2].py, point[p1].py, point[p2DDN2].pz, point[p1].pz);

 b = (yAxAb(point[midPoint[0]].py, point[midPoint[0]].pz, tan(Pi*(kfAngleS[0])/180)));
 b1 = (yAxAb(point[p2DDN2].py, point[p2DDN2].pz, -tan(Pi*(kfAngleS[2])/180)));
 gSchnittP(SP,-tan(Pi*(kfAngleS[2])/180), tan(Pi*(kfAngleS[0])/180), b1, b);
 p1 = pnt("P1_b", 0, SP[0], SP[1], 0);
 l1 = line_i("L_b", p1, midPoint[0], -1, 4, 1.0, ' ');
 seta(footPara, "l", set[setNr].line[l1]);
 sV.b = calcRadius(point[midPoint[0]].py, point[p1].py, point[midPoint[0]].pz, point[p1].pz);



 completeSet("footPara", "do");


/////////////////
//  Data export
/////////////////
 fr = fopen(datei, "w");
 if(fr == NULL) { printf ("\nThe data file %s could not be opened.\n\n", datei); return EXIT_FAILURE; }

 fprintf(fr, "Amount of Blades \t %d\n", anzSchaufel);
 fprintf(fr, "Distance Disk Center to AE  \t %f\n", abstandAE);
 fprintf(fr, "RM \t %f\n", sV.RM);
 fprintf(fr, "BN \t %f\n", 2*sV.BN);
 fprintf(fr, "beta \t %f\n", beta);
 fprintf(fr, "alpha \t %f\n", angle);
 fprintf(fr, "a \t %f\n", sV.a);
 fprintf(fr, "b \t %f\n", sV.b);
 fprintf(fr, "DF_1 \t %f \t DF_2 \t %f \n" , 2*fV.DF_1, 2*fV.DF_2);
 fprintf(fr, "EF_1 \t %f \t EF_2 \t %f \n" , 2*fV.EF_1, 2*fV.EF_2);
 fprintf(fr, "HF_1 \t %f \t HF_2 \t %f \n" ,fV.HF_1, fV.HF_2);
 fprintf(fr, "PF_1 \t %f \t PF_2 \t %f \n" ,fV.PF_1, fV.PF_2);
 fprintf(fr, "RF_1 \t %f \t RF_2 \t %f \n" ,fV.RF_1, fV.RF_2);
 fprintf(fr, "GF_1 \t %f \t GF_2 \t %f \n" ,2*fV.GF_1, 2*fV.GF_2);
 fprintf(fr, "DDF_1 \t %f \t DDF_2 \t %f \n" ,2*fV.DDF_1, 2*fV.DDF_2);
 fprintf(fr, "AF_1 \t %f \t AF_2 \t %f \n" ,fV.AF_1, fV.AF_2);
 fprintf(fr, "CF_1 \t %f \t CF_2 \t %f \n" ,fV.CF_1, fV.CF_2);
 fprintf(fr, "XF_1 \t %f \t XF_2 \t %f \n" ,fV.XF_1, fV.XF_2);
 fprintf(fr, "YF_1 \t %f \t YF_2 \t %f \n" ,fV.YF_1, fV.YF_2);
 fprintf(fr, "DN_1 \t %f \t DN_2 \t %f \n" , 2*sV.DN_1, 2*sV.DN_2);
 fprintf(fr, "EN_1 \t %f \t EN_2 \t %f \n" , 2*sV.EN_1, 2*sV.EN_2);
 fprintf(fr, "HN_1 \t %f \t HN_2 \t %f \n" , sV.HN_1, sV.HN_2);
 fprintf(fr, "PN_1 \t %f \t PN_2 \t %f \n" , sV.PN_1, sV.PN_2);
 fprintf(fr, "RN_1 \t %f \t RN_2 \t %f \n" , sV.RN_1, sV.RN_2);
 fprintf(fr, "DDN_1 \t %f \t DDN_2 \t %f \n" , 2*sV.DDN_1, 2*sV.DDN_2);

 fclose(fr);
       
 *point_f = point;
 *line_f = line;
//  *lcmb_f = lcmb;
//  *set_f = set;
return EXIT_SUCCESS;
}


// void multPoints(Points **point_f, Sets **set_f, double multi){
//  int i, setNr;
//  Points point;
//  Sets set;
//        
//  point = *point_f;
//  set = *set_f;
//        
//  setNr = getSetNr("all");
//  
//  for(i=0; i < set[setNr].anz_p; i++){
//     point[i].px = point[i].px * multi;
//     point[i].py = point[i].py * multi;
//     point[i].pz = point[i].pz * multi;
//  }
//        
//  *point_f = point;
//  *set_f = set;
// }

       
void midLine(double *mPoint, double y1, double y2, double z1, double z2){
 *mPoint = 0.5 * (y1 + y2);
 *(mPoint+1) = 0.5 * (z1 + z2);
 }

void findStartpoint(Points **point_f, Lines **line_f, int Lin[], int *p01, int *p02, double *kfAng, int *skS, double bS, double skAng, int flag){
 Points *point;
 Lines  *line;
  
 int i, count, p1, p2;
 double angle, b, sP[2], Pi;

 point = *point_f;
 line = *line_f;
 p1 = *p01;
 p2 = *p02;

 Pi = 4.0 * atan(1.0);
 count = 0;
 if((flag == 0)){
   for(i=1; i<= Lin[0]; i++){
      if(line[Lin[i]].typ == ' '){
         angle = angleTan(point[line[Lin[i]].p2].pz, point[line[Lin[i]].p1].pz, point[line[Lin[i]].p2].py, point[line[Lin[i]].p1].py);
         //printf("Das ist der Winkel: %f", angle);
         //printf("Das ist der Name der Line: %s\n", line[Lin[i]].name);
         if((abs(KONTAKTFLAECHENWINKEL-angle)) <= KFWINKELTOLERANZ){
            if(count == 0){
               if(point[line[Lin[i]].p1].pz > point[line[Lin[i]].p2].pz){
                  p1 = line[Lin[i]].p2;
                  //printf("PName: %s ; LName: %s\n", point[line[Lin[i]].p2].name, line[Lin[i]].name);
               }
               else{
                  p1 = line[Lin[i]].p1;
               }
               *kfAng = angle;
               if(skAng !=-1.0){
                  b = (yAxAb(point[line[Lin[i]].p1].py, point[line[Lin[i]].p1].pz, tan(Pi*(90-angle)/180)));
                  gSchnittP(sP, -tan(Pi*(90-skAng)/180), tan(Pi*(90-angle)/180), bS, b);
                  *skS = pnt("SP1", 0.0, sP[0], sP[1],0);
               }
               count++;
            }
            else if(count == 2){
              if(point[line[Lin[i]].p1].pz > point[line[Lin[i]].p2].pz){
                  p2 = line[Lin[i]].p2;
                  //printf("PName: %s ; LName: %s\n", point[line[Lin[i]].p2].name, line[Lin[i]].name);
               }
               else{
                  p2 = line[Lin[i]].p1;
               }
               *(kfAng + count) = angle;
               if(skAng !=-1){
                  b = (yAxAb(point[line[Lin[i]].p1].py, point[line[Lin[i]].p1].pz, tan(Pi*(90-angle)/180)));
                  gSchnittP(sP, -tan(Pi*(90-skAng)/180), tan(Pi*(90-angle)/180), bS, b);
                  *(skS + 1) = pnt("SP2", 0.0, sP[0], sP[1],0);
               }
               count++;
            }
         }
         if((abs(angle)) > 7 && (abs(angle) < 40)){
              *(kfAng + count) = angle;
              count++;
         }
      }
   }
 }

if(flag == 1){
   for(i=1; i<= Lin[0]; i++){
      if(line[Lin[i]].typ == ' '){
         angle = angleTan(point[line[Lin[i]].p2].pz, point[line[Lin[i]].p1].pz, point[line[Lin[i]].p2].py, point[line[Lin[i]].p1].py);
//          printf("Das ist der Winkel: %f", angle);
         //printf("Das ist der Name der Line: %s\n", line[Lin[i]].name);
         if((abs(KONTAKTFLAECHENWINKEL-angle)) <= KFWINKELTOLERANZ){
            if(count == 0){
               if(point[line[Lin[i]].p1].pz > point[line[Lin[i]].p2].pz){
                  p1 = line[Lin[i]].p1;
//                   printf("PName: %s ; LName: %s\n", point[line[Lin[i]].p2].name, line[Lin[i]].name);
               }
               else{
                  p1 = line[Lin[i]].p2;
               }
               *kfAng = angle;
               count++;
            }
            else if(count == 2){
               if(point[line[Lin[i]].p1].pz > point[line[Lin[i]].p2].pz){
                  p2 = line[Lin[i]].p1;
//                   printf("PName: %s ; LName: %s\n", point[line[Lin[i]].p2].name, line[Lin[i]].name);
               }
               else{
                  p2 = line[Lin[i]].p2;
               }
               *(kfAng + count) = angle;
               count++;
            }
         }
         if((abs(angle)) > 7 && (abs(angle) < 40)){
          *(kfAng + count) = angle;
          count++;
         }
      }
   }
 }

 *point_f = point;
 *line_f = line;
 *p01 = p1;
 *p02 = p2;
}

void gSchnittP(double *sp, double m1, double m2, double b1, double b2){
 *sp = (b2 - b1)/(m1-m2);
 *(sp+1) = (m1*b2 - m2*b1)/(m1-m2);
}

void gKreisSP(double *sp, double m, double b, double ym, double zm, double R){
 double p, q, y, z;
 
 p = 2*(m*(b-zm)-ym)/(1 + pow(m,2));
 q = (pow(ym,2) + pow((b-zm),2) - pow(R,2))/(1 + pow(m,2));
 y = -p*0.5 + sqrt(pow((p*0.5),2) - q);
 z = zm - sqrt(pow(R,2) - pow((y - ym),2));
 *sp = y;
 *(sp+1) = z;
/*          printf("m: %f\n", m);
          printf("b: %f\n",b);
          printf("ym: %f\n",ym);
          printf("zm: %f\n",zm);
          printf("R: %f\n",R);
          printf("p: %f\n",p);
          printf("q: %f\n",q);
          printf("y: %f\n",y);
          printf("z: %f\n",z);*/
}

void calcHN1(Points **point_f, int *pDH1, int pt1, int pt2, double bSKL , double bCS, double angSK, double angFS, double angL){
 int ptHN1, ptMP, ptSSK, i, iter;
 double Pi, gamma, eps, phi, mp[2], hn1[2] , ssk[2], pDum1[]={0.0, 0.0}, pDum2[]={0.0, 0.0};
 double g, d, c, b, A1, A2, err;
 
 Pi = 4.0 * atan(1.0);
 gamma = 90.0 - angSK - angL;
 eps = 90.0 - angL - angFS;
 phi = angL + angFS;
 iter = 1000; // Number of maximum iterations

//  printf("gamma: %f, eps: %f, phi: %f\n", gamma, eps, phi);
 
 midLine(mp, point[pt1].py, point[pt2].py, point[pt1].pz, point[pt2].pz);
          
 for(i=0; i < iter; i++){
     b = (yAxAb(mp[0], mp[1], tan(Pi*(angFS)/180)));
     gSchnittP(hn1, tan(Pi*(angFS)/180) , -tan(Pi*(90-angFS)/180), b, -abs(bCS));
    //  ptHN1 = genPointS(&point, ptMP, angFS, bCS, "P1HN1");
     
     b = (yAxAb(hn1[0], hn1[1], tan(Pi*(angFS)/180)));
     gSchnittP(ssk, -tan(Pi*(90-angSK)/180), tan(Pi*(angFS)/180), bSKL, b);
     
     g = calcRadius(point[pt2].py, mp[0], point[pt2].pz, mp[1]);
     A2 = 0.5* pow(g,2) * cos(Pi*eps/180) * sin(Pi*eps/180);
    
     d = calcRadius(ssk[0], mp[0], ssk[1], mp[1]);
     c = calcRadius(ssk[0], point[pt1].py, ssk[1], point[pt1].pz);
     A1 = 0.5 * c * d * sin(Pi*(gamma+phi)/180);

     err = A1/A2;

     if((abs(1-err) < 0.0001)){
        break;
     }

     if(err < 1){ // A1 < A2
        if(i == 0){
           pDum1[0] = mp[0];
           pDum1[1] = mp[1];
           pDum2[0] = point[pt2].py;
           pDum2[1] = point[pt2].pz;
           midLine(mp, pDum1[0], pDum2[0], pDum1[1], pDum2[1]);
        }
        else{
           pDum1[0] = mp[0];
           pDum1[1] = mp[1];
           midLine(mp, pDum1[0], pDum2[0], pDum1[1], pDum2[1]);
        }
     }
     
     if(err > 1){ //  A1 > A2
        if( i== 0){
           pDum1[0] = point[pt1].py;
           pDum1[1] = point[pt1].pz;
           pDum2[0] = mp[0];
           pDum2[1] = mp[1];
           midLine(mp, pDum1[0], pDum2[0], pDum1[1], pDum2[1]);
        }
        else{
           pDum2[0] = mp[0];
           pDum2[1] = mp[1];
           midLine(mp, pDum1[0], pDum2[0], pDum1[1], pDum2[1]);
        }
     }

        if(err == 1){
           break;
        }
     
     if(i == iter-1){
        printf("The limit of %d iterations in function calcHN1 exceeded!\n", iter);
        printf("Area A1: %f\n", A1);
        printf("Area A2: %f\n", A2);
        printf("ERROR: %f\n", err);
     }
 }

//  printf("A1: %f, A2: %f, ERROR: %f\n", A1, A2, err);
 ptMP = pnt("PMP", 0, mp[0], mp[1], 0);
 ptHN1 = pnt("PHN1", 0, hn1[0], hn1[1], 0);
 ptSSK = pnt("PSSK", 0, ssk[0], ssk[1], 0);
 *pDH1 = ptHN1;
 *(pDH1 + 1) = ptSSK;

 *point_f = point;
}

int genPointS(Points **point_f, int pt1, double ang, double absAE, char str[]){

 double b, SP[2], Pi;
 int p;

 Pi = 4.0 * atan(1.0);

 b = (yAxAb(point[pt1].py, point[pt1].pz, tan(Pi*(ang)/180)));
 gSchnittP(SP, tan(Pi*(ang)/180) , -tan(Pi*(90-ang)/180), b, -abs(absAE));
 p = pnt(str, 0, SP[0], SP[1],0);

 *point_f = point;
 return(p);
}


int findPoint(Points **point_f, Lines **line_f, int Lin[], int pos, char str[]){
//  Points *point;
//  Lines  *line;
//  point = *point_f;
//  line = *line_f;

 int pt = -1;
 int count;
 double mP[2], dis, ratio;
      
 //printf("Funktionsaufruf findPoint\n");
 if((line[Lin[pos]].typ == 'a') && (abs(point[line[Lin[pos]].p1].py) <= abs(point[line[Lin[pos]].trk].py))){
   for(count=1; count < 5; count++){
      if(line[Lin[pos+1]].typ == ' '){
         break;
      }
      else{
         if(abs(point[line[Lin[pos]].p1].py) > abs(point[line[Lin[pos]].p2].py)){
            if((abs(point[line[Lin[pos]].p2].py) > abs(point[line[Lin[pos+1]].p1].py)) || (abs(point[line[Lin[pos]].p2].py) > abs(point[line[Lin[pos+1]].p2].py))){
               pos++;
            }
         }
         else if(abs(point[line[Lin[pos]].p2].py) > abs(point[line[Lin[pos]].p1].py)){
            if((abs(point[line[Lin[pos]].p1].py) > abs(point[line[Lin[pos+1]].p1].py)) || (abs(point[line[Lin[pos]].p1].py) > abs(point[line[Lin[pos+1]].p2].py))){
               pos++;
            }
         }
         else{
            break;
         }
      }
   }
   //Sprintf("ERROR: %d arc segments after contact area (see function in findPoint!)\n", count);
   //printf("Linienname %s: \n", line[Lin[pos]].name);
   //printf("Linienname +1 %s: \n", line[Lin[pos+1]].name);
   //return EXIT_FAILURE;
 }


 else if((line[Lin[pos]].typ == 'a') && (abs(point[line[Lin[pos]].p1].py) >= abs(point[line[Lin[pos]].trk].py))){
   for(count=1; count < 5; count++){
      if(line[Lin[pos+1]].typ == ' '){
         break;
      }
      else{
         if(abs(point[line[Lin[pos]].p1].py) < abs(point[line[Lin[pos]].p2].py)){
            if((abs(point[line[Lin[pos]].p2].py) < abs(point[line[Lin[pos+1]].p1].py)) || (abs(point[line[Lin[pos]].p2].py) < abs(point[line[Lin[pos+1]].p2].py))){
               pos++;
            }
         }
         else if(abs(point[line[Lin[pos]].p2].py) < abs(point[line[Lin[pos]].p1].py)){
            if((abs(point[line[Lin[pos]].p1].py) < abs(point[line[Lin[pos+1]].p1].py)) || (abs(point[line[Lin[pos]].p1].py) < abs(point[line[Lin[pos+1]].p2].py))){
               pos++;
            }
         }
         else{
            break;
         }
      }
   }
   //printf("ERROR: %d arc segments after contact area (see function in findPoint!)\n", count);
   //printf("Linienname %s: \n", line[Lin[pos]].name);
   //printf("Linienname +1 %s: \n", line[Lin[pos+1]].name);
   //return EXIT_FAILURE;
 }


 else{
   printf("ERROR: Straight line after the contact area in findPoint!!!\n");
   return EXIT_FAILURE;
 }
 
 ratio = point[line[Lin[pos+1]].p1].py / point[line[Lin[pos+1]].p2].py;
  
 if((line[Lin[pos+1]].typ == ' ') && (ratio >= (1-findPointToleranz)) && (ratio <= (1+findPointToleranz))){
    midLine(mP, point[line[Lin[pos+1]].p1].py, point[line[Lin[pos+1]].p2].py, point[line[Lin[pos+1]].p1].pz, point[line[Lin[pos+1]].p2].pz);
    pt = pnt(str, 0.0 , mP[0], mP[1], 0);
 }
 else{
    if((point[line[Lin[pos]].trk].pz > point[line[Lin[pos]].p1].pz) && (point[line[Lin[pos]].trk].pz > point[line[Lin[pos]].p2].pz)){
        if((point[line[Lin[pos+1]].trk].pz < point[line[Lin[pos+1]].p1].pz) || (point[line[Lin[pos+1]].trk].pz < point[line[Lin[pos+1]].p2].pz)){
            //printf("Else Bedingung Linienname %s: \n", line[Lin[pos]].name);
            //printf("Else BedingungLinienname +1 %s: \n", line[Lin[pos+1]].name);
            dis = calcRadius(point[line[Lin[pos+1]].trk].py, point[line[Lin[pos+1]].p1].py, point[line[Lin[pos+1]].trk].pz, point[line[Lin[pos+1]].p1].pz);
            //printf("ElseBed Wert fuer Distance: %f \n", dis);
            if((point[line[Lin[pos+1]].trk].py < point[line[Lin[pos+1]].p1].py) || (point[line[Lin[pos+1]].trk].py < point[line[Lin[pos+1]].p2].py)){
              mP[0] = point[line[Lin[pos+1]].trk].py + dis;
              //printf("ElseBed ifBed \n");
            }
            else{
              mP[0] = point[line[Lin[pos+1]].trk].py - dis;
              //printf("ElseBed elseBed \n");
            }
            mP[1] = point[line[Lin[pos+1]].trk].pz;
            //printf("ElseBed Werte fuer mP: mP1: %f und mP2: %f \n", mP[0], mP[1]);
            pt = pnt(str, 0.0 , mP[0], mP[1], 0);
        }
    }
    else if((point[line[Lin[pos]].trk].pz < point[line[Lin[pos]].p1].pz) || (point[line[Lin[pos]].trk].pz < point[line[Lin[pos]].p2].pz)){
       //printf("ElseIf 1 Bedingung Linienname %s: \n", line[Lin[pos]].name);
       //printf("ElseIf 1 BedingungLinienname +1 %s: \n", line[Lin[pos+1]].name);
       dis = calcRadius(point[line[Lin[pos]].trk].py, point[line[Lin[pos]].p1].py, point[line[Lin[pos]].trk].pz, point[line[Lin[pos]].p1].pz);
       if((point[line[Lin[pos]].trk].py < point[line[Lin[pos]].p1].py) || (point[line[Lin[pos]].trk].py < point[line[Lin[pos]].p2].py)){
          mP[0] = point[line[Lin[pos]].trk].py + dis;
        }
        else{
          mP[0] = point[line[Lin[pos]].trk].py - dis;
        }
        mP[1] = point[line[Lin[pos]].trk].pz;
        pt = pnt(str, 0.0 , mP[0], mP[1], 0);
    }
    else if((point[line[Lin[pos]].trk].pz == point[line[Lin[pos]].p1].pz) || (point[line[Lin[pos]].trk].pz == point[line[Lin[pos]].p2].pz)){
        //printf("ElseIf 2 Bedingung Linienname %s: \n", line[Lin[pos]].name);
        //printf("ElseIf 2 BedingungLinienname +1 %s: \n", line[Lin[pos+1]].name);
        dis = calcRadius(point[line[Lin[pos]].trk].py, point[line[Lin[pos]].p1].py, point[line[Lin[pos]].trk].pz, point[line[Lin[pos]].p1].pz);
        if((point[line[Lin[pos]].trk].py < point[line[Lin[pos]].p1].py) || (point[line[Lin[pos]].trk].py < point[line[Lin[pos]].p2].py)){
          mP[0] = point[line[Lin[pos]].trk].py + dis;
        }
        else{
          mP[0] = point[line[Lin[pos]].trk].py - dis;
        }
        mP[1] = point[line[Lin[pos]].trk].pz;
        pt = pnt(str, 0.0 , mP[0], mP[1], 0);
        if((point[line[Lin[pos+1]].trk].pz != point[line[Lin[pos+1]].p1].pz) && (point[line[Lin[pos+1]].trk].pz != point[line[Lin[pos+1]].p2].pz)){
          printf("WARNING: No contiuous transition between the lines %s and %s.\n",line[Lin[pos]].name, line[Lin[pos+1]].name);
        }
    }
    else{
       printf("ERROR: in Funktion findPoint. No lines fit the search criteria.\n");
       return EXIT_FAILURE;
    }
 }
 if (pt == -1) {
    printf("\nERROR pt = -1 in findPoint. \n");
    printf("Line name %s: \n", line[Lin[pos]].name);
    printf("Z coordinate of P1: %f\n", point[line[Lin[pos]].p1].pz);
    printf("Z coordinate of P2: %f\n", point[line[Lin[pos]].p2].pz);
    printf("Z coordinate of trk: %f\n\n", point[line[Lin[pos]].trk].pz);
    return EXIT_FAILURE;
 }
 *point_f = point;
 return(pt);
}

int findMin (Points **point_f){
 int i;
 int count=0;
 int setNr=0;
 double min=10000.0;
 Points *point;
 point = *point_f;

 setNr=getSetNr("all");
  for(i=0; i<set[setNr].anz_p; i++){
//   printf("i: %d; set[setNr].anz_p: %d\n", i, set[setNr].anz_p);
      if((point[i].name!=(char *)NULL) && (compare(point[i].name, "#", 1)==1) && (point[i].py == 0.0)){
         if(point[i].pz < min){
            count = i;
            min = point[i].pz;
         }
      }
  }
 *point_f = point;
 if(point[count].pz <= 0){return(count);}
 else{count=-1; return(count);}
}

double yAxAb(double y, double z, double m){
  double b;
  b = z - m*y;
  return(b);
}

double angleTan (double y2, double y1, double z2, double z1){
 double angle, Pi;
 Pi = 4.0 * atan(1.0);
 angle = abs((180.0/Pi)*(atan((y2 - y1)/(z2 - z1))));
 //printf("Das ist y2: %f\n", y2);
 //printf("Das ist y1: %f\n", y1);
 //printf("Das ist z2: %f\n", z2);
 //printf("Das ist z1: %f\n", z1);
 //printf("Das ist PI: %f\n", Pi);
 //printf("Das ist aTan: %f\n", (atan((y2 - y1)/(z2 - z1))));
 //printf("Das ist der Skelettwinkel: %f\n", angle);
 return(angle);
}


double calcRadius(double y2, double y1, double z2, double z1){
 double radius = sqrt(pow((y2-y1),2) + pow((z2-z1),2));
 return(radius);
}

