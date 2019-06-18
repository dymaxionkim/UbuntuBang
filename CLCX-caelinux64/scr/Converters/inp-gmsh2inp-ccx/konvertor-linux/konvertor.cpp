// konvertor
// Convert GMSH ABACUS INP file -> INP file for CGX
// by prool, 2015-2017
// gpl v2

#include "stdio.h"
#include "string.h"

#define BUFSIZE 256
#define MESSAGE printf

#define tmpfile "tmpfile.txt"
#define tmpfile2 "tmpfile2.txt"

// global variables
float scale;

char tolower(char c) // by prool
{
if ((c<='Z')&&(c>='A')) return c+('a'-'A');
else return c;
}

void strtolower(char *str) // by prool
{
while (*str) {*str=tolower(*str); str++;}
}

char *strstr_prool(char *str1, char *str2) // strstr() without case. foolish algorythm by prool
{
char strlocal1 [BUFSIZE], strlocal2 [BUFSIZE], *pp;

strcpy(strlocal1, str1);
strcpy(strlocal2, str2);
strtolower(strlocal1);
strtolower(strlocal2);
pp=strstr(strlocal1,strlocal2);
if (pp) return str1 + (strstr(strlocal1,strlocal2) - strlocal1);
else return pp;
}

char *strstr_curve_line(char *str) // unused code. prool
{char *pp;

pp=strstr_prool(str,"CURVE");
if (pp) return pp;
return strstr_prool(str,"LINE");
}

char *table(char *param)
{
printf("table() param=`%s'\n", param);
if ( (!strcmp(param,"C1D2")) || (!strcmp(param,"T3D2")) ) return "B31";
if ( (!strcmp(param,"C1D3")) || (!strcmp(param,"T3D3")) ) return "B32";
if ( (!strcmp(param,"C2D3")) || (!strcmp(param,"CPS3")) ) return "S3";
if ( (!strcmp(param,"C2D6")) || (!strcmp(param,"CPS6")) ) return "S6";
if ( (!strcmp(param,"C2D4")) || (!strcmp(param,"CPS4")) ) return "S4";
return param;
}

void replace(char *str, char *str1, char *str2)
{char kopiya[BUFSIZE]; char *pp, *pp2;

strcpy(kopiya,str);
pp=strstr(str,str1);
pp2=strstr(kopiya,str1);

if (!pp) return;
strcpy(kopiya,str);
strcpy(pp,str2);
strcpy(pp+strlen(str2),pp2+strlen(str1));

printf("replace debug\n1) %s\n2) %s\n", kopiya, str);
}

int commas9(char *str)
{
int counter;
counter=0;
while(*str) if (*str++==',') counter++;
if (counter==9) return 1;
else return 0;
}

void delete9(char *str)
{
int counter;
counter=0;
while(*str) {if (*str==',') counter++; if (counter==9) {*str++='\n'; *str=0; return; } else str++; }
}

int is_nondigit(char c)
{
if ((c>='0')&&(c<='9')) return 0;
return 1;
}

void exponentization(char *str)
{
int num;
float x,y,z;
sscanf(str,"%i,%f,%f,%f",&num,&x,&y,&z);
sprintf(str,"%i,%.12e,%.12e,%.12e\n",num,scale*x,scale*y,scale*z);
}

void prool_process2(const char*filename, const char *outfile)
{
FILE *fp, *fo;
char buf [BUFSIZE], buf2 [BUFSIZE], tmp_buf[BUFSIZE];
int i, l, status;

status=0; // 0 - status "zero" ("others sections"), 1 - section "NODE", 2 - section type=S4

if ((fp=fopen(filename, "r"))==0) {MESSAGE("prool_process2(): Unable to open input file '%s'", filename); return; }
if ((fo=fopen(outfile, "w"))==0) {MESSAGE("prool_process2(): Unable to create output file '%s'", filename); return; }

while(fgets(buf,BUFSIZE,fp))
	{
	replace(buf,"CURVE","ln");
	replace(buf,"SURFACE","sur");
	replace(buf,"VOLUME","vol");
	replace(buf,"Curve","ln");
	replace(buf,"Surface","sur");
	replace(buf,"Volume","vol");

	if (is_nondigit(buf[0]))
		{
		l=strlen(buf);
		for (i=0;i<l;i++) tmp_buf[i]=tolower(buf[i]);
#define NODE "*node"
		if (!memcmp(tmp_buf,NODE,strlen(NODE))) {fputs("*Node,NSET=Nall\n",fo); status=1;}
		else if (strstr(buf,"type=S4")) 
			{char strS4[BUFSIZE];
			strcpy(strS4,buf);
			fgets(buf,BUFSIZE,fp);
			if (commas9(buf)) {replace (strS4,"type=S4", "type=S8R"); delete9(buf);}
			fputs(strS4, fo);
			fputs(buf, fo);
			status=2;
			}
		else {status=0; fputs(buf, fo);}
		}
	else
		switch (status)
		{
		case 1: exponentization(buf);
		case 2: delete9(buf);
		default: fputs(buf, fo);
		}
	}
fclose(fp);
fclose(fo);
fflush(0);
}

#define NODELIST_SIZE 10000000
int nodelist [NODELIST_SIZE];

void clear_nodelist(void)
{int i;
printf("clear nodelist\n");
for (i=0;i<NODELIST_SIZE;i++) nodelist[i]=0;
}

void process_node (int node, FILE *fo)
{int i;
if (node==0) return;
printf("node=%i\n", node);
for (i=0;i<NODELIST_SIZE;i++)
	{
	if (nodelist[i]==node) return;
	if (nodelist[i]==0)
		{
		nodelist[i]=node;
		printf("print node %i\n", node);
		fprintf(fo,"%i\n",node);
		return;
		}
	}
fprintf(fo,"* ERROR: nodelist is small. prool\n");
printf("* ERROR: nodelist is small. prool\n");
}
                                                                                             
void process_nset_string(char *str, FILE *fo)
{char *pp; int digit;
pp=str;
printf("nset string=%s\n", pp);
while(*pp)
	{
	sscanf(pp,"%i",&digit);
	if (digit==0) break;
	process_node(digit,fo);
	pp=strchr(pp,',');
	if (pp) pp++; else break;
	if (!*pp) break;
	}
}
                                                                                             
void prool_process3(const char*filename, const char *outfile)
{
FILE *fp, *fo;
char buf [BUFSIZE], buf2 [BUFSIZE];
int status;

status=0; // 0 - status "zero" ("others sections"), 1 - section NSET,NSET

clear_nodelist();

if ((fp=fopen(filename, "r"))==0) {MESSAGE("prool_process3(): Unable to open input file '%s'", filename); return; }
if ((fo=fopen(outfile, "w"))==0) {MESSAGE("prool_process3(): Unable to create output file '%s'", filename); return; }

#define NSETNSET "*NSET,NSET="
while(fgets(buf,BUFSIZE,fp))
	{
	if (is_nondigit(buf[0]))
		{
			if (!memcmp(buf,NSETNSET,strlen(NSETNSET))) {status=1; clear_nodelist();}
		else status=0;
		fputs(buf, fo);
		}
	else
		{
		if (status) process_nset_string(buf,fo);
		else fputs(buf, fo);
		}
	}
fclose(fp);
fclose(fo);
fflush(0);
}

void prool_process(const char *filename, const char *outfile)
{
FILE *fp, *fo;
char buf [BUFSIZE], buf2 [BUFSIZE], clipboard [BUFSIZE], *c, *c2, *c3;
int flag_volume=0, flag_surface=0, delete_first_column=0;

//Msg::Info("prool_process(): filename = %s", filename); // prool

// scan file
if ((fp=fopen(filename, "r"))==0) {MESSAGE("prool_process(): Unable to open input file '%s'", filename); return; }

while(fgets(buf,BUFSIZE,fp))
	{
	//Msg::Info("prool_process: line = %s", buf);
	if (strstr(buf,"ELSET=VOLUME") || strstr(buf,"ELSET=Volume"))
		{
		MESSAGE("Found VOLUME\n");
		flag_volume=1;
		}
	if (strstr(buf,"ELSET=SURFACE") || strstr(buf,"ELSET=Surface"))
		{
		MESSAGE("Found SURFACE\n");
		flag_surface=1;
		}
	}
fclose(fp);

// copy file
if ((fp=fopen(filename, "r"))==0) {MESSAGE("prool_process: Unable to open input file '%s'", filename); return; }
if ((fo=fopen(outfile, "w"))==0) {MESSAGE("prool_process: Unable to create output file '%s'", filename); return; }
while(fgets(buf,BUFSIZE,fp))
	{
	if (flag_volume)
		{ // VOLUME
		if (strstr_prool(buf, "ELSET=VOLUME"))
			{
			delete_first_column=0;
			fputs(buf, fo);
			}
		else if (c=strstr_prool(buf, "ELSET=CURVE"))
			{
			delete_first_column=1;
			// *Element, type=C1D2, ELSET=CURVE1
			// *NSET,NSET=CURVE1
			strcpy(buf2, "*NSET,NSET=");
			strcat(buf2, c+6);
			// printf("`%s'-<`%s'\n",buf,buf2);
			fputs(buf2,fo);
			}
		else if (c=strstr_prool(buf, "ELSET=LINE")) // new spec
			{
			delete_first_column=1;
			// *Element, type=C1D2, ELSET=LINE1
			// *NSET,NSET=CURVE1
			strcpy(buf2, "*NSET,NSET=");
			strcat(buf2, c+6);
			// printf("`%s'-<`%s'\n",buf,buf2);
			fputs(buf2,fo);
			}
		else if (c=strstr_prool(buf, "ELSET=SURFACE"))
			{
			delete_first_column=1;
			// *Element, type=C2D3, ELSET=SURFACE6
			// *NSET,NSET=SURFACE6
			strcpy(buf2, "*NSET,NSET=");
			strcat(buf2, c+6);
			// printf("`%s'-<`%s'\n",buf,buf2);
			fputs(buf2,fo);
			}
		else
			{
			if (delete_first_column)
				{
				if (c = strchr(buf, ' ')) fputs(c+1,fo);
				else
					{
					MESSAGE("ERROR in delete first column!\n");
					fputs("ERROR in delete first column!", fo);
					}
				}
			else
				{
				fputs(buf, fo);
				}
			}
		}
	else if (flag_surface)
		{ // SURFACE
		if (c=strstr_prool(buf, "ELSET=SURFACE"))
			{
			delete_first_column=0;
			// *Element, type=C2D3, ELSET=SURFACE6
			// *Element, type=S6, ELSET=SURFACE6
			if((c2=strstr(buf,"type="))==0)
				{
					MESSAGE("ERROR: `type' is absent\n");
					fputs("ERROR: `type' is absent", fo);
				}
			strcpy(buf2, "*Element, type=");
			strcpy(clipboard,c2+5);
			if((c3=strchr(clipboard,','))==0)
				{
					MESSAGE("ERROR: comma is absent\n");
					fputs("ERROR: comma is absent", fo);
				}
			*c3=0;
			strcat(buf2,table(clipboard));
			strcat(buf2,", ");
			strcat(buf2,c);
			// printf("`%s'->`%s'", buf, buf2);
			fputs(buf2, fo);
			}
		else if (c=strstr_prool(buf,"ELSET=CURVE"))
			{
			delete_first_column=1;
			// *Element, type=C1D3, ELSET=CURVE4
			// *NSET,NSET=CURVE4
			strcpy(buf2, "*NSET,NSET=");
			strcat(buf2, c+6);
			// printf("`%s'-<`%s'\n",buf,buf2);
			fputs(buf2,fo);
			}
		else if (c=strstr_prool(buf,"ELSET=LINE"))
			{
			delete_first_column=1;
			// *Element, type=C1D3, ELSET=LINE4
			// *NSET,NSET=LINE4
			strcpy(buf2, "*NSET,NSET=");
			strcat(buf2, c+6);
			// printf("`%s'-<`%s'\n",buf,buf2);
			fputs(buf2,fo);
			}
		else
			{
			if (delete_first_column)
				{
				if (c = strchr(buf, ' ')) fputs(c+1,fo);
				else
					{
					MESSAGE("ERROR in delete first column!\n");
					fputs("ERROR in delete first column!", fo);
					}
				}
			else
				{
				fputs(buf, fo);
				}
			}
		}
	else
		{ // No Volume and No Surface
		if (c=strstr(buf, "ELSET="))
			{
			delete_first_column=0;
			// *Element, type=C2D3, ELSET=SURFACE6
			// *Element, type=S6, ELSET=SURFACE6
			if((c2=strstr(buf,"type="))==0)
				{
					MESSAGE("ERROR: `type' is absent\n");
					fputs("ERROR: `type' is absent", fo);
				}
			strcpy(buf2, "*Element, type=");
			strcpy(clipboard,c2+5);
			if((c3=strchr(clipboard,','))==0)
				{
					MESSAGE("ERROR: comma is absent\n");
					fputs("ERROR: comma is absent", fo);
				}
			*c3=0;
			strcat(buf2,table(clipboard));
			strcat(buf2,", ");
			strcat(buf2,c);
			// printf("`%s'->`%s'", buf, buf2);
			fputs(buf2, fo);
			}
		else
			{
				fputs(buf, fo);
			}
		}
	}
fclose(fp);
fclose(fo);
fflush(0);

}


int main(int argc, char* argv[])
{
	char file1 [BUFSIZE], file2 [BUFSIZE];
	printf("Konvertor v.0.6 by Prool. http://calculix.kharkov.org\n");
	printf("konvert GMSH ABACUS INP file -> INP file for CGX\n");
	scale=1;
	// printf("argc=%i argv[0]=%s\n",argc,argv[0]);
	if (argc==3)
	{
	sprintf(file1, "%s", argv[1]);
	sprintf(file2, "%s", argv[2]);
	prool_process(file1, tmpfile);
	prool_process2(tmpfile,tmpfile2);
	prool_process3(tmpfile2,file2);
	remove(tmpfile);
	remove(tmpfile2);
	}
	else if (argc==4)
	{char *pp;
	pp=argv[1];
	pp++;
	if (*pp) sscanf(pp,"%f",&scale);
	printf("scalefactor=%f\n", scale);
	sprintf(file1, "%s", argv[2]);
	sprintf(file2, "%s", argv[3]);
	prool_process(file1, tmpfile);
	prool_process2(tmpfile,tmpfile2);
	prool_process3(tmpfile2,file2);
	remove(tmpfile);
	remove(tmpfile2);
	}
	else {
		printf("\nUsage: konvertor [-scalefactor] inputfilename outputfilename\n");
	}
	// printf("\nPress any key\n"); getchar();
	return 0;
}
