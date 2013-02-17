#include<stdio.h>
#include<stdlib.h>
#define MAX_VARS 4 /*variables in the product term*/
#define TRUE 1
#define FALSE 0
#define MAXTERMS 16 /*since maximum number of variables for an n variable is pow(2,n)*/

struct term
{
    unsigned t:4;
    unsigned f:4;
    int compterms[MAXTERMS];
    int nn;
};
typedef struct term TERM;

//take a term and store its corresponding representation in this TERM var

/********************************FUNCTION DEFINITIONS****************************************/

void init(TERM *tr,int n);
int combinable(TERM tt1,TERM tt2);
int onebit(int num);
void combine(TERM m1,TERM m2,TERM *m3);
int EqualTerms(TERM tt1,TERM tt2);
void printTerm(TERM tt1);
int inDontCares(int n);
void makezeros(int column,int final_index,int table[][MAXTERMS]);
void sort_both(int row_ones[],int pos[],int count);
void print_solutions(int ii,int table1[][MAXTERMS],int final_index,TERM essential1[],int ess);

/*********************************************************************************************/

/**********************************GLOBAL VARS************************************************/

TERM terms[MAX_VARS+1][MAXTERMS];//calculate the terms at eacch level
int dont_care[MAX_VARS+1][MAXTERMS];//sees if it is completely made up of dont care terms
int numTerms[MAX_VARS+1];//the number of terms at each level
TERM prime_implicants[MAXTERMS];//stores the prime implicants

/*********************************************************************************************/


int main()
{
    static int table[MAXTERMS][MAXTERMS];//the table
    int covered[MAX_VARS+1][MAXTERMS];//checks to see if it was covered in the next level
    int m;
    int j,k,p;
    TERM tempTerm;
    int found;
    /*Initialize number of terms at each level m*/
    for(m=0;m<MAX_VARS+1;m++)
    numTerms[m]=0;
    /*read input minterms from user*/
    int in;
    printf("Enter the number of minterms to be entered ::");
    scanf("%d",&in);
    printf("Enter the normal terms::\n");
    int ind=0,trm;
    numTerms[0]=in;
    for(ind=0;ind<in;ind++)
    {
        scanf("%d",&trm);
        init(&terms[0][ind],trm);
        covered[0][ind]=FALSE;
        dont_care[0][ind]=FALSE;
    }
    printf("Enter the number of dont care terms::");
    int dc;
    scanf("%d",&dc);
    numTerms[0]+=dc;
    int counter;
    for(counter=0;counter<dc;counter++)
    {
        scanf("%d",&trm);
        init(&terms[0][ind+counter],trm);
        covered[0][ind+counter]=FALSE;
        dont_care[0][ind+counter]=TRUE;
    }
    for(m=0;m<MAX_VARS;m++)
    for(j=0;j<numTerms[m];j++)
    for(k=j+1;k<numTerms[m];k++)
    {
        int both_dontcare=FALSE;
        if(combinable(terms[m][j],terms[m][k]))
        {
            covered[m][j]=TRUE;
            covered[m][k]=TRUE;
            if((dont_care[m][j]==TRUE)&&(dont_care[m][k]==TRUE))
            both_dontcare=TRUE;
            combine(terms[m][j],terms[m][k],&tempTerm);
            found=FALSE;
            for(p=0;p<numTerms[m+1];p++)
            if (EqualTerms(terms[m+1][p],tempTerm))
            found=TRUE;
            if(!found)
            {
                numTerms[m+1]=numTerms[m+1]+1;
                terms[m+1][numTerms[m+1]-1]=tempTerm;
                covered[m+1][numTerms[m+1]-1]=FALSE;
                if(both_dontcare)
                dont_care[m+1][numTerms[m+1]-1]=TRUE;
                else
                dont_care[m+1][numTerms[m+1]-1]=FALSE;
            }
        }
    }
    //creating the minterm table
    /*******************get the number of nonrepeating final prime implicants*****************/
    int final_index=0,ii=0;
    for(m=0;m<MAX_VARS;m++)
    for(j=0;j<numTerms[m];j++)
    {
        if((!covered[m][j])&&(!dont_care[m][j]))
        {
            int flag=1;
            for(ii=0;ii<final_index;ii++)
            {
                if(EqualTerms(terms[m][j],prime_implicants[ii]))
                flag=0;
            }
            if(flag)
            {
                prime_implicants[final_index]=terms[m][j];
                final_index++;
            }
        }
    }
    //and finally the table
    /*setting the table values according to the prime implicants*/
    for(ii=0;ii<final_index;ii++)
    {
        for(j=0;j<prime_implicants[ii].nn;j++)
        table[ii][prime_implicants[ii].compterms[j]]=1;
    }
    for(ii=0;ii<MAXTERMS;ii++)
    {
        if(inDontCares(ii))
        for(j=0;j<final_index;j++)
        table[j][ii]=0;
    } 
    TERM essential[MAXTERMS];
    int ess=0;
    //get the essential prime implicants
    while(TRUE)
    {
               int flag=1;
               for(ii=0;ii<MAXTERMS;ii++)
               {
                    int count=0;
                    int pos=0;
                    for(j=0;j<final_index;j++)
                    if(table[j][ii]==1)
                    {count++;pos=j;}
                    if(count==1)
                    {
                                //this is an essential prime implicant
                                essential[ess]=prime_implicants[pos];
                                ess++;
                                table[j][ii]=0;
                                int c;//remove all other  ones
                                for(c=0;c<MAXTERMS;c++)
                                if(table[pos][c]==1)
                                makezeros(c,final_index,table);//set the col to zero
                                flag=0;
                    }
               }
               if(flag)//no more count=1 i.e no more ess prime im 
               break;
    }
    print_solutions(0,table,final_index,essential,ess);
}

void print_solutions(int ii,int table1[][MAXTERMS],int final_index,TERM essential1[],int ess)
{
     int i,j;
     if(ii==(MAXTERMS-1))
     {
                        //print the ans
                        int i;
                        for(i=0;i<ess;i++)
                        {
                                          printTerm(essential1[i]);
                                          if(i<ess-1)
                                          printf("+");
                        }
                        printf("\n");
     }
     else
     {
         TERM essential[MAXTERMS];
         int ess2=ess;
         int pos[MAXTERMS];
         int row_ones[MAXTERMS];
         int index=0;
         int count=0;
         int table[MAXTERMS][MAXTERMS];
         for(i=0;i<MAXTERMS;i++)
         for(j=0;j<MAXTERMS;j++)
         table[i][j]=table1[i][j];
         //copied a new table
         for(i=0;i<MAXTERMS;i++)
         essential[i]=essential1[i];
         //copied essentials
         for(j=0;j<final_index;j++)
         if(table[j][ii]==1)
         {
               pos[count]=j;
               row_ones[count]=onesInRow(j,table);
               count++;
         }
         if(count==0)//col has no ones
         print_solutions(ii+1,table,final_index,essential,ess);
         else
         {
             int t_max=row_ones[0];
             int unq=0;
             while(row_ones[unq]==t_max)
             {
             	int row=pos[unq];
                //reinitialixe the table
                for(i=0;i<MAXTERMS;i++)
                for(j=0;j<MAXTERMS;j++)
                table[i][j]=table1[i][j];
                //reinitialize essentials
                for(i=0;i<MAXTERMS;i++)
                essential[i]=essential1[i];
                ess2=ess;
                //make all ones in this row zero
                int c;
                for(c=0;c<MAXTERMS;c++)
                if(table[row][c]==1)
                makezeros(c,final_index,table);
                //add to prime implicants
                essential[ess2++]=prime_implicants[row];
                //now send it with increased index
                print_solutions(ii+1,table,final_index,essential,ess2);
                unq++;
             }
         }
     }
}
         
                                  


void sort_both(int row_ones[],int pos[],int count)
{
     int i,j;
     for(i=0;i<count;i++)
     {
         int temp=row_ones[i];
         int p=i;
         for(j=i+1;j<count;j++)
         {
             if(temp<row_ones[j])
             {temp=row_ones[j];
             p=j;
             }
         }//we got the max now swap
         temp=row_ones[i];
         row_ones[i]=row_ones[p];
         row_ones[p]=temp;
         temp=pos[i];
         pos[i]=pos[p];
         pos[p]=temp;
     }
}
     
        
int onesInRow(int row,int table[][MAXTERMS])//will help to find the dominated row
{
     int i,count=0;
     for(i=0;i<MAXTERMS;i++)
     if(table[row][i]==1)
     count++;
     //printf("count");
     return count;
}

void makezeros(int column,int final_index,int table[][MAXTERMS])
{
    int i;
    for(i=0;i<final_index;i++)
    table[i][column]=0;
}
int inDontCares(int n)
{
    int i,flag=0;
    for(i=0;i<numTerms[0];i++)
    if((dont_care[0][i]==TRUE)&&(terms[0][i].t==n))
    flag=1;
    if(flag)
    return TRUE;
    else
    return FALSE;
}

int combinable(TERM tt1,TERM tt2)
{
    if(((tt1.t^tt2.t)==(tt1.f^tt2.f))&& onebit(tt1.t^tt2.t))
    return TRUE;
    else
    return FALSE;
}

int onebit(int num)
{
    int ones=0,i;
    for(i=0;i<MAX_VARS;i++)
    {

        if(num&1)
        ones++;
        num=num>>1;
    }
    if(ones==1)
    return 1;
    else
    return 0;
}

void combine(TERM m1,TERM m2,TERM *m3)
{
    m3->t=m1.t & m2.t;
    m3->f=m1.f & m2.f;
    m3->nn=m1.nn+m2.nn;
    int i=0,ind=0;
    for(i=0;i<m1.nn;i++)
    {
             m3->compterms[ind]=m1.compterms[i];
             ind++;
    }
    for(i=0;i<m2.nn;i++)
    {
             m3->compterms[ind]=m2.compterms[i];
             ind++;
    }                    
}

int EqualTerms(TERM tt1,TERM tt2)
{
    return ((tt1.t==tt2.t)&&(tt1.f==tt2.f));
}

void init(TERM *tr,int n)
{
    tr->t=n;
    tr->f=~n;
    tr->compterms[0]=n;
    tr->nn=1;
}

void printTerm(TERM tt1)
{
    int i;
    char ch='D';
    while((tt1.t>0)||(tt1.f>0))
    {
        int bit1=tt1.t&1;
        int bit2=tt1.f&1;
        if((bit1!=bit2)&&(bit1==0))
        printf("%c'",ch);
        if((bit1!=bit2)&&(bit1==1))
        printf("%c",ch);
        ch--;
        tt1.t=tt1.t>>1;
        tt1.f=tt1.f>>1;
    }
}

