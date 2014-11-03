#>
#include <string.h>
#include <stdlib.h>


char * cat(char * rigthS, char * LeftS){
	char * ret;
	ret = malloc(sizeof(rigthS)*(strlen(rigthS)+strlen(LeftS)+1));
	strcpy(ret,rigthS);
	strcat(ret,LeftS);
	return ret;
}

int greterThan(int a, int b){
	return a>b;
}

int isEqual(int a, int b){
	return a==b;
}

int ilen(int a){
	int len = 0;
	do{
		len++;
		a = a/10
	}while(a)
	return len;
}

//stolen from http://stackoverflow.com/questions/9655202/how-to-convert-integer-to-string-in-c
char* itoa(int i, char b[]){
    char const digit[] = "0123456789";
    char* p = b;
    if(i<0){
        *p++ = '-';
        i *= -1;
    }
    int shifter = i;
    do{ //Move to where representation ends
        ++p;
        shifter = shifter/10;
    }while(shifter);
    *p = '\0';
    do{ //Move back, inserting digits as u go
        *--p = digit[i%10];
        i = i/10;
    }while(i);
    return b;
}

<#