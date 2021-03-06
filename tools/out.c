// RTFM-core for RTFM-RT
const char* CORE_FILE_INFO = "Compiled with : RTFM-core options:\ninfile       : out.core\noutfile      : out.c\nasync_err    : false\ntarget       : RTFM_RT\nbackend      : GCC\nverbose      : false\ndebug        : false\ngv_task      : false\ngv_taskf     : \ngv_res       : false\ngv_resf      : \nd_ast        : false\n";

enum resources {sendLock,RES_NR};
int ceilings[] = {2};
const char* res_names[] = {"sendLock"};
enum entry_nr {user_reset_nr, user_idle_nr, idle_server_sendData_0_nr, idle_server_sendData_0_server_sendData_0_nr, idle_idle_websocket_0_client_receive_0_nr, ENTRY_NR};
int entry_prio[] = {0, 0, 2, 2, 1};
char* entry_names[] = {"user_reset", "user_idle", "idle_server_sendData_0", "idle_server_sendData_0_server_sendData_0", "idle_idle_websocket_0_client_receive_0"};

// Task instance definition: @prio 2 idle_server_sendData_0
void idle_server_sendData_0(int RTFM_id); // function prototype for the instance task
void entry_idle_server_sendData_0(int RTFM_id); // function prototype for the instance task

// Task instance definition: @prio 2 idle_server_sendData_0_server_sendData_0
void idle_server_sendData_0_server_sendData_0(int RTFM_id); // function prototype for the instance task
void entry_idle_server_sendData_0_server_sendData_0(int RTFM_id); // function prototype for the instance task

// Task instance definition: @prio 1 idle_idle_websocket_0_client_receive_0
void idle_idle_websocket_0_client_receive_0(int RTFM_id, char* msg); // function prototype for the instance task
void entry_idle_idle_websocket_0_client_receive_0(int RTFM_id); // function prototype for the instance task

ENTRY_FUNC entry_func[] = {user_reset, user_idle, entry_idle_server_sendData_0, entry_idle_server_sendData_0_server_sendData_0, entry_idle_idle_websocket_0_client_receive_0};

typedef struct {;} ARG_idle_server_sendData_0; // type definition for arguments
ARG_idle_server_sendData_0 arg_idle_server_sendData_0; // instance for argument
void entry_idle_server_sendData_0(int RTFM_id) {
	idle_server_sendData_0(RTFM_id); // (inlined) call to the async function
}
int idle_server_sendData_0_sendLock_server_addJson_0(int RTFM_id, char* key, char* value); // function prototype
int idle_server_sendData_0_sendLock_server_compileJson_0(int RTFM_id); // function prototype
void idle_server_sendData_0_sendLock_ws_send_0(int RTFM_id, char* message); // function prototype
typedef struct {;} ARG_idle_server_sendData_0_server_sendData_0; // type definition for arguments
ARG_idle_server_sendData_0_server_sendData_0 arg_idle_server_sendData_0_server_sendData_0; // instance for argument
void entry_idle_server_sendData_0_server_sendData_0(int RTFM_id) {
	idle_server_sendData_0_server_sendData_0(RTFM_id); // (inlined) call to the async function
}
void idle_idle_websocket_0(int RTFM_id); // function prototype
typedef struct {char* msg;} ARG_idle_idle_websocket_0_client_receive_0; // type definition for arguments
ARG_idle_idle_websocket_0_client_receive_0 arg_idle_idle_websocket_0_client_receive_0; // instance for argument
void entry_idle_idle_websocket_0_client_receive_0(int RTFM_id) {
	idle_idle_websocket_0_client_receive_0(RTFM_id, arg_idle_idle_websocket_0_client_receive_0.msg); // (inlined) call to the async function
}
int idle_idle_websocket_0_client_receive_0_server_reset1_0(int RTFM_id); // function prototype


#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <openssl/sha.h>
#include <openssl/bio.h>
#include <openssl/evp.h>
#include <math.h>
#include "fmemopen.h"

#ifdef TRACE_WS
#define DPS(fmt, ...) {fprintf(stderr, "\tWS:<%f> "fmt"\n", RT_time_to_float(time_get()), ##__VA_ARGS__);}
#else
#define DPS(...) 
#endif

typedef char* char_p;

// Encodes a string to base64
int Base64Encode(const char* message, int len, char** buffer) { 
  BIO *bio, *b64;
  FILE* stream;
  int encodedSize = 4 * ceil((double) len / 3);
  *buffer = (char *) malloc(encodedSize + 1);

  stream = fmemopen(*buffer, encodedSize + 1, "w");
  b64 = BIO_new(BIO_f_base64());
  bio = BIO_new_fp(stream, BIO_NOCLOSE);
  bio = BIO_push(b64, bio);
  BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL); // write everything in one line
  BIO_write(bio, message, len);

  BIO_flush(bio);
  BIO_free_all(bio);
  fclose(stream);

  return (0); //success
}

int connfd = 0;
void error(char* err) {
  fprintf(stderr, "%s", err);
  exit(0);
}

char* resp1 = "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n";
char* resp2 = "Sec-WebSocket-Accept: ";
char* resp3 = "Sec-WebSocket-Protocol: lost-protocol\r\n\r\n"; // with an extra blank line
char* magic = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

char sendBuff[1025];


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
		a = a/10;
	}while(a);
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


struct struct_server {
int endTime;
char* sendBuff;
int sendTime;
int currentTime;
};
struct struct_server server;
void init_server() {
server.endTime = 1000;
server.sendBuff = malloc(12);
server.sendTime = 1000;
server.currentTime = 0;
}

struct struct_client {
};
struct struct_client client;
void init_client() {
}

void user_reset(int RTFM_id) {
init_server();
init_client();
}
void user_idle(int RTFM_id) {
arg_idle_server_sendData_0 = (ARG_idle_server_sendData_0){}; 
RTFM_pend(500000, 4611686018426887903, RTFM_id, idle_server_sendData_0_nr);
idle_idle_websocket_0(RTFM_id);
}
void idle_server_sendData_0(int RTFM_id){ // function implementation for the task:idle_server_sendData_0[idle_server_sendData_0]
RTFM_lock(RTFM_id, sendLock);
char* timeString;
timeString = malloc(8* ilen(server.currentTime / 1000));
itoa(server.endTime - server.currentTime / 1000, timeString );
idle_server_sendData_0_sendLock_server_addJson_0(RTFM_id, "time", timeString);
idle_server_sendData_0_sendLock_server_compileJson_0(RTFM_id);
idle_server_sendData_0_sendLock_ws_send_0(RTFM_id, server.sendBuff);
server.currentTime = server.sendTime + server.currentTime ;
memset(server.sendBuff , 0, strlen(server.sendBuff ));
RTFM_unlock(RTFM_id, sendLock);
arg_idle_server_sendData_0_server_sendData_0 = (ARG_idle_server_sendData_0_server_sendData_0){}; 
RTFM_pend(1000000, 4611686018425887903, RTFM_id, idle_server_sendData_0_server_sendData_0_nr);
}
int idle_server_sendData_0_sendLock_server_addJson_0(int RTFM_id, char* key, char* value){
char* temp1;
char* temp2;
char* temp3;
char* temp4;
char* temp;
temp1 = "";
if (isEqual(0, strlen(server.sendBuff ))) {
temp1 = cat("\"", key );
} else {
temp1 = cat(",\"", key );
}
temp2 = cat(temp1 , "\":\"");
temp3 = cat(temp2 , value );
temp4 = "";
if (isEqual(0, strlen(server.sendBuff ))) {
temp4 = cat(temp3 , "\"");
} else {
temp4 = cat(temp3 , "\",");
}
free(temp1 );
free(temp2 );
free(temp3 );
temp = cat(server.sendBuff , temp4 );
server.sendBuff = temp ;
free(temp4 );
return 1;
}
int idle_server_sendData_0_sendLock_server_compileJson_0(int RTFM_id){
char* temp1;
char* temp2;
temp1 = cat("{", server.sendBuff );
temp2 = cat(temp1 , "}");
server.sendBuff = temp2 ;
free(temp1 );
return 1;
}
void idle_server_sendData_0_sendLock_ws_send_0(int RTFM_id, char* message){
if (connfd == 0)
    return;

  unsigned char *out_decoded = (unsigned char *) &sendBuff[2]; //6 with mask
  sprintf((char *) out_decoded, "%s", message);

  int len = strlen(message);
  if (len > 126)
    error("we do not support split messages\n");
  sendBuff[0] = 0x80 | 0x1; // FIN + text_msg opcode
  sendBuff[1] = 0x00 | len; // no mask
  send(connfd, sendBuff, len + 2, 0);
}
void idle_server_sendData_0_server_sendData_0(int RTFM_id){ // function implementation for the task:idle_server_sendData_0_server_sendData_0[idle_server_sendData_0]
RTFM_lock(RTFM_id, sendLock);
char* timeString;
timeString = malloc(8* ilen(server.currentTime / 1000));
itoa(server.endTime - server.currentTime / 1000, timeString );
idle_server_sendData_0_sendLock_server_addJson_0(RTFM_id, "time", timeString);
idle_server_sendData_0_sendLock_server_compileJson_0(RTFM_id);
idle_server_sendData_0_sendLock_ws_send_0(RTFM_id, server.sendBuff);
server.currentTime = server.sendTime + server.currentTime ;
memset(server.sendBuff , 0, strlen(server.sendBuff ));
RTFM_unlock(RTFM_id, sendLock);
arg_idle_server_sendData_0_server_sendData_0 = (ARG_idle_server_sendData_0_server_sendData_0){}; 
RTFM_pend(1000000, 4611686018425887903, RTFM_id, idle_server_sendData_0_server_sendData_0_nr);
}
void idle_idle_websocket_0(int RTFM_id){
int listenfd = 0;
  struct sockaddr_in serv_addr;

  char readBuff[1025], *reqline[3];

  listenfd = socket(AF_INET, SOCK_STREAM, 0);
  memset(&serv_addr, (char ) '\0', sizeof(serv_addr));
  memset(sendBuff, (char ) '\0', sizeof(sendBuff));
  memset(readBuff, (char ) '\0', sizeof(readBuff));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
  serv_addr.sin_port = htons(5000);

  bind(listenfd, (struct sockaddr*) &serv_addr, sizeof(serv_addr));
  listen(listenfd, 1);
  while (1) {
    DPS("before accept\n");
    connfd = accept(listenfd, (struct sockaddr*) NULL, NULL);
    DPS("after accept\n");
    int n = recv(connfd, readBuff, sizeof(readBuff), 0);
    if (n < 0)
      error("ERROR reading from socket");

    DPS("%d, %s\n", n, readBuff);reqline[0] = strtok(readBuff, " \t\n");
    if (strncmp(reqline[0], "GET\0", 4) == 0) {
      reqline[1] = strtok(NULL, " \t");
      reqline[2] = strtok(NULL, " \t\n");
      if (strncmp(reqline[2], "HTTP/1.1", 8) != 0) {
        write(connfd, "HTTP/1.0 400 Bad Request\n", 25);
        error("bad request");
      } 
      DPS("OK request\n");
      while (1) {
        reqline[0] = strtok(NULL, " \n\r");
        char ws_key[] = "Sec-WebSocket-Key:";
        DPS("%s\n", reqline[0]);if (strncmp(reqline[0], ws_key, sizeof(ws_key)) == 0) {
          reqline[1] = strtok(NULL, " \r\n");
          DPS("KEY = %s\n", reqline[1]);break;
        }
      }
      DPS("--- response header ---\n");
      send(connfd, resp1, strlen(resp1), 0);// the response header
      DPS("%s", resp1);

      // compute key
      char key_in[256];
      char* key_out;

      sprintf(key_in, "%s%s", reqline[1], magic); // append the magic
      DPS("key_in  %d : %s", (int) strlen(key_in), key_in);

      unsigned char hash[SHA_DIGEST_LENGTH];
      SHA1((const unsigned char *)key_in, strlen(key_in), hash); // SHA1 hashing
      DPS("hash, %d, %s", (int) sizeof(hash), hash);

      Base64Encode((const char *)hash, sizeof(hash), &key_out); // Encode as Base64
      DPS("key_out %d, %s", (int)strlen(key_out), key_out);

      sprintf(sendBuff, "%s%s\r\n", resp2, key_out);
      send(connfd, sendBuff, strlen(sendBuff), 0); // the unique session key
      DPS("%s", sendBuff);

      send(connfd, resp3, strlen(resp3), 0); // the protocol name
      DPS("%s", resp3);

      while (1) {
        DPS("--- recv ---");
        n = recv(connfd, readBuff, sizeof(readBuff), 0);
        if (n < 0)
          error("ERROR reading from socket");

        DPS("readBuff[0] %x ", (0xFF & readBuff[0]));
        unsigned char msg_fin = readBuff[0] & 0x80; // logic (bitwise) and

        if (msg_fin == 0)
          error ("message split, not implemented");

        DPS("fin OK");
        unsigned char msg_op = readBuff[0] & 0xF; // opcode 4 bits

        if (msg_op == 0x8) {
          DPS("disconnect by server");
          break;
        }

        if (msg_op != 0x1)
          error ("non-text message, not implemented");
        DPS("Text msg OK");

        unsigned char msg_size = readBuff[1] & 0x7F; // length
        if (msg_size >= 126)
          error ("multi byte length, no implemented");
        DPS("Size OK %d", msg_size);

        unsigned char *decoded = (unsigned char *) &readBuff[6];
        unsigned char *encoded = (unsigned char *) &readBuff[6];
        unsigned char *mask = (unsigned char *) &readBuff[2]; // point to the mask bits

        for (int i = 0; i < msg_size; i++) 
          decoded[i] = (encoded[i] ^ mask[i % 4]);

        decoded[msg_size] = 0; // terminate the string
        DPS("Text msg %s", decoded);
arg_idle_idle_websocket_0_client_receive_0 = (ARG_idle_idle_websocket_0_client_receive_0){(char*)decoded}; 
RTFM_pend(0, 4611686018427387903, RTFM_id, idle_idle_websocket_0_client_receive_0_nr);
}
      DPS("trying to reconnect");
      close(connfd);
    }
  }
  // never happens	
  close(listenfd);
}
void idle_idle_websocket_0_client_receive_0(int RTFM_id, char* msg){ // function implementation for the task:idle_idle_websocket_0_client_receive_0[idle_idle_websocket_0_client_receive_0]
if (isEqual(strcmp(msg , "vinner"), 0)) {
idle_idle_websocket_0_client_receive_0_server_reset1_0(RTFM_id);
} else {
}
}
int idle_idle_websocket_0_client_receive_0_server_reset1_0(int RTFM_id){
server.endTime = 1000;
free(server.sendBuff );
server.sendTime = 1000;
server.currentTime = 0;
}

