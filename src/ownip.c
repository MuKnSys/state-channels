#include <stdio.h>      
#include <sys/types.h>
#include <ifaddrs.h>
#include <netinet/in.h> 
#include <string.h> 
#include <arpa/inet.h>
#include <netpacket/packet.h>

// to confirm, do 'ip a|grep global|grep 641'
int main (int argc, const char * argv[]) {
  struct ifaddrs * ifAddrStruct=NULL;
  struct ifaddrs * ifa=NULL;
  void * tmpAddrPtr=NULL;
  int PRINT=0;
  char *NAME=NULL;
  char *IP=NULL;
  char *IP6=NULL;
  char *MAC=NULL;

  getifaddrs(&ifAddrStruct);
  for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
    if (!ifa->ifa_addr) continue;
    if (ifa->ifa_addr->sa_family == AF_INET) { // IP4
      tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
      char addressBuffer[INET_ADDRSTRLEN];
      inet_ntop(AF_INET, tmpAddrPtr, addressBuffer, INET_ADDRSTRLEN);
      if (PRINT) {
        NAME=strdup(ifa->ifa_name);
        break;
      }
      if (!strcmp(ifa->ifa_name,"lo")) PRINT=1;
    }
  }
  if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);

  getifaddrs(&ifAddrStruct);
  for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
    if (!ifa->ifa_addr) {
      continue;
    }
    if (ifa->ifa_addr->sa_family == AF_PACKET) { // MAC
      char macp[INET6_ADDRSTRLEN];
      struct sockaddr_ll *s = (struct sockaddr_ll*)(ifa->ifa_addr);
      int i;
      int len = 0;
      for (i = 0; i < 6; i++) {
        len += sprintf(macp+len, "%02X%s", s->sll_addr[i], i < 5 ? ":":"");
      }
      if (!strcmp(ifa->ifa_name,NAME)) MAC=strdup(macp);
    }
    else
    if (ifa->ifa_addr->sa_family == AF_INET) { // IP4
      tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
      char addressBuffer[INET_ADDRSTRLEN];
      inet_ntop(AF_INET, tmpAddrPtr, addressBuffer, INET_ADDRSTRLEN);
      if (!strcmp(ifa->ifa_name,NAME)) IP=strdup(addressBuffer);
    }
    else
    if (ifa->ifa_addr->sa_family == AF_INET6) { // IP6
      tmpAddrPtr=&((struct sockaddr_in6 *)ifa->ifa_addr)->sin6_addr;
      char addressBuffer[INET6_ADDRSTRLEN];
      inet_ntop(AF_INET6, tmpAddrPtr, addressBuffer, INET6_ADDRSTRLEN);
      if (!strcmp(ifa->ifa_name,NAME)) IP6=strdup(addressBuffer);
    } 
  }
  if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);

//if (NAME!=NULL) printf("NAME=%s\n",NAME);
  if (IP!=NULL) printf("%s",IP);
//if (IP6!=NULL) printf("IP6=%s\n",IP6);
  if (MAC!=NULL) printf(" %s",MAC);
  printf("\n");

  return 0;
}
