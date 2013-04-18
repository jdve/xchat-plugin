#include "xchat-plugin-hack.h"

/* The xchat_emit_print function is a vararg.
   It was also the case for xchat_printf and xchat_commandf,
   but they were not necessary.
   While browsing the code, it occured that this version
   of XChat doesn't use more than 4 args
   (it is explicitely used in the implementation of xchat_emit_print),
   so to ease my Haskell binding, I provide the 5 versions.
*/

int xchat_emit_print0 (
	xchat_plugin *ph,
	const char *event_name
) { return (xchat_emit_print(ph,event_name,NULL)); }
int xchat_emit_print1 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0
) { return (xchat_emit_print(ph,event_name,arg0,NULL)); }
int xchat_emit_print2 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0,
    const char *arg1
) { return (xchat_emit_print(ph,event_name,arg0,arg1,NULL)); }
int xchat_emit_print3 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0,
    const char *arg1,
    const char *arg2
) { return (xchat_emit_print(ph,event_name,arg0,arg1,arg2,NULL)); }
int xchat_emit_print4 (
	xchat_plugin *ph,
	const char *event_name,
    const char *arg0,
    const char *arg1,
    const char *arg2,
    const char *arg3
) { return (xchat_emit_print(ph,event_name,arg0,arg1,arg2,arg3,NULL)); }

/* There is a cast in the interface to get a context from a pointer.
   To keep well typed, we do this cast explicitely here.
*/
const xchat_context * xchat_list_context (
	xchat_plugin *ph,
	xchat_list *xlist,
	const char *name
) { return (const xchat_context *) xchat_list_str(ph, xlist, name); }

char* strListChan     = "channels"     ;
char* strListDcc      = "dcc"          ;
char* strListIgn      = "ignore"       ;
char* strListNotify   = "notify"       ;
char* strListUser     = "users"        ;
char* strAddress      = "address32"    ;
char* strCps          = "cps"          ;
char* strDestfile     = "destfile"     ;
char* strFile         = "file"         ;
char* strNick         = "nick"         ;
char* strPort         = "port"         ;
char* strPos          = "pos"          ;
char* strResume       = "iresume"      ;
char* strSize         = "isize"        ;
char* strSizehigh     = "isizehigh"    ;
char* strStatus       = "istatus"      ;
char* strChannel      = "channel"      ;
char* strChantypes    = "chantypes"    ;
char* strContext      = "context"      ;
char* strFlags        = "flags"        ;
char* strId           = "id"           ;
char* strLag          = "lag"          ;
char* strMaxmodes     = "maxmodes"     ;
char* strNetwork      = "network"      ;
char* strNickprefixes = "nickprefixes" ;
char* strNickmodes    = "nickmodes"    ;
char* strQueue        = "queue"        ;
char* strServer       = "server"       ;
char* strType         = "type"         ;
char* strUsers        = "users"        ;
char* strMask         = "mask"         ;
char* strAway         = "away"         ;
char* strLasttalk     = "lasttalk"     ;
char* strHost         = "host"         ;
char* strPrefix       = "prefix"       ;
char* strRealname     = "realname"     ;
char* strSelected     = "seleceted"    ;
char* strNetworks     = "networks"     ;
char* strOn           = "on"           ;
char* strOff          = "off"          ;
char* strSeen         = "seen"         ;
