// RTFM-core for RTFM-RT
const char* CORE_FILE_INFO = "Compiled with : RTFM-core options:\ninfile       : test.core\noutfile      : test.c\nasync_err    : false\ntarget       : RTFM_RT\nbackend      : GCC\nverbose      : false\ndebug        : false\ngv_task      : false\ngv_taskf     : \ngv_res       : false\ngv_resf      : \nd_ast        : false\n";

enum resources {RES_NR};
int ceilings[] = {};
const char* res_names[] = {};
enum entry_nr {user_reset_nr, user_idle_nr, ENTRY_NR};
int entry_prio[] = {0, 0};
char* entry_names[] = {"user_reset", "user_idle"};


ENTRY_FUNC entry_func[] = {user_reset, user_idle};

char_p reset_test_0(int RTFM_id); // function prototype

void user_reset(int RTFM_id) {
char * a =
reset_test_0(RTFM_id);
}
char_p reset_test_0(int RTFM_id){
return "hej"
}
void user_idle(int RTFM_id) {
}

