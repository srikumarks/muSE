#include "muse.h"

int main( int argc, char **argv )
{
	muse_env *env = muse_init_env(NULL);
	muse_repl();
	muse_destroy_env(env);
	return 0;
}
