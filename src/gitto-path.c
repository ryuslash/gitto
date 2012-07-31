#include <stdlib.h>
#include <libguile.h>

SCM
realpath_wrapper(SCM str)
{
    char *path = scm_to_locale_string(str);
    char *resolved_path = realpath(path, NULL);
    SCM scm_resolved_path = scm_from_locale_string(resolved_path);

    free(path);
    free(resolved_path);

    return scm_resolved_path;
}

void
init_gitto()
{
    scm_c_define_gsubr("realpath", 1, 0, 0, realpath_wrapper);
}
