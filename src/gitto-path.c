/* gitto -- Keep track of your git repositories
   Copyright (C) 2012 Tom Willemsen <tom at ryuslash dot org>

   This file is part of gitto.

   gitto is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   gitto is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with gitto.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <stdlib.h>
#include <libguile.h>

SCM_DEFINE(realpath_wrapper, "realpath", 1, 0, 0,
           (SCM path),
           "Transform PATH into an absolute path.")
{
    char *relative_path = scm_to_locale_string(path);
    char *resolved_path = realpath(relative_path, NULL);
    SCM scm_resolved_path = SCM_BOOL_F;

    if (resolved_path)
        scm_resolved_path = scm_from_locale_string(resolved_path);

    free(relative_path);
    free(resolved_path);

    return scm_resolved_path;
}

void
init_gitto()
{
#include "gitto-path.x"
}
