/*
 * Determine whether or not a Conf represents a session which can
 * sensibly be launched right now.
 */

#include "putty.h"

bool conf_launchable(Conf *conf)
{
#ifndef TGDLL
    if (conf_get_int(conf, CONF_protocol) == PROT_SERIAL)
        return conf_get_str(conf, CONF_serline)[0] != 0;
    else
#endif
        return conf_get_str(conf, CONF_host)[0] != 0;
}
