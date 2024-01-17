// wayland2hs little interface module for the Haskell <-> Wayland bridge

#ifndef CALLERBACK_H
#define CALLERBACK_H

#include <wayland-client.h>

const struct wl_interface *getCompositorInterface();

const struct wl_interface *getShmInterface();

const struct wl_interface *getXdgWmBaseInterface();

#endif