#define _POSIX_C_SOURCE 200112L

// See: https://stackoverflow.com/questions/30235480/returning-a-bare-struct-from-c-to-haskell-in-c2hs

#include <wayland-client.h>
#include "wayland2hs.h"
#include "xdg-shell-client-protocol.h"

const struct wl_interface *getCompositorInterface() {
    return &wl_compositor_interface; }

const struct wl_interface *getShmInterface() {
    return &wl_shm_interface; }

const struct wl_interface *getXdgWmBaseInterface() {
    return &xdg_wm_base_interface; }
