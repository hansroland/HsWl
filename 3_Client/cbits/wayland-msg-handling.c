/*
 * Copyright © 2008 Kristian Høgsberg
 * Copyright © 2013 Jason Ekstrand
 * Copyright © 2014 Intel Corporation
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that copyright
 * notice and this permission notice appear in supporting documentation, and
 * that the name of the copyright holders not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  The copyright holders make no representations
 * about the suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
 * OF THIS SOFTWARE.
 */

#include <stdio.h>

#include "wayland-msg-handling.h"

#define MAX_FDS 28 /* this constant is from Wayland library */

static int decode_cmsg(int *fds, int bufsize, struct msghdr *msg)
{
    struct cmsghdr *cmsg;
    size_t size;
    int n = 0;
    int *fdp = fds;

    cmsg = CMSG_FIRSTHDR(msg);

    while (cmsg) {
        int n_fds_in_cmsg = 0;

        if (cmsg->cmsg_level != SOL_SOCKET || cmsg->cmsg_type != SCM_RIGHTS) {
            cmsg = CMSG_NXTHDR(msg, cmsg);
            continue;
        }

        size = cmsg->cmsg_len - CMSG_LEN(0);

        n_fds_in_cmsg = size / sizeof(int32_t);

        if (bufsize < size) {
            /* TODO: close the fds */
            return -1;
        }

        memcpy(fdp, CMSG_DATA(cmsg), size);
        fdp += n_fds_in_cmsg;
        n += n_fds_in_cmsg;
        bufsize -= size;

        cmsg = CMSG_NXTHDR(msg, cmsg);
    }

    return n;
}

int recvmsg_wayland(int fd, const char *buf, int bufsize, int *fds,
        int fdbufsize, int *n_fds)
{
    char cmsg_buf[CMSG_LEN(MAX_FDS*4)];
    struct iovec iov[1];
    struct msghdr msg;
    int len;

    iov[0].iov_base = (void *) buf;
    iov[0].iov_len = bufsize;

    msg.msg_name = NULL;
    msg.msg_namelen = 0;
    msg.msg_iov = iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf;
    msg.msg_controllen = sizeof(cmsg_buf);
    msg.msg_flags = 0;

    do {
        len = recvmsg(fd, &msg, MSG_CMSG_CLOEXEC);
    } while (len == -1 && errno == EINTR);

    if (len >= 0)
        *n_fds = decode_cmsg(fds, fdbufsize, &msg);
    else {
        printf("recvmsg error: %m!\n");
        *n_fds = 0;
    }

    return len;
}

void hexprint(char title[], unsigned char bytearr[], int arrlen)
{
    printf ("%s\n", title);
    for (size_t i = 0; i < arrlen; i++) {
        printf("%.2x", bytearr[i]);
        if ((i+1)%4 == 0) {printf(" ");}
        if ((i+1)%16 == 0) {printf(" ");}
        if ((i+1)%32 == 0) {printf("\n");}
    }
    printf ("\n");
}

/*
struct msghdr {
    void            *msg_name;      // optional address
    socklen_t       msg_namelen;    // size of address
    struct          iovec *msg_iov; // scatter/gather array
    int             msg_iovlen;     // # elements in msg_iov
    void            *msg_control;   // ancillary data, see below
    socklen_t       msg_controllen; // ancillary data buffer len
    int             msg_flags;      // flags on received message
};

struct cmsghdr {
    socklen_t cmsg_len;    // data byte count, including header
    int       cmsg_level;  //riginating protocol
    int       cmsg_type;   // protocol-specific type
    // followed by
    unsigned char cmsg_data[];};

*/
