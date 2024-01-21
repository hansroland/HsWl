# HsWl Haskell Wayland Client

This is a proof of concept: Write a simple Wayland client in Haskell
without calling the libwayland client library via FFI. The code implements directly the Wayland wire protocol.

We have the following subdirectories:

## 0_Book

This is the original code of the [Wayland Book](https://github.com/rcalixte/wayland-book/blob/master/src/SUMMARY.md). At the time of writing, the [original version](https://wayland-book.com/) was no longer accessible.

#### Status

Currently this directory contains the code upto the chapter [example-code](https://github.com/rcalixte/wayland-book/blob/master/src/xdg-shell-basics/example-code.md)

#### TODO

Add additional requests from the folllowing chapters

## 1_FFI

This directory contains a more or less simple translation of the code in the `Book` directory from C to Haskell. I think the result is rather clumsy. Therefore I won't add additional code.

#### Status

At the moment, the code up to the extended example has been translated to Haskell. However the checkerboard background has not been implemented.


## 2_Generator

This directory contains a generator application to generate Haskell functions for the Wayland request and event-handling functions from the defining xml files.

#### Status

The generator is in alpha state. It generates al the functions from the wayland.xml and xdg-shell.xml files.

Additional changes will be needed during the development of the client. The `enums` are still missing.

## 3_Client

This directory contains a extremly simple Haskell client for Wayland. This is currently the main focus of the development.

## Ressources

The used *.xml files:

* [wayland.xml](https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/protocol/wayland.xml)
* [xdg-shell.xml](https://cgit.freedesktop.org/wayland/wayland-protocols/tree/stable/xdg-shell/xdg-shell.xml)