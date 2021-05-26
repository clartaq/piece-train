# piece-train
A backing buffer for the text in an editor implemented as a page table in Scheme.

## Overview

One of the most fundamental parts of a text editor is the text buffer used to store and access the actual text being edited. There has been much research on this topic. This project implements one of the best -- a piece table.

This project makes a very general purpose API available for manipulating the text in the buffer.

## Motivation

Lisps in general are execellent languages for implementing complex programs, yet, with the exception of emacs, there are not many well-known or much-used examples where a Lisp is used to implement a text editor.

There are many reasons for this. One of the major reasons, in my opinion, is poor access to low-level hardware devices like the keyboard.

Despite that difficulty, I wanted to take a crack at writing a simple editor in Scheme. **piece-train** is the first componenet of that editor. 

## The Name

This project was inspired by the tutorial on [Piece Chains](http://www.catch22.net/tuts/neatpad/piece-chains#) (a piece table that keeps the pieces in a list) by James Brown as part of his [larger tutorial](http://www.catch22.net/tuts/neatpad) on implementing a text editor for Win32.

The name is just a little play on words, using the rhyming title of the 1971 song Peace Train by Cat Stevens. It's the first thing that went through my head when I heard "piece chain".

## Development

The library is written in Chez Scheme 9.5.4 on macOS 11.4.

## Testing

A program to test the buffer is provided in the `piece-train-test.ss` file. The bash script `run-tests.sh` provides an easy way to execute the tests.

The test program requires access to SRFI 64 for use as the testing framework. The ways to access the SRFI vary, but in my project layout, SRFIs obtained from thunderchez are present in another directory. You can point Chez Scheme at it by setting an environment variable. With the way my system is set up, I use:

```
export CHEZSCHEMELIBDIRS="./:/Users/David/projects/scheme/thunderchez:"
```

in my shell profile (`zsh` in my case). You will need to do something similar.

## Other Resources

There is abundant reference material available on text buffers, including piece-tables. Some of the best includes:
