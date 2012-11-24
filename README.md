# Volt - A Variant of Bolt OCaml Logging Tool

## Overview

This package provides a variant of [Bolt OCaml logging tool](http//bolt.x9c.fr)
called Volt. Volt offers the following features in addition to the original:

+ **Pass filter concept.**
  Each logger has an associated pass filter, which ensures that events
  will never be propagated to the ancestor loggers when the events do
  not satisfy the filter.

+ **Extended implicit logging syntax.**  
  log_expr ::= ...  
   | FATAL_MSG (string|ident) arguments attributes  
   | ERROR_MSG (string|ident) arguments attributes  
   | WARN_MSG (string|ident) arguments attributes  
   | INFO_MSG (string|ident) arguments attributes  
   | DEBUG_MSG (string|ident) arguments attributes  
   | TRACE_MSG (string|ident) arguments attributes  
  block_expr ::=  
   | BEGIN_FATAL expr_seq END_FATAL  
   | BEGIN_ERROR expr_seq END_ERROR  
   | BEGIN_WARN expr_seq END_WARN  
   | BEGIN_INFO expr_seq END_INFO  
   | BEGIN_DEBUG expr_seq END_DEBUG  
   | BEGIN_TRACE expr_seq END_TRACE

+ **More informative default logger name.**
  In addition to a capitalized source file name without suffix,
  surrounding module names, class names, or function names are
  used to compute a logger name when no NAME attribute is provided.

+ **Suppression of unwanted evaluation of arguments.**
  Arguments in the LOG and *_MSG expressions are not evaluated when the defined
  logger does not record events.

+ **Additional keys for use by the pattern and csv layouts.**  
  monthname - name of month e.g. January, February, ...  
  monthnm - abbreviated name of month e.g. Jan, Feb, ...

## Sources

The development sources are available from [GitHub](https://github.com/codinuum/volt/).

## License

As in the original version, this tool is free software released under the LGPL v3.

## Installation

See INSTALL.

## Original

The original Bolt is Copyright (C) 2009-2011 Xavier Clerc and released under the LGPL v3.
The official website of Bolt is [here](http://bolt.x9c.fr).

----------
Copyright &copy; 2012 [Codinuum Software Lab](http://codinuum.com/)