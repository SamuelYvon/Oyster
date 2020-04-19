# Oyster

#### Version Alpha 0.0.0

> An extensible Scheme library to execute Scheme as a Shell script


I hate writing shell scripts, but I have too. However, I love writing Scheme. It's 2020, why do I have to suffer this much. Oyster is a Scheme library that allows me to write Scheme scripts that can call programs found in the path. 


The idea is fairly simple; instead of using a normal `define`, I use `define-shell` which will parse the code and find commands that would be programs. For instance,

```Scheme

(define-shell (foo bar)
 (map string->symbol (ls "~")))

```

Would become


```Scheme

(define (foo bar)
 (map string->symbol (shuck 'ls "~")))

```

Where the `shuck` function would understand the command `ls` and provide a result that would be made out of a list of string, each one being a directory / file in my home folder.

# Design Goals

# Getting Started

# Configuration

# Roadmap
