# Oyster

> An extensible Scheme library to execute Scheme as a Shell script

#### Version Alpha 0.0.6

__ATTENTION__ Since this is in Alpha, the API might change from version to version!

I hate writing shell scripts, but I have too. However, I love writing Scheme. It's 2020 and I should not have to suffer this much. Oyster is a Scheme library that allows one to write Scheme scripts that can call programs found in the path.

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

Where the `shuck` function would understand the command `ls` and provide a result that would be made out of a list of string, each one being a directory / file in my home folder (by calling `ls`).

This library allows you to avoid the horrible shell syntax and still have a natural access to the programs provided by your operating system.

For instance, if you wanted to perform `ls` on all the files in your home folder, you could do

```Scheme
(import (oyster))

(map ls (directory-files "~"))
```

Of course, you might want to pass more arguments to ls, which you can do using the `partial` method provided:

```Scheme
(import (oyster))

(map (partial ls "-al") (directory-files "~"))
```

## Usage

Oyster is a Scheme library for Gambit. Therefore, you will be able to use it by creating Scheme files that will be run from the `gsi` (or `gsc`) programs.

Those scripts will require you to include the library:

```Scheme
#!/usr/local/Gambit/bin/gsi
(import (oyster))

; Your script here!
```


### `define-shell`

The majority of what you need should be covered by the `define-shell` and `->>` macros. The `define-shell` macro allows you to define Scheme functions, where the identifiers that represents programs found in your path will be transformed into calls to the programs themselves. For instance,

```Scheme
#!/usr/local/Gambit/bin/gsi
(import (oyster))

(define-shell (example)
 (ls "-a"))

(example)
```
Will produce something akin to (if called from ~)

```Scheme
("total 26"
"drwxr-xr-x  4 syvon syvon 4096 Apr 20 10:44 Desktop"
"drwxr-xr-x 24 syvon syvon 4096 Apr 19 13:26 Documents"
"drwxr-xr-x  2 syvon syvon 4096 Apr 19 21:47 Downloads"
"drwxr-xr-x  2 syvon syvon 4096 Feb 25 21:55 Music"
"drwxr-xr-x  2 syvon syvon 4096 Apr 18 18:51 Pictures"
"drwxr-xr-x  2 syvon syvon 4096 Feb 25 21:55 Public"
"drwxr-xr-x  2 syvon syvon 4096 Feb 25 21:55 Templates"
"drwxr-xr-x  3 syvon syvon 4096 Apr 10 15:59 Videos")
```

### `->>`

The `->>` function, also called the force-feed function, will pipe commands together, and parse the result of the last command in the pipe chain. For instance,

```Scheme
(import (oyster))
(->> (ls "-al")
     (wc "-l"))
```

Will return the number of lines from the result of the "ls" call. Pipes are done sequentially. You do not need to execute the force-feed function from a `define-shell` function.

### `with-shell`

The `with-shell` function executes a block of code with shell expansion without using the `define-shell` command. You can use it for inline shell commands or a list of forms. It uses the same mecanisms but without creating a definition.

`(with-shell thunks)`

### `with-sudo`

`with-sudo` asks for super user access. It takes a list of thunks to be executed, with the result of the expression being the last statement. This functions does not enter
a "shell" mode and so you will need to use either inside a `define-shell` or use commands inside a `with-shell`.

### Shucking-knifes

Analysing the result of a command and transforming it into a Scheme result is called `shucking`. You can define `shuking-knifes`, functions responsible for parsing the result, in your `~/oyster.scm` config file. By default, shucking is done with the `butter-knife`, which only transform lines into an array of strings.

One can define a specific shucking knife  for a function (for `ls in this case) this way:

```Scheme

   (define (knife command args result)
           (do-stuff))

   (define-shuck-knife 'ls knife)
```

The `knife` function receives the command symbol, the args the function was called with and the result string. The result of the knife is used directly as the return argument of the call.


### Aliases

You can set aliases for function to add parameters by default and create other function names. To do this, you can either have an alias setup in your config or in the file you're running.

```Scheme
(define (alias what to . args) ...)
```

`what` and `to` are symbols that map `what -> to`. Args is a variadic argument that takes in the list of arguments you want to add to the function call (may be empty). All calls to `what` result in a call to `to` with the args added. It will append `args` to the called arguments.

### Supplying arguments

Multiple ways of providing arguments are supported. If you use a symbol as an argument, it will be printed verbatim inside the command line generated. For instance `(with-shell (ls '-al))` yields `ls -al`. If you pass a string, for instance `(with-shell (ls "-al"))` the generated command will be `ls "-al"`, with quotation around the argument. This may not be desirable for all commands. For flags, you can use the flag macro which will correctly generate a flag set. For instance, `(with-shell (ls (flags a l))) = (with-shell (ls (flags al))) = (with-shell (ls (flags "al"))) = ...`.

## Getting Started

Oyster is designed for [Gambit Scheme](https://github.com/gambit/gambit). Therefore, using Oyster
requires that you have Gambit installed and properly configured. Once you have Gambit installed somewhere (by default, `/usr/local/Gambit`), you can install the oyster library in your Gambit library folder by using the `install.sh` script:

```Shell
git clone https://github.com/SamuelYvon/Oyster
cd Oyster
chmod u+x install.sh
./install.sh
```

If you decide to leave the default install library folder, it will install Oyster in Gambit's bundled libraries. You will need to run the `install.sh` script as root (`sudo ./install.sh`).

## Configuration

The configuration file is loaded from `~/.oyster.scm`. A default configuration is provided in `default-config.scm`.

## Roadmap
