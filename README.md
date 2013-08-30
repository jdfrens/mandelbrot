# Functional, Concurrent, Parallel Computation of Fractals

I'm a computer hacker.  I love fractals.  I love computing fractals.

I'm a functional programmer.  I love concurrency.  I love parallelism (and wrote a dissertation on it).

I'm a polyglot.  I love Haskell.  I love Scheme.  I want to love Erlang (but I have to get to know it first).

## Haskell code

The Haskell code (in the `Haskell` directory) was started in the Spring of 2008 for a programming languages course I was teaching at the time.  Check out the [github repository for that class](https://github.com/jdfrens/cs214) for the ancient history of that code.

I've worked on this code more recently to push on the parallelism so that I can compare its performance to my Erlang code.

## Erlang code

I'm interested in how concurrent I can make my Haskell algorithm, and Erlang is supposed to be the go-to language for concurrency.

## Versions

### 2013-08-30

* Erlang code is horribly out of date and hasn't been touched in years.
* Haskell code...
    * Reworked code to read YAML configurations.
	* Reorganized the code.
	* GOT PARALLELISM WORKING---this turned out to be picking the right strategy and parallizing at the right level.

### 2011-02-02

* Both the Haskell and Erlang code read configurations from the same YAML files.
* Output seems to reasonably the same.
* Concurrency is for crap, so far.
