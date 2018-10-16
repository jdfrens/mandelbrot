# Fractals

[![Build
Status](https://semaphoreci.com/api/v1/jdfrens/mandelbrot/branches/master/badge.svg)](https://semaphoreci.com/jdfrens/mandelbrot)
[![CircleCI](https://circleci.com/gh/jdfrens/mandelbrot.svg?style=svg)](https://circleci.com/gh/jdfrens/mandelbrot)

Generates escape-time fractals.

I'm blogging about this project at [Programming During Recess](http://www.programming-during-recess.net/).


## Blog Articles

* [I reboot my Elixir app](http://www.programming-during-recess.net/2016/05/29/fractals-in-elixir-rebooted/)
* [I process my output](http://www.programming-during-recess.net/2016/06/05/output-process-for-elixir-fractals/), tag: [`blog_2016_05_04`](https://github.com/jdfrens/mandelbrot/tree/blog_2016_05_04/elixir)
* [I plan for my processes](http://www.programming-during-recess.net/2016/06/12/processes-for-elixir-fractals/), tag: [`blog_2016_06_12`](https://github.com/jdfrens/mandelbrot/tree/blog_2016_06_12/elixir)
* [I produce a minimal viable Mandelbrot](http://www.programming-during-recess.net/2016/06/19/minimal-viable-mandelbrot/), tag: [`blog_2016_06_19`](https://github.com/jdfrens/mandelbrot/tree/blog_2016_06_19/elixir)
* [I implement color schemes](http://www.programming-during-recess.net/2016/06/26/color-schemes-for-mandelbrot-sets/), tag: [`blog_2016_06_26`](https://github.com/jdfrens/mandelbrot/tree/blog_2016_06_26/elixir)
* [I generate Julia set and burning ships](http://www.programming-during-recess.net/2016/07/03/mandelbrots-julias-and-burning-ships/), tag: [`blog_2016_07_03`](https://github.com/jdfrens/mandelbrot/tree/blog_2016_07_03/elixir)
* [I parse my params better](http://www.programming-during-recess.net/2016/07/17/better-params-parsing/), tag: [`blog_2016_07_17`](https://github.com/jdfrens/mandelbrot/tree/blog_2016_07_17/elixir)

## Installation

```
$ git clone https://github.com/jdfrens/mandelbrot.git
$ cd mandlebrot/elixir
```

Checkout a tag to along with my blog post for that week:

```
$ git checkout tags/blog_2016_06_12 -b whatever_you_want
```

You can run the tests and compile the executable:

```elixir
$ mix deps.get
$ mix test
$ mix credo --strict
$ mix escript.build
```

Some earlier branches might have a `spec` task instead of `test`.  `credo --strict` may fail on older branches.

You may want compile the executable with `MIX_ENV=prod`.

## Generating Fractals

If you generate [PPM file](https://en.wikipedia.org/wiki/Netpbm_format#PPM_example) files, you're all done.  If you want PNG files, you need to install [ImageMagick](http://www.imagemagick.org/).  The Elixir program will automatically call the `convert` program to convert from PPM to PNG.

```
$ fractals ../yaml/burningship-line-blue.yml
# bunch of output, then:
finished images/burningship-line-blue.png
ALL DONE!
Have a nice day.
$ open images/burningship-line-blue.png
```
