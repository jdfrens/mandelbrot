# Fractals

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
$ mix spec
$ mix escript.build
```

You can run the executable, although you'll probably want to convert the [PPM file](https://en.wikipedia.org/wiki/Netpbm_format#PPM_example) into a more common image format with [ImageMagick](http://www.imagemagick.org/).

```
$ fractals ../json/burningship-line-blue.json blb.ppm
$ convert blb.ppm blb.png   # from ImageMagick
$ open blb.png
```
