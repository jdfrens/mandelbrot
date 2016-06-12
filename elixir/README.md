# Fractals

Generates escape-time fractals.

I'm blogging about this project at [Programming During Recess](http://www.programming-during-recess.net/).

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
