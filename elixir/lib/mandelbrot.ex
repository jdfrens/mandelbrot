defmodule Mandelbrot do
  def main([filename]) do
    IO.inspect Mandelbrot.Options.parse(File.read!(filename))
  end
end
