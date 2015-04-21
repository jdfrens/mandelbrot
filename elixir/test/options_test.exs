defmodule Mandelbrot.Options.Test do

  use ExUnit.Case, async: true

  import Mandelbrot.Options, only: [ parse: 1 ]

  test "parse" do
    json = """
    { "fractal": "Mandelbrot" }
    """
    assert parse(json) == %Mandelbrot.Options{ fractal: :mandelbrot }
  end

end
