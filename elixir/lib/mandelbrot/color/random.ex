defmodule Mandelbrot.Color.Random do
  def build_random do
    colors = Enum.map(0..255, random_color)
    fn
      { :inside, _, _ } -> PPM.black
      { :outside, _, iterations } -> Enum.at(colors, iterations)
    end
  end

  def random_color do
    fn _ -> PPM.ppm(:random.uniform(255), :random.uniform(255), :random.uniform(255)) end
  end
end
