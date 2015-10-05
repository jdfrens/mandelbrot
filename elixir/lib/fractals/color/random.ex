defmodule Fractals.Color.Random do
  @moduledoc """
  Creates an array of 256 random colors.
  """

  def build_random do
    colors = Enum.map(0..255, random_color)
    fn
      { :inside, _, _ } -> PPM.black
      { :outside, _, iterations } -> Enum.at(colors, iterations)
    end
  end

  def random_color do
    fn _ -> PPM.ppm(random_hue, random_hue, random_hue) end
  end

  def random_hue do
    :random.uniform(255)
  end
end
