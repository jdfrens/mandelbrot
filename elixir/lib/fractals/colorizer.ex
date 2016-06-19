defmodule Fractals.Colorizer do
  # NOTE: this implements the very simple black-on-white coloring
  # TODO: write other colorizers and figure out a good naming scheme

  def color_point({_, iterations}, _) when iterations >= 256 do
    PPM.black
  end
  def color_point(_, _), do: PPM.white
end
