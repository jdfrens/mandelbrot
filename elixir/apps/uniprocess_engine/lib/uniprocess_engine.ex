defmodule UniprocessEngine do
  @moduledoc """
  Documentation for UniprocessEngine.
  """

  alias Fractals.{Colorizer, EscapeTime, ImageMagick, Grid, Params, Reporters.Broadcaster}
  alias Fractals.Output.PPMFile

  @spec generate(Fractals.Params.t()) :: :ok
  def generate(params) do
    {params, nil}
    |> grid()
    |> pixels()
    |> colors()
    |> write()
    |> convert()
    |> done()
  end

  def grid({params, nil}) do
    {params, Grid.grid(params)}
  end

  def pixels({params, grid}) do
    {params, EscapeTime.pixels(params.fractal, grid, params)}
  end

  def colors({params, pixels}) do
    {params, Stream.map(pixels, &Colorizer.color_point(&1, params))}
  end

  def write({params, colors}) do
    PPMFile.write_file(params, colors)
    Params.close(params)
    {params, nil}
  end

  def convert({params, nil}) do
    params.output_filename
    |> Path.extname()
    |> convert_to(params)

    {params, nil}
  end

  def done({params, nil}) do
    Broadcaster.report(:done, params, from: self())
    {params, nil}
  end

  defp convert_to(".png", params) do
    root_filename =
      params.output_filename
      |> Path.rootname(".png")
      |> Path.rootname(".ppm")

    ppm_filename = root_filename <> ".ppm"
    ImageMagick.convert(ppm_filename, params.output_filename)

    params
  end
end
