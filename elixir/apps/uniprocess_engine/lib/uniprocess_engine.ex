defmodule UniprocessEngine do
  @moduledoc """
  Documentation for UniprocessEngine.
  """

  alias Fractals.{
    Colorizer,
    EscapeTime,
    Grid,
    ImageMagick,
    Output.PPMFile,
    Params,
    Reporters.Broadcaster
  }

  @spec generate(Fractals.Params.t()) :: :ok
  def generate(params) do
    {params, nil}
    |> grid()
    |> pixels()
    |> colors()
    |> write()
    |> convert()
    |> done()

    :ok
  end

  @spec grid({Params.t(), nil}) :: {Params.t(), Grid.t()}
  def grid({params, nil}) do
    {params, Grid.grid(params)}
  end

  @spec pixels({Params.t(), Grid.t()}) :: {Params.t(), EscapeTime.t()}
  def pixels({params, grid}) do
    {params, EscapeTime.pixels(params.fractal, grid, params)}
  end

  @spec colors({Params.t(), EscapeTime.t()}) :: {Params.t(), [PPM.color()]}
  def colors({params, pixels}) do
    {params, Enum.map(pixels, &Colorizer.color_point(&1, params))}
  end

  @spec write({Params.t(), [PPM.color()]}) :: {Params.t(), nil}
  def write({params, colors}) do
    PPMFile.write_file(params, colors)
    Params.close(params)
    {params, nil}
  end

  @spec convert({Params.t(), nil}) :: {Params.t(), nil}
  def convert({params, nil}) do
    params.output_filename
    |> Path.extname()
    |> convert_to(params)

    {params, nil}
  end

  @spec done({Params.t(), nil}) :: {Params.t(), nil}
  def done({params, nil}) do
    Broadcaster.report(:done, params, from: self())
    {params, nil}
  end

  @spec convert_to(String.t(), Params.t()) :: Params.t()
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
