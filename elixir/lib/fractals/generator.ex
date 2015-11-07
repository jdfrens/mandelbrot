defmodule Fractals.Generator do
  @moduledoc """
  The primary module for generating a fractal.
  """

  import Complex, only: :macros

  @magnitude_cutoff         2.0
  @magnitude_cutoff_squared 4.0
  @max_iterations           256

  def max_iterations, do: @max_iterations

  def do_it(options) do
    File.open(options[:image_filename], [:write],
      fn(image_file) ->
        generate(options[:size], concurrency_function(options))
        |> Stream.each(fn line -> IO.puts(image_file, line) end)
        |> Stream.run
      end)
  end

  def concurrency_function(options) do
    case options[:concurrency] do
      "none"     -> &Fractals.Generator.Taskless.generate/2
      "chunked"  -> &Fractals.Generator.TasklessChunked.generate/2
      "original" -> &Fractals.Generator.OriginalTasked.generate/2
      "crunchy"  -> &Fractals.Generator.LongerTasked.generate/2
      "smooth"   -> &Fractals.Generator.PooledProcesses.generate/2
    end
  end

  def generate(options, concurrency \\ &default_generate/1) do
    Stream.concat(header(options.size), image(options, concurrency))
  end

  def header(%Fractals.Size{width: width, height: height}) do
    PPM.p3_header(width, height)
  end

  def image(options, generator \\ &default_generate/1) do
    generator.(options)
  end

  def default_generate(color_func) do
    Fractals.Generator.LongerTasked.generate(color_func)
  end

  def build_complex(r, i), do: cmplx(r, i)

  def pixels(grid_points, options) do
    grid_points
    |> Enum.map(fn gp ->
      pixel(gp, options.iterator_builder.(gp), options.color_func)
    end)
  end

  def pixel(grid_point, iterator, color_func) do
    iterator
    |> fractal_iterate(&escaped?/1, grid_point)
    |> in_or_out
    |> color(color_func)
  end

  def escaped?(z) do
    Complex.magnitude_squared(z) >= @magnitude_cutoff_squared
  end

  def color(in_out, color_func) do
    color_func.(in_out)
  end

  def in_or_out({z, iterations}) do
    status = if iterations >= 255, do: :inside, else: :outside
    {status, z, iterations}
  end

  def fractal_iterate(iterator, escaped?, grid_point) do
    grid_point
    |> Stream.iterate(iterator)
    |> Stream.with_index
    |> Stream.drop_while(fn {z, i} -> !escaped?.(z) && i < 255 end)
    |> Stream.take(1)
    |> Enum.to_list
    |> List.first
  end
end
