defmodule Mandelbrot do
  def main(argv) do
    parse = OptionParser.parse(argv)
    case parse do
      { _, [ json_filename, ppm_filename ], _ }
        -> IO.inspect Mandelbrot.Options.parse(File.read!(json_filename))
    end
  end
end
