defmodule Fractals.IntegrationTest do
  use ExUnit.Case

  @moduletag :skip

  # IDEA: generate two fractals

  @mandelbrot_input_filename "test/inputs/integration_mandelbrot.yml"
  @mandelbrot_output_filename "test/images/integration_mandelbrot.ppm"

  @expected_output [
                     "P3",
                     "2",
                     "2",
                     "255",
                     "255 255 255 ",
                     "255 255 255 ",
                     "  0   0   0 ",
                     "255 255 255 ",
                     ""
                   ]
                   |> Enum.join("\n")

  setup do
    if File.exists?(@mandelbrot_output_filename) do
      File.rm!(@mandelbrot_output_filename)
    end

    :ok
  end

  test "generates a pretty picture" do
    params =
      Fractals.Params.process(
        output_directory: "test/images",
        params_filename: @mandelbrot_input_filename,
        source_pid: self()
      )

    ExUnit.CaptureIO.capture_io(fn ->
      # SMELL: this seemss awkward
      Fractals.fractalize(params)
      Fractals.CLI.watch([@mandelbrot_input_filename])
    end)

    output = File.read!(@mandelbrot_output_filename)
    assert output == @expected_output
  end
end
