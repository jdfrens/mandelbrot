defmodule Fractals.IntegrationTest do
  use ExUnit.Case

  @input_filename  "test/integration_input.yml"
  @output_filename "test/integration_output.ppm"

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
    ] |> Enum.join("\n")

  setup do
    if File.exists?(@output_filename) do
      File.rm!(@output_filename)
    end
    Application.start(:fractals)

    on_exit fn ->
      Application.stop(:fractals)
    end

    :ok
  end

  test "generates a pretty picture" do
    params = Fractals.Params.parse([
      params_filename: @input_filename,
      output_filename: @output_filename,
      next_pid: self
    ])
    ExUnit.CaptureIO.capture_io(fn ->
      Fractals.CLI.main_helper(params)
    end)
    output = File.read!(@output_filename)
    assert output == @expected_output
  end
end
