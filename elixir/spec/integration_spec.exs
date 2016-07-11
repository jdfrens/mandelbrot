defmodule Fractals.IntegrationSpec do
  use ESpec

  @input_filename  "spec/integration_input.json"
  @output_filename "spec/integration_output.ppm"

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

  before do
    if File.exists?(@output_filename) do
      File.rm!(@output_filename)
    end
    Application.start(:fractals)
  end

  finally do
    Application.stop(:fractals)
  end

  it "generates a pretty picture" do
    params = Fractals.Params.parse([
      params_filename: @input_filename,
      output_filename: @output_filename,
      next_pid: self
    ])
    ExUnit.CaptureIO.capture_io(fn ->
      Fractals.CLI.main_helper(params)
    end)
    output = File.read!(@output_filename)
    expect(output) |> to(eq @expected_output)
  end
end
