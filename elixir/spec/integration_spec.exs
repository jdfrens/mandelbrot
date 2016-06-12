defmodule Fractals.IntegrationSpec do
  use ESpec

  @input_filename  "spec/integration_input.json"
  @output_filename "spec/integration_output.ppm"

  @expected_output [
    "P3",
    "2",
    "3",
    "255",
    "  0   0   0 ",
    "  0   0   0 ",
    "  0   0   0 ",
    "  0   0   0 ",
    "  0   0   0 ",
    "  0   0   0 ",
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
    options = Fractals.Options.parse([], @input_filename, @output_filename)
    ExUnit.CaptureIO.capture_io(fn ->
      Fractals.CLI.main_helper(options)
    end)
    output = File.read!(@output_filename)
    expect(output) |> to(eq @expected_output)
  end
end
