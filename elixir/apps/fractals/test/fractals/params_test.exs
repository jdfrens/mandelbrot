defmodule Fractals.ParamsTest do
  use ExUnit.Case, async: true

  alias Fractals.{Params, Size}

  describe ".process a full set of params" do
    setup do
      [argv: [params_filename: "test/inputs/full_params.yml"]]
    end

    test "parsing the fractal type", %{argv: argv} do
      assert Params.process(argv).fractal == :mandelbrot
    end

    test "parsing the image size", %{argv: argv} do
      assert Params.process(argv).size == %Size{width: 720, height: 480}
    end

    test "parsing the color scheme", %{argv: argv} do
      assert Params.process(argv).color == :blue
    end

    test "parsing the random seed", %{argv: argv} do
      assert Params.process(argv).seed == 12_345
    end

    test "parsing the upper-left corner", %{argv: argv} do
      assert Params.process(argv).upper_left == Complex.new(0.0, 55.2)
    end

    test "parsing the lower-right corder", %{argv: argv} do
      assert Params.process(argv).lower_right == Complex.new(92.3, 120.3)
    end

    test "parsing the c parameter", %{argv: argv} do
      assert Params.process(argv).c == Complex.new(3.14, 4.13)
    end

    test "parsing the z parameter", %{argv: argv} do
      assert Params.process(argv).z == Complex.new(4.4, 1.1)
    end

    test "parsing the r parameter", %{argv: argv} do
      assert Params.process(argv).r == Complex.new(9.9, 3.3)
    end

    test "parsing the p parameter", %{argv: argv} do
      assert Params.process(argv).p == Complex.new(0.3, 0.5)
    end

    test "precomputing the output_filename paramter", %{argv: argv} do
      assert Params.process(argv).output_filename == "test/images/full_params.png"
    end

    test "precomputing the ppm_filename paramter", %{argv: argv} do
      assert Params.process(argv).ppm_filename == "test/images/full_params.ppm"
    end
  end

  describe ".process and relying on defaults" do
    setup do
      [argv: []]
    end

    test "defaults to Mandelbrot", %{argv: argv} do
      assert Params.process(argv).fractal == :mandelbrot
    end

    test "defaults the image size", %{argv: argv} do
      assert Params.process(argv).size == %Size{width: 512, height: 384}
    end

    test "defaults the color scheme", %{argv: argv} do
      assert Params.process(argv).color == :black_on_white
    end

    test "defaults the random seed", %{argv: argv} do
      assert Params.process(argv).seed == 666
    end

    test "still parsing the upper-left corner", %{argv: argv} do
      assert Params.process(argv).upper_left == Complex.new(5.0, 6.0)
    end

    test "still parsing the lower-right corder", %{argv: argv} do
      assert Params.process(argv).lower_right == Complex.new(6.0, 5.0)
    end

    test "defaults the c parameter", %{argv: argv} do
      assert Params.process(argv).c == Complex.new(1.0, 0.0)
    end

    test "defaults the z parameter", %{argv: argv} do
      assert Params.process(argv).z == Complex.new(0.0, 0.0)
    end

    test "defaults the r parameter", %{argv: argv} do
      assert Params.process(argv).r == Complex.new(0.0, 0.0)
    end

    test "defaults the p parameter", %{argv: argv} do
      assert Params.process(argv).p == Complex.new(0.0, 0.0)
    end

    test "defaults the chunk size", %{argv: argv} do
      assert Params.process(argv).chunk_size == 1000
    end

    test "outputs to images directory", %{argv: argv} do
      assert Params.process(argv).output_directory == "images"
    end
  end

  describe "parsing flags and input file" do
    setup do
      argv = [
        c: "99.0+0.0i",
        params_filename: "test/inputs/partial_params.yml",
        fractal: "Burningship"
      ]

      [argv: argv]
    end

    test "recognizes the early flag", %{argv: argv} do
      assert Params.process(argv).c == Complex.new(99.0)
    end

    test "recognizes a value from the file", %{argv: argv} do
      assert Params.process(argv).color == :blue
    end

    test "recognizes a value overridden by a flag", %{argv: argv} do
      assert Params.process(argv).fractal == :burningship
    end
  end

  describe "computed chunk count" do
    test "divides evenly" do
      assert Params.process(size: "10x2", chunk_size: 5).chunk_count == 4
    end

    test "adds one for a remainder" do
      assert Params.process(size: "10x2", chunk_size: 3).chunk_count == 7
    end

    test "always computes and overrides explicit setting" do
      assert Params.process(size: "10x2", chunk_size: 3, chunk_count: 99_999).chunk_count == 7
    end
  end
end
