defmodule Fractals.Colorizer.WarpPovTest do
  use ExUnit.Case, async: true

  alias Fractals.Colorizer.WarpPov
  alias Fractals.Params

  setup do
    [params: Params.default()]
  end

  describe ".red" do
    test "treats the primary hue as red", %{params: params} do
      assert WarpPov.red(127, params) == PPM.ppm(251, 0, 0)
    end
  end

  describe ".green" do
    test "treats the primary hue as green", %{params: params} do
      assert WarpPov.green(127, params) == PPM.ppm(0, 251, 0)
    end
  end

  describe ".blue" do
    test "treats the primary hue as blue", %{params: params} do
      assert WarpPov.blue(127, params) == PPM.ppm(0, 0, 251)
    end
  end

  describe ".intensities" do
    test "is black inside", %{params: params} do
      assert WarpPov.intensities(256, params) == {0, 0}
    end

    test "is all 0 for 0 iterations", %{params: params} do
      assert WarpPov.intensities(0, params) == {0, 0}
    end

    test "is all 0 for 1 iteration", %{params: params} do
      assert WarpPov.intensities(1, params) == {0, 0}
    end

    test "is primary 2 for 2 iteration", %{params: params} do
      assert WarpPov.intensities(2, params) == {2, 0}
    end

    test "is primary 251 for 127 (half minus one) iterations", %{params: params} do
      assert WarpPov.intensities(127, params) == {251, 0}
    end

    test "is secondary 0 for 128 (half max) iterations", %{params: params} do
      assert WarpPov.intensities(128, params) == {255, 0}
    end

    test "is 253 secondary for 255 iterations", %{params: params} do
      assert WarpPov.intensities(255, params) == {255, 253}
    end
  end

  describe ".scale" do
    test "scales 1 iteration to 0", %{params: params} do
      assert WarpPov.scale(1, params) == 0
    end

    test "scales 64 iterations to 251", %{params: params} do
      assert WarpPov.scale(127, params) == 251
    end

    test "scales half of max iterations to max intensity", %{params: params} do
      assert WarpPov.scale(128, params) == 253
    end
  end
end
