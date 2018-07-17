defmodule Fractals.ColorizerSpec do
  use ExUnit.Case, async: true

  alias Fractals.Colorizer
  alias Fractals.Params

  describe "black on white" do
    test "colors outside" do
      params = %{Params.default() | color: :black_on_white}
      assert Colorizer.color_point({nil, 512}, params) == PPM.black()
    end

    test "colors inside" do
      params = %{Params.default() | color: :black_on_white}
      assert Colorizer.color_point({nil, 128}, params) == PPM.white()
    end
  end

  describe "white on black" do
    test "colors outside" do
      params = %{Params.default() | color: :white_on_black}
      assert Colorizer.color_point({nil, 512}, params) == PPM.white()
    end

    test "colors inside" do
      params = %{Params.default() | color: :white_on_black}
      assert Colorizer.color_point({nil, 128}, params) == PPM.black()
    end
  end

  describe "gray" do
    test "black inside" do
      params = %{Params.default() | color: :gray}
      assert Colorizer.color_point({nil, 512}, params) == PPM.black()
    end

    test "scales 0 to black" do
      params = %{Params.default() | color: :gray}
      assert Colorizer.color_point({nil, 0}, params) == PPM.black()
    end

    test "scales 128 to 180" do
      params = %{Params.default() | color: :gray}
      assert Colorizer.color_point({nil, 128}, params) == PPM.ppm(180, 180, 180)
    end

    test "white after maximum iterations" do
      params = %{Params.default() | color: :gray}
      assert Colorizer.color_point({nil, 255}, params) == PPM.white()
    end
  end

  describe "Warp POV" do
    test "red" do
      params = %{Params.default() | color: :red}
      assert Colorizer.color_point({nil, 128}, params) == PPM.ppm(255, 0, 0)
    end

    test "green" do
      params = %{Params.default() | color: :green}
      assert Colorizer.color_point({nil, 128}, params) == PPM.ppm(0, 255, 0)
    end

    test "blue" do
      params = %{Params.default() | color: :blue}
      assert Colorizer.color_point({nil, 128}, params) == PPM.ppm(0, 0, 255)
    end
  end
end
