defmodule Fractals.ColorizerSpec do
  use ESpec, async: true

  alias Fractals.Colorizer
  alias Fractals.Params

  let :z, do: nil

  describe ".color_point" do
    context "for black-on-white" do
      let :params, do: %{Params.default | color: :black_on_white}

      it "colors outside" do
        expect(Colorizer.color_point({z, 512}, params)) |> to(eq PPM.black)
      end

      it "colors inside" do
        expect(Colorizer.color_point({z, 128}, params)) |> to(eq PPM.white)
      end
    end

    context "for white-on-black" do
      let :params, do: %{Params.default | color: :white_on_black}

      it "colors outside" do
        expect(Colorizer.color_point({z, 512}, params)) |> to(eq PPM.white)
      end

      it "colors inside" do
        expect(Colorizer.color_point({z, 128}, params)) |> to(eq PPM.black)
      end
    end

    context "for gray" do
      let :params, do: %{Params.default | color: :gray}

      it "is black inside" do
        expect(Colorizer.color_point({z, 512}, params)) |> to(eq PPM.black)
      end
      it "scales 0 to black" do
        expect(Colorizer.color_point({z,   0}, params)) |> to(eq PPM.black)
      end
      it "scales 128 to 180" do
        expect(Colorizer.color_point({z, 128}, params))
        |> to(eq PPM.ppm(180, 180, 180))
      end
      it "is white after maximum iterations" do
        expect(Colorizer.color_point({z, 255}, params)) |> to(eq PPM.white)
      end
    end

    context "for Warp POV" do
      it "computes red" do
        expect(Colorizer.color_point({z, 128}, %{Params.default | color: :red}))
        |> to(eq PPM.ppm(255, 0, 0))
      end
      it "computes green" do
        expect(Colorizer.color_point({z, 128}, %{Params.default | color: :green}))
        |> to(eq PPM.ppm(0, 255, 0))
      end
      it "computes blue" do
        expect(Colorizer.color_point({z, 128}, %{Params.default | color: :blue}))
        |> to(eq PPM.ppm(0, 0, 255))
      end
    end
  end
end
