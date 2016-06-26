defmodule Fractals.ColorizerSpec do
  use ESpec, async: true

  alias Fractals.Colorizer
  alias Fractals.Options

  let :z, do: nil

  describe ".color_point" do
    context "for black-on-white" do
      let :options, do: %Options{color: :black_on_white}

      it "colors outside" do
        expect(Colorizer.color_point({z, 512}, options)) |> to(eq PPM.black)
      end

      it "colors inside" do
        expect(Colorizer.color_point({z, 128}, options)) |> to(eq PPM.white)
      end
    end

    context "for white-on-black" do
      let :options, do: %Options{color: :white_on_black}

      it "colors outside" do
        expect(Colorizer.color_point({z, 512}, options)) |> to(eq PPM.white)
      end

      it "colors inside" do
        expect(Colorizer.color_point({z, 128}, options)) |> to(eq PPM.black)
      end
    end

    context "for gray" do
      let :options, do: %Options{color: :gray}

      it "is black inside" do
        expect(Colorizer.color_point({z, 512}, options)) |> to(eq PPM.black)
      end
      it "scales 0 to black" do
        expect(Colorizer.color_point({z,   0}, options)) |> to(eq PPM.black)
      end
      it "scales 128 to 181" do
        expect(Colorizer.color_point({z, 128}, options))
        |> to(eq PPM.ppm(181, 181, 181))
      end
      it "is white after maximum iterations" do
        expect(Colorizer.color_point({z, 255}, options)) |> to(eq PPM.white)
      end
    end
  end
end
