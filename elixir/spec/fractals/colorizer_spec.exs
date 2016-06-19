defmodule Fractals.ColorizerSpec do
  use ESpec, async: true

  alias Fractals.Colorizer

  let :z, do: nil
  let :options, do: nil

  describe ".color_point" do
    it "colors outside" do
      expect(Colorizer.color_point({z, 512}, options)) |> to(eq PPM.black)
    end

    it "colors inside" do
      expect(Colorizer.color_point({z, 128}, options)) |> to(eq PPM.white)
    end
  end
end
