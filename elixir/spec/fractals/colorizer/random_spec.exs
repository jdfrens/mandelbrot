defmodule Fractals.Colorizer.RandomSpec do
  use ESpec

  alias Fractals.Colorizer.Random

  before do: Random.start_link(:ok)
  finally do: Process.unregister(Random)

  describe ".at" do
    it "returns black when inside" do
      expect(Random.at(Random, 256)) |> to(eq PPM.black)
    end

    it "returns a color when outside" do
      expect(Random.at(Random, 127)) |> to(match ~r/\s*\d{1,3}\s+\d{1,3}\s+\d{1,3}/)
    end

    it "returns the same color for the same iterations" do
      color = Random.at(Random, 52)
      expect(Random.at(Random, 52)) |> to(eq color)
      expect(Random.at(Random, 52)) |> to(eq color)
      expect(Random.at(Random, 52)) |> to(eq color)
    end
  end
end
