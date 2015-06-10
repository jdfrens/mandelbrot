defmodule PPM.Test do

  use Pavlov.Case, async: true
  import Pavlov.Syntax.Expect

  describe ".ppm" do
    it "spits out a strictly formatted string" do
      expect PPM.ppm(  0,  0, 255) |> to_eq "  0   0 255 "
      expect PPM.ppm(128, 64, 201) |> to_eq "128  64 201 "
    end
  end
end
