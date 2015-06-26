defmodule PPMSpec do

  use ESpec

  describe ".ppm" do
    it "emits a string with RGB components" do
      expect(PPM.ppm(128, 64, 201)).to eq("128  64 201 ")
    end

    it "pads each components with spaces" do
      expect(PPM.ppm(  0,  0, 255)).to eq("  0   0 255 ")
    end
  end
end
