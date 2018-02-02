defmodule PPMSpec do
  use ExUnit.Case, async: true

  describe ".p3_header" do
    test "the header rows" do
      assert PPM.p3_header(726, 28) == ["P3", "726", "28", "255"]
    end
  end

  describe ".ppm" do
    test "emits a string with RGB components" do
      assert PPM.ppm(128, 64, 201) == "128  64 201 "
    end

    test "pads each components with spaces" do
      assert PPM.ppm(0, 0, 255) == "  0   0 255 "
    end
  end
end
