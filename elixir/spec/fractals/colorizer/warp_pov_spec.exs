defmodule Fractals.Colorizer.WarpPovSpec do
  use ESpec, async: true

  alias Fractals.Colorizer.WarpPov

  describe ".red" do
    it "treats the primary hue as red" do
      expect(WarpPov.red(127)).to eq(PPM.ppm(251, 0, 0))
    end
  end

  describe ".green" do
    it "treats the primary hue as green" do
      expect(WarpPov.green(127)).to eq(PPM.ppm(0, 251, 0))
    end
  end

  describe ".blue" do
    it "treats the primary hue as blue" do
      expect(WarpPov.blue(127)).to eq(PPM.ppm(0, 0, 251))
    end
  end

  describe ".intensities" do
    it "is black inside" do
      expect(WarpPov.intensities(256)).to eq({0, 0})
    end

    context "for the plateau (when secondary is 0)" do
      it "is all 0 for 0 iterations" do
        expect(WarpPov.intensities(0)).to eq({0, 0})
      end
      it "is all 0 for 1 iteration" do
        expect(WarpPov.intensities(1)).to eq({0, 0})
      end
      it "is primary 2 for 2 iteration" do
        expect(WarpPov.intensities(2)).to eq({2, 0})
      end
      it "is primary 251 for 127 (half minus one) iterations" do
        expect(WarpPov.intensities(127)).to eq({251, 0})
      end
    end

    context "on the border (when primary at maximum)" do
      it "is secondary 0 for 128 (half max) iterations" do
        expect(WarpPov.intensities(128)).to eq({255, 0})
      end
      it "is 253 secondary for 255 iterations" do
        expect(WarpPov.intensities(255)).to eq({255, 253})
      end
    end
  end

  describe ".scale" do
    it "scales 1 iteration to 0" do
      expect(WarpPov.scale(1)).to eq(0)
    end
    it "scales 64 iterations to 251" do
      expect(WarpPov.scale(127)).to eq(251)
    end
    it "scales half of max iterations to max intensity" do
      expect(WarpPov.scale(128)).to eq(253)
    end
  end
end
