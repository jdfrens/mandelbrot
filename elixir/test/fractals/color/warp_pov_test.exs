defmodule Fractals.Color.WarpPovTest do

  use Pavlov.Case, async: true
  import Pavlov.Syntax.Expect

  import Fractals.Color.WarpPov
  import PPM, only: [ ppm: 3 ]

  describe ".red" do
    it "treats the primary hue as red" do
      expect red({ :outside, :meh, 127 }) |> to_eq ppm(251, 0, 0)
    end
  end

  describe ".green" do
    it "treats the primary hue as green" do
      expect green({ :outside, :meh, 127 }) |> to_eq ppm(0, 251, 0)
    end
  end

  describe ".blue" do
    it "treats the primary hue as blue" do
      expect blue({ :outside, :meh, 127 }) |> to_eq ppm(0, 0, 251)
    end
  end

  describe ".hues" do
    it "is black inside" do
      expect hues({:inside, :meh, :meh}) |> to_eq { 0, 0 }
    end

    context "for the plateau (when secondary is 0)" do
      it "is all 0 for 0 iterations" do
        expect hues({:outside, :meh, 0}) |> to_eq { 0, 0 }
      end
      it "is all 0 for 1 iteration" do
        expect hues({:outside, :meh, 1}) |> to_eq { 0, 0 }
      end
      it "is primary 2 for 2 iteration" do
        expect hues({:outside, :meh, 2}) |> to_eq { 2, 0 }
      end
      it "is primary 251 for 127 (half minus one) iterations" do
        expect hues({:outside, :meh, 127}) |> to_eq { 251, 0 }
      end
    end

    context "on the border (when primary at maximum)" do
      it "is secondary 0 for 128 (half max) iterations" do
        expect hues({:outside, :meh, 128}) |> to_eq { 255, 0 }
      end
      it "is 253 secondary for 255 iterations" do
        expect hues({:outside, :meh, 255}) |> to_eq { 255, 253 }
      end
      it "is secondary maximum for 256 (max) iterations" do
        expect hues({:outside, :meh, 256}) |> to_eq { 255, 255 }
      end
    end
  end

  describe ".scale" do
    it "scales 1 iteration to 0" do
      expect scale(1) |> to_eq 0
    end
    it "scales 64 iterations to 251" do
      expect scale(127) |> to_eq 251
    end
    it "scales half of max iterations to max intensity" do
      expect scale(128) |> to_eq 253
    end
  end
end
