defmodule Mandelbrot.Color.SimpleTest do

  use Pavlov.Case, async: true
  import Pavlov.Syntax.Expect

  import Mandelbrot.Color.Simple

  describe ".black_on_white" do
    it "is black on the inside" do
      expect black_on_white({  :inside, :meh, :meh })
        |> to_eq PPM.black
    end
    it "is white on the outside" do
      expect black_on_white({ :outside, :meh, :meh })
        |> to_eq PPM.white
    end
  end

  describe ".white_on_black" do
    it "is white on the inside" do
      expect white_on_black({  :inside, :meh, :meh })
        |> to_eq PPM.white
    end
    it "is black on the outside" do
      expect white_on_black({ :outside, :meh, :meh })
        |> to_eq PPM.black
    end
  end

  describe ".gray" do
    it "is black inside" do
      expect gray({ :inside, :meh, :meh }) |> to_eq PPM.black
    end
    it "scales 0 to 0" do
      expect gray({ :outside, :meh, 0 }) |> to_eq PPM.ppm(0, 0, 0)
    end
    it "scales 128 to 128" do
      expect gray({ :outside, :meh, 128 }) |> to_eq PPM.ppm(128, 128, 128)
    end
    it "is white after maximum iterations" do
      expect gray({ :outside, :meh,  256 })
       |> to_eq PPM.white
    end
  end

end
