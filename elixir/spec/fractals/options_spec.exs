defmodule Fractals.OptionsSpec do
  use ESpec

  import Fractals.Options, only: [ build_from_json: 2 ]
  import Complex, only: :macros

  describe ".build_from_json" do
    let :options, do: %Fractals.Options{}

    context "when specifying all options" do
      let :full_json do
        """
        {
          "fractal":    "Mandelbrot",
          "size":       "720x480",
          "color":      "Blue",
          "seed":       12345,
          "upperLeft":  "0.0+55.2i",
          "lowerRight": "92.3+120.3i",
          "c":          "3.14+4.13i",
          "z":          "4.4+1.1i",
          "r":          "9.9+3.3i",
          "p":          "0.3+0.5i"
        }
        """
        |> Poison.Parser.parse!
      end

      it "parses the fractal type" do
        expect(build_from_json(options, full_json).fractal).to eq(:mandelbrot)
      end
      it "parses the image size" do
        expect(build_from_json(options, full_json).size)
        .to eq(%Fractals.Size{width: 720, height: 480})
      end
      it "parses the color scheme" do
        expect(build_from_json(options, full_json).color).to eq(:blue)
      end
      it "parses the random seed" do
        expect(build_from_json(options, full_json).seed).to eq(12345)
      end
      it "parses the upper-left corner" do
        expect(build_from_json(options, full_json).upper_left).to eq(cmplx(0.0, 55.2))
      end
      it "parses the lower-right corder" do
        expect(build_from_json(options, full_json).lower_right).to eq(cmplx(92.3, 120.3))
      end
      it "parses the c parameter" do
        expect(build_from_json(options, full_json).c).to eq(cmplx(3.14, 4.13))
      end
      it "parses the z parameter" do
        expect(build_from_json(options, full_json).z).to eq(cmplx(4.4,  1.1))
      end
      it "parses the r parameter" do
        expect(build_from_json(options, full_json).r).to eq(cmplx(9.9,  3.3))
      end
      it "parses the p parameter" do
        expect(build_from_json(options, full_json).p).to eq(cmplx(0.3,  0.5))
      end
    end

    context "when relying on defaults" do
      let :default_json do
        """
        {
          "fractal":    "Julia",
          "upperLeft":  "5.0+6.0i",
          "lowerRight": "6.0+5.0i"
        }
        """
        |> Poison.Parser.parse!
      end

      it "still parses the fractal type" do
        expect(build_from_json(options, default_json).fractal).to eq(:julia)
      end
      it "defaults the image size" do
        expect(build_from_json(options, default_json).size)
        .to eq(%Fractals.Size{width: 512, height: 384})
      end
      it "defaults the color scheme" do
        expect(build_from_json(options, default_json).color).to eq(:black_on_white)
      end
      it "defaults the random seed" do
        expect(build_from_json(options, default_json).seed).to eq(666)
      end
      it "still parses the upper-left corner" do
        expect(build_from_json(options, default_json).upper_left).to eq(cmplx(5.0, 6.0))
      end
      it "still parses the lower-right corder" do
        expect(build_from_json(options, default_json).lower_right).to eq(cmplx(6.0, 5.0))
      end
      it "defaults the c parameter" do
        expect(build_from_json(options, default_json).c).to eq(cmplx(1.0, 0.0))
      end
      it "defaults the z parameter" do
        expect(build_from_json(options, default_json).z).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the r parameter" do
        expect(build_from_json(options, default_json).r).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the p parameter" do
        expect(build_from_json(options, default_json).p).to eq(cmplx(0.0, 0.0))
      end
    end
  end

end
