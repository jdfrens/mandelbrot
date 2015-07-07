defmodule Fractals.OptionsSpec do

  use ESpec

  import Fractals.Options, only: [ parse: 1 ]

  describe ".parse" do
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
    end

    context "when specifying all options" do
      it "parses the fractal type" do
        expect(parse(full_json).fractal).to eq(:mandelbrot)
      end
      it "parses the image size" do
        expect(parse(full_json).size).to eq(%Fractals.Size{ width: 720, height: 480 })
      end
      it "parses the color scheme" do
        expect(parse(full_json).color).to eq(:blue)
      end
      it "parses the random seed" do
        expect(parse(full_json).seed).to eq(12345)
      end
      it "parses the upper-left corner" do
        expect(parse(full_json).upper_left).to eq(%Complex{ real: 0.0, imag: 55.2 })
      end
      it "parses the lower-right corder" do
        expect(parse(full_json).lower_right).to eq(%Complex{ real: 92.3, imag: 120.3 })
      end
      it "parses the c parameter" do
        expect(parse(full_json).c).to eq(%Complex{ real: 3.14, imag: 4.13 })
      end
      it "parses the z parameter" do
        expect(parse(full_json).z).to eq(%Complex{ real: 4.4,  imag: 1.1 })
      end
      it "parses the r parameter" do
        expect(parse(full_json).r).to eq(%Complex{ real: 9.9,  imag: 3.3 })
      end
      it "parses the p parameter" do
        expect(parse(full_json).p).to eq(%Complex{ real: 0.3,  imag: 0.5 })
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
      end

      it "still parses the fractal type" do
        expect(parse(default_json).fractal).to eq(:julia)
      end
      it "defaults the image size" do
        expect(parse(default_json).size).to eq(%Fractals.Size{ width: 512, height: 384 })
      end
      it "defaults the color scheme" do
        expect(parse(default_json).color).to eq(:black_on_white)
      end
      it "defaults the random seed" do
        expect(parse(default_json).seed).to eq(666)
      end
      it "still parses the upper-left corner" do
        expect(parse(default_json).upper_left).to eq(%Complex{ real: 5.0, imag: 6.0 })
      end
      it "still parses the lower-right corder" do
        expect(parse(default_json).lower_right).to eq(%Complex{ real: 6.0, imag: 5.0 })
      end
      it "defaults the c parameter" do
        expect(parse(default_json).c).to eq(%Complex{ real: 1.0, imag: 0.0 })
      end
      it "defaults the z parameter" do
        expect(parse(default_json).z).to eq(%Complex{ real: 0.0, imag: 0.0 })
      end
      it "defaults the r parameter" do
        expect(parse(default_json).r).to eq(%Complex{ real: 0.0, imag: 0.0 })
      end
      it "defaults the p parameter" do
        expect(parse(default_json).p).to eq(%Complex{ real: 0.0, imag: 0.0 })
      end
    end
  end

end
