defmodule Fractals.ParamsSpec do
  use ESpec, async: true

  import Complex, only: :macros

  alias Fractals.{Params, Size}

  describe ".parse" do
    context "when specifying all params" do
      let :raw_params, do: [params_filename: "spec/inputs/full_params.yml"]

      it "parses the fractal type" do
        expect(Params.parse(raw_params).fractal).to eq(:mandelbrot)
      end
      it "parses the image size" do
        expect(Params.parse(raw_params).size)
        .to eq(%Size{width: 720, height: 480})
      end
      it "parses the color scheme" do
        expect(Params.parse(raw_params).color).to eq(:blue)
      end
      it "parses the random seed" do
        expect(Params.parse(raw_params).seed).to eq(12345)
      end
      it "parses the upper-left corner" do
        expect(Params.parse(raw_params).upper_left).to eq(cmplx(0.0, 55.2))
      end
      it "parses the lower-right corder" do
        expect(Params.parse(raw_params).lower_right).to eq(cmplx(92.3, 120.3))
      end
      it "parses the c parameter" do
        expect(Params.parse(raw_params).c).to eq(cmplx(3.14, 4.13))
      end
      it "parses the z parameter" do
        expect(Params.parse(raw_params).z).to eq(cmplx(4.4,  1.1))
      end
      it "parses the r parameter" do
        expect(Params.parse(raw_params).r).to eq(cmplx(9.9,  3.3))
      end
      it "parses the p parameter" do
        expect(Params.parse(raw_params).p).to eq(cmplx(0.3,  0.5))
      end
    end

    context "when relying on defaults" do
      let :raw_params, do: []

      it "defaults to Mandelbrot" do
        expect(Params.parse(raw_params).fractal).to eq(:mandelbrot)
      end
      it "defaults the image size" do
        expect(Params.parse(raw_params).size)
        .to eq(%Size{width: 512, height: 384})
      end
      it "defaults the color scheme" do
        expect(Params.parse(raw_params).color).to eq(:black_on_white)
      end
      it "defaults the random seed" do
        expect(Params.parse(raw_params).seed).to eq(666)
      end
      it "still parses the upper-left corner" do
        expect(Params.parse(raw_params).upper_left).to eq(cmplx(5.0, 6.0))
      end
      it "still parses the lower-right corder" do
        expect(Params.parse(raw_params).lower_right).to eq(cmplx(6.0, 5.0))
      end
      it "defaults the c parameter" do
        expect(Params.parse(raw_params).c).to eq(cmplx(1.0, 0.0))
      end
      it "defaults the z parameter" do
        expect(Params.parse(raw_params).z).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the r parameter" do
        expect(Params.parse(raw_params).r).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the p parameter" do
        expect(Params.parse(raw_params).p).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the chunk size" do
        expect(Params.parse(raw_params).chunk_size).to eq(1000)
      end
    end

    context "when specifying flags and input file" do
      let :raw_params do
        [
          c: "99.0+0.0i",
          params_filename: "spec/inputs/partial_params.yml",
          fractal: "Burningship"
        ]
      end

      it "recognizes the early flag" do
        expect(Params.parse(raw_params).c) |> to(eq(cmplx(99.0)))
      end
      it "recognizes a value from the file" do
        expect(Params.parse(raw_params).color) |> to(eq(:blue))
      end
      it "recognizes a value overridden by a flag" do
        expect(Params.parse(raw_params).fractal) |> to(eq(:burningship))
      end
    end

    context "when values need to be computed" do
      it "divides evenly" do
        expect(Params.parse([size: "10x2", chunk_size: 5]).chunk_count)
        |> to(eq(4))
      end

      it "adds one for a remainder" do
        expect(Params.parse([size: "10x2", chunk_size: 3]).chunk_count)
        |> to(eq(7))
      end

      it "always computes and overrides explicit setting" do
        expect(Params.parse([size: "10x2", chunk_size: 3, chunk_count: 99999]).chunk_count)
        |> to(eq(7))
      end
    end
  end
end
