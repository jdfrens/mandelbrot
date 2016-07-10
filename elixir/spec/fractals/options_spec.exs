defmodule Fractals.OptionsSpec do
  use ESpec, async: true

  import Complex, only: :macros

  alias Fractals.{Options, Size}

  describe ".merge_configs" do
    it "merges two maps" do
      expect(Options.merge_configs([%{a: 1}, %{b: 2}]))
      |> to(eq(%{a: 1, b: 2}))
    end
    it "lets the second override the first" do
      expect(Options.merge_configs([%{a: 1}, %{a: 111}]))
      |> to(eq(%{a: 111}))
    end
    it "merges keyword list into map" do
      expect(Options.merge_configs([[a: 1], [b: 2]]))
      |> to(eq(%{a: 1, b: 2}))
    end
    it "merges a map with a keyword list into map" do
      expect(Options.merge_configs([%{a: 1}, [b: 2]]))
      |> to(eq(%{a: 1, b: 2}))
    end
  end

  describe ".parse" do
    let :options, do: %Fractals.Options{}

    context "when specifying all options" do
      let :full_yaml do
        """
          fractal:    Mandelbrot
          size:       720x480
          color:      Blue
          seed:       12345
          upperLeft:  0.0+55.2i
          lowerRight: 92.3+120.3i
          c:          3.14+4.13i
          z:          4.4+1.1i
          r:          9.9+3.3i
          p:          0.3+0.5i
        """
        |> YamlElixir.read_from_string(atoms: true)
      end

      it "parses the fractal type" do
        expect(Options.parse(options, full_yaml).fractal).to eq(:mandelbrot)
      end
      it "parses the image size" do
        expect(Options.parse(options, full_yaml).size)
        .to eq(%Size{width: 720, height: 480})
      end
      it "parses the color scheme" do
        expect(Options.parse(options, full_yaml).color).to eq(:blue)
      end
      it "parses the random seed" do
        expect(Options.parse(options, full_yaml).seed).to eq(12345)
      end
      it "parses the upper-left corner" do
        expect(Options.parse(options, full_yaml).upper_left).to eq(cmplx(0.0, 55.2))
      end
      it "parses the lower-right corder" do
        expect(Options.parse(options, full_yaml).lower_right).to eq(cmplx(92.3, 120.3))
      end
      it "parses the c parameter" do
        expect(Options.parse(options, full_yaml).c).to eq(cmplx(3.14, 4.13))
      end
      it "parses the z parameter" do
        expect(Options.parse(options, full_yaml).z).to eq(cmplx(4.4,  1.1))
      end
      it "parses the r parameter" do
        expect(Options.parse(options, full_yaml).r).to eq(cmplx(9.9,  3.3))
      end
      it "parses the p parameter" do
        expect(Options.parse(options, full_yaml).p).to eq(cmplx(0.3,  0.5))
      end
    end

    context "when relying on defaults" do
      let :default_yaml do
        """
          fractal:    Julia
          upperLeft:  5.0+6.0i
          lowerRight: 6.0+5.0i
        """
        |> YamlElixir.read_from_string(atoms: true)
      end

      it "still parses the fractal type" do
        expect(Options.parse(options, default_yaml).fractal).to eq(:julia)
      end
      it "defaults the image size" do
        expect(Options.parse(options, default_yaml).size)
        .to eq(%Size{width: 512, height: 384})
      end
      it "defaults the color scheme" do
        expect(Options.parse(options, default_yaml).color).to eq(:black_on_white)
      end
      it "defaults the random seed" do
        expect(Options.parse(options, default_yaml).seed).to eq(666)
      end
      it "still parses the upper-left corner" do
        expect(Options.parse(options, default_yaml).upper_left).to eq(cmplx(5.0, 6.0))
      end
      it "still parses the lower-right corder" do
        expect(Options.parse(options, default_yaml).lower_right).to eq(cmplx(6.0, 5.0))
      end
      it "defaults the c parameter" do
        expect(Options.parse(options, default_yaml).c).to eq(cmplx(1.0, 0.0))
      end
      it "defaults the z parameter" do
        expect(Options.parse(options, default_yaml).z).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the r parameter" do
        expect(Options.parse(options, default_yaml).r).to eq(cmplx(0.0, 0.0))
      end
      it "defaults the p parameter" do
        expect(Options.parse(options, default_yaml).p).to eq(cmplx(0.0, 0.0))
      end
    end
  end

  describe ".compute_chunk_count" do
    it "divides evenly" do
      options = %Options{
        size: %Size{width: 10, height: 2},
        chunk_size: 5}
      expect(Options.compute_chunk_count(options)) |> to(eq(4))
    end

    it "adds one for a remainder" do
      options = %Options{
        size: %Size{width: 10, height: 2},
        chunk_size: 3}
      expect(Options.compute_chunk_count(options)) |> to(eq(7))
    end
  end
end
