defmodule Fractals.NextFunctionSpec do

  use ESpec

  import Complex, only: :macros

  let :z, do: cmplx(3.0, 2.0)
  let :c, do: cmplx(1.0, 1.0)

  describe ".mandelbrot_next" do
    import Fractals.NextFunction, only: [ mandelbrot_next: 1 ]

    it "squares and adds" do
      expect(mandelbrot_next(c).(z)).to eq(cmplx(6.0, 13.0))
    end
  end

  describe ".julia_next" do
    import Fractals.NextFunction, only: [ julia_next: 1 ]

    it "squares and adds" do
      expect(julia_next(c).(z)).to eq(cmplx(6.0, 13.0))
    end
  end

  describe ".burning_ship_next" do
    import Fractals.NextFunction, only: [ burning_ship_next: 1 ]

    it "burns and squares and adds" do
      expect(burning_ship_next(c).(z)).to eq(cmplx(6.0, -11.0))
    end

    describe ".burn" do
      import Fractals.NextFunction, only: [ burn: 1 ]

      it "negates the imaginary component" do
        expect(burn(cmplx(5.0, 3.0))).to eq(cmplx(5.0, -3.0))
      end
      it "keeps the imaginary component negative" do
        expect(burn(cmplx(5.0, -3.0))).to eq(cmplx(5.0, -3.0))
      end
      it "uses the absolute value of the real component" do
        expect(burn(cmplx(-5.0,  3.0))).to eq(cmplx(5.0, -3.0))
      end
    end
  end

  describe ".newton" do
    import Fractals.NextFunction, only: [ newton: 0, p: 1, p_prime: 1 ]

    it "diferentiates" do
      # apologies for the fragile nature of these specs
      expect(newton.(z).real).to eq(1.009861932938856)
      expect(newton.(z).imag).to eq(0.6429980276134122)
    end

    describe ".p" do
      it "cubes and subtracts" do
        expect(p(z)).to eq(cmplx(-8.0, 46.0))
      end
    end

    describe ".p_prime" do
      it "squares and tripples" do
        expect(p_prime(z)).to eq(cmplx(15.0, 36.0))
      end
    end
  end
end
