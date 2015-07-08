defmodule Fractals.Iterators.NewtonSpec do

  use ESpec, async: true

  import Complex
  alias Fractals.Iterators.Newton

  let :z, do: cmplx(3.0, 2.0)
  let :c, do: cmplx(1.0, 1.0)

  describe ".iterator" do
    it "diferentiates" do
      # apologies for the fragile nature of these specs
      expect(Newton.iterator.(z).real).to eq(1.009861932938856)
      expect(Newton.iterator.(z).imag).to eq(0.6429980276134122)
    end
  end

  describe ".p" do
    it "cubes and subtracts" do
      expect(Newton.p(z)).to eq(cmplx(-8.0, 46.0))
    end
  end

  describe ".p_prime" do
    it "squares and tripples" do
      expect(Newton.p_prime(z)).to eq(cmplx(15.0, 36.0))
    end
  end

end
