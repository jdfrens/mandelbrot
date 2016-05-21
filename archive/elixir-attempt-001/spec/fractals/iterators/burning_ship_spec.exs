defmodule Fractals.Iterators.BurningShipSpec do

  use ESpec, aysnc: true

  import Complex

  alias Fractals.Iterators.BurningShip

  let :z, do: cmplx(3.0, 2.0)
  let :c, do: cmplx(1.0, 1.0)

  describe ".iterator" do
    it "burns and squares and adds" do
      expect(BurningShip.iterator(c).(z)).to eq(cmplx(6.0, -11.0))
    end
  end

  describe ".burn" do
    it "negates the imaginary component" do
      expect(BurningShip.burn(cmplx(5.0, 3.0))).to eq(cmplx(5.0, -3.0))
    end
    it "keeps the imaginary component negative" do
      expect(BurningShip.burn(cmplx(5.0, -3.0))).to eq(cmplx(5.0, -3.0))
    end
    it "uses the absolute value of the real component" do
      expect(BurningShip.burn(cmplx(-5.0,  3.0))).to eq(cmplx(5.0, -3.0))
    end
  end

end
