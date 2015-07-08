defmodule Fractals.Iterators.JuliaSpec do

  use ESpec, async: true

  import Complex
  alias Fractals.Iterators.Julia

  let :z, do: cmplx(3.0, 2.0)
  let :c, do: cmplx(1.0, 1.0)

  describe ".iterator" do
    it "squares and adds" do
      expect(Julia.iterator(c).(z)).to eq(cmplx(6.0, 13.0))
    end
  end

end
