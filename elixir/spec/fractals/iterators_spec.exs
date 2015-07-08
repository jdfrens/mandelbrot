defmodule Fractals.IteratorsSpec do

  use ESpec, async: true

  import Complex, warn: false

  let :grid_point, do: cmplx(3.0, 2.0)
  let :options do
    %Fractals.Options{
      size:        %Fractals.Size{ width: 2, height: 3 },
      upper_left:  cmplx(-1.0,  1.0),
      lower_right: cmplx( 1.0, -1.0)
    }
  end


  describe ".build_iterator" do
    import Fractals.Iterators, only: [ build_iterator: 2 ]

    it "works for a Mandelbrot set" do
      options = %{ options | fractal: :mandelbrot }
      expect(build_iterator(grid_point, options)).to be_function
    end

    it "works for a Julia set" do
      options = %{ options | fractal: :julia }
      expect(build_iterator(grid_point, options)).to be_function
    end

    it "works for a Burning Ship" do
      options = %{ options | fractal: :burningship }
      expect(build_iterator(grid_point, options)).to be_function
    end

    it "works for the Netwon method" do
      options = %{ options | fractal: :newton }
      expect(build_iterator(grid_point, options)).to be_function
    end

    it "works for the nova fractal" do
      options = %{ options | fractal: :nova }
      expect(build_iterator(grid_point, options)).to be_function
    end

  end

end
