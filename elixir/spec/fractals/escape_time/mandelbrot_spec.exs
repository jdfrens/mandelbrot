defmodule Fractals.EscapeTime.MandelbrotSpec do
  use ESpec, async: true
  import ComplexAssertions

  import Complex

  alias Fractals.EscapeTime.Mandelbrot

  describe ".pixels" do
    it "converts grid points to iterations" do
      grid_points = [cmplx(0.0, 0.0), cmplx(1.1, 1.1), cmplx(0.5, -0.5)]
      actual_pixels = Mandelbrot.pixels(grid_points)
      expected_pixels = [
        {cmplx(0.0, 0.0),      Mandelbrot.max_iterations},
        {cmplx(1.1, 3.52),     2},
        {cmplx(3.285, -1.344), 5}
      ]
      Enum.zip(actual_pixels, expected_pixels)
      |> Enum.map(fn {actual, expected} -> check_pixel(actual, expected) end)
    end
  end

  describe ".escape_time" do
    it "escapes" do
      check_pixel(
        Mandelbrot.escape_time(cmplx(1.1, 1.1)),
        {cmplx(1.1, 3.52), 2})
    end

    it "does not escape" do
      expect(Mandelbrot.escape_time(cmplx(0.0, 0.0)))
      |> to(eq({cmplx(0.0, 0.0), Mandelbrot.max_iterations}))
    end
  end

  let :z, do: cmplx(3.0, 2.0)
  let :c, do: cmplx(1.0, 1.0)

  describe ".iterator" do
    it "squares and adds" do
      expect(Mandelbrot.iterator(z, c)).to eq(cmplx(6.0, 13.0))
    end
  end

  def check_pixel({actual_result, actual_iterations},
    {expected_result, expected_iterations}) do
    expect(actual_result) |> to(be_complex_close_to expected_result, 0.001)
    expect(actual_iterations) |> to(eq expected_iterations)
  end
end
