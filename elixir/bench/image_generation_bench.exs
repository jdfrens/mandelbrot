defmodule ImageGenerationBench do

  use Benchfella

  alias Fractals.Generator
  alias Fractals.Generator.Taskless
  alias Fractals.Generator.OriginalTasked
  alias Fractals.Generator.LongerTasked

  def options do
    Fractals.options("../json/burningship-line-blue.json")
  end

  bench "without tasks" do
    Generator.image(options, &Taskless.generate/2)
  end

  bench "original tasked" do
    Generator.image(options, &OriginalTasked.generate/2)
  end

  bench "longer tasked" do
    Generator.image(options, &LongerTasked.generate/2)
  end
end
