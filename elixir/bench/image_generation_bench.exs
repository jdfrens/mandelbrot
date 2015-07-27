defmodule ImageGenerationBench do

  use Benchfella

  alias Fractals.Generator

  def options do
    Fractals.options("../json/burningship-line-blue.json")
  end

  bench "without tasks" do
    Generator.image(options, &Generator.taskless_image/2)
  end

  bench "with tasks" do
    Generator.image(options, &Generator.tasked_image/2)
  end

  bench "with (longer) tasks" do
    Generator.image(options, &Generator.tasked_image2/2)
  end
end
