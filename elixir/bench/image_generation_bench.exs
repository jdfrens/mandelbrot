defmodule ImageGenerationBench do

  use Benchfella

  alias Fractals.Options
  alias Fractals.Generator
  alias Fractals.Generator.Taskless
  alias Fractals.Generator.OriginalTasked
  alias Fractals.Generator.LongerTasked
  alias Fractals.Generator.PooledProcesses

  def options do
    %Fractals.Options{}
    |> Options.parse_file("../json/burningship-line-blue.json")
    |> Options.cache
  end

  bench "without tasks" do
    Generator.image(options, &Taskless.generate/1)
  end

  bench "original tasked" do
    Generator.image(options, &OriginalTasked.generate/1)
  end

  bench "longer tasked" do
    Generator.image(options, &LongerTasked.generate/1)
  end

  # TODO: write a macro!

  bench "pooled 100  2" do
    options = %{options | chunk_size: 100, processes: 2}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 1000  2" do
    options = %{options | chunk_size: 1000, processes: 2}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 5000  2" do
    options = %{options | chunk_size: 5000, processes: 2}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 100  4" do
    options = %{options | chunk_size: 100, processes: 4}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 1000  4" do
    options = %{options | chunk_size: 1000, processes: 4}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 5000  4" do
    options = %{options | chunk_size: 5000, processes: 4}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 100  8" do
    options = %{options | chunk_size: 100, processes: 8}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 1000  8" do
    options = %{options | chunk_size: 1000, processes: 8}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 5000  8" do
    options = %{options | chunk_size: 5000, processes: 8}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 100 16" do
    options = %{options | chunk_size: 100, processes: 16}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 1000 16" do
    options = %{options | chunk_size: 1000, processes: 16}
    Generator.image(options, &PooledProcesses.generate/1)
  end

  bench "pooled 5000 16" do
    options = %{options | chunk_size: 5000, processes: 16}
    Generator.image(options, &PooledProcesses.generate/1)
  end
end
