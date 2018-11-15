defmodule CLI.MixProject do
  use Mix.Project

  def project do
    [
      app: :cli,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.7",
      escript: [main_module: CLI, name: "fractals"],
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:credo, "~> 0.10.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0.0-rc.2", only: [:dev, :test], runtime: false},
      {:fractals, in_umbrella: true},
      {:stage_engine, in_umbrella: true},
      {:uniprocess_engine, in_umbrella: true}
    ]
  end
end
