defmodule Fractals.Mixfile do
  use Mix.Project

  def project do
    [
      app: :fractals,
      version: "0.0.1",
      elixir: "~> 1.3",
      escript: [main_module: Fractals.CLI],
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [applications: [:logger, :porcelain, :yaml_elixir], mod: {Fractals, []}]
  end

  defp deps do
    [
      {:credo, "~> 0.9.1", only: [:dev, :test]},
      {:dialyxir, "~> 0.5.0", only: [:dev, :test]},
      {:gen_stage, "~> 0.14"},
      {:inflex, "~> 1.10.0"},
      {:porcelain, "~> 2.0"},
      {:uuid, "~> 1.1"},
      {:yaml_elixir, "~> 2.1.0"}
    ]
  end
end
