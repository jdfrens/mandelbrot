defmodule Fractals.Mixfile do
  use Mix.Project

  def project do
    [app: :fractals,
     version: "0.0.1",
     elixir: "~> 1.3",
     escript: [main_module: Fractals.CLI],
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [applications: [:logger, :porcelain, :yaml_elixir],
     mod: {Fractals, []}]
  end

  defp deps do
    [
      {:yaml_elixir, "~> 1.3.0"},
      {:porcelain, "~> 2.0"},
      {:inflex, "~> 1.8.0"},
      {:credo, "~> 0.5.0", only: [:dev, :test]},
      {:dialyxir, "~> 0.4.0", only: [:dev, :test]},
    ]
  end
end
