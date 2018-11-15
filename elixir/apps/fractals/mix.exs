defmodule Fractals.Mixfile do
  use Mix.Project

  def project do
    [
      app: :fractals,
      version: "0.0.1",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.7",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger, :yaml_elixir],
      mod: {Fractals.Application, []}
    ]
  end

  defp deps do
    [
      {:complex, github: "jdfrens/elixir-complex", ref: "51e2804"},
      {:credo, "~> 0.10.0", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.0.0-rc.2", only: [:dev, :test], runtime: false},
      {:earmark, "~> 1.2", override: true},
      {:gen_stage, "~> 0.14"},
      {:inflex, "~> 1.10.0"},
      {:mogrify, "~> 0.6.1"},
      {:uuid, "~> 1.1"},
      {:yaml_elixir, "~> 2.1.0"}
    ]
  end

  defp aliases do
    [
      all: ["test", "mix format --check-formatted", "credo --strict"]
    ]
  end
end
