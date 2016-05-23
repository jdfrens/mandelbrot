defmodule Fractals.Mixfile do
  use Mix.Project

  def project do
    [app: :fractals,
     version: "0.0.1",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     preferred_cli_env: [espec: :test],
     deps: deps]
  end

  def application do
    [applications: [:logger],
     mod: {Fractals, []}]
  end

  defp deps do
    [
      {:inflex, "~> 1.5.0"},
      {:poison, "~> 2.0"},
      {:espec, "~> 0.8.20", only: :test}
    ]
  end
end
