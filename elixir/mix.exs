defmodule Fractals.Mixfile do
  use Mix.Project

  def project do
    [app: :fractals,
     version: "0.0.1",
     elixir: "~> 1.2",
     escript: [main_module: Fractals.CLI],
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
      {:inflex, "~> 1.7.0"},
      {:poison, "~> 2.2"},
      {:credo, "~> 0.4.5", only: [:dev, :test]},
      {:dialyxir, "~> 0.3.3", only: [:dev, :test]},
      {:espec, "~> 0.8.22", only: :test}
    ]
  end
end
