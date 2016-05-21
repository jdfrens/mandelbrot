defmodule Fractals.Mixfile do
  use Mix.Project

  def project do
    [app: :fractals,
     version: "0.0.1",
     elixir: "~> 1.0",
     escript:         escript_config,
     build_embedded:  Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     preferred_cli_env: [espec: :test],
     deps: deps,
     aliases: aliases]
  end

  def application do
    [applications: [:logger, :poison]]
  end

  defp aliases do
    [test: ["espec"]]
  end

  defp deps do
    [
      {:poison, "~> 1.5.0"},
      {:inflex, "~> 1.5.0"},
      {:espec, "~> 0.8.0", only: :test},
      {:benchfella, "~> 0.2.0"},
      {:dogma, github: "lpil/dogma", only: :dev},
    ]
  end

  defp escript_config do
    [ main_module: Fractals ]
  end
end
