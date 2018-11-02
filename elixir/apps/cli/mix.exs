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
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {CLI.Application, []}
    ]
  end

  defp deps do
    [
      {:credo, "~> 0.10.0", only: [:dev, :test]},
      {:dialyxir, "~> 1.0.0-rc.2", only: [:dev, :test], runtime: false}
    ]
  end
end
