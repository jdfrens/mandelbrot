defmodule Fractals.MixProject do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [ignore_warnings: ".dialyzer-ignore-warnings"]
    ]
  end

  defp deps do
    []
  end
end
