defmodule CLITest do
  use ExUnit.Case
  doctest CLI

  test "greets the world" do
    assert CLI.hello() == :world
  end
end
