defmodule StageEngineTest do
  use ExUnit.Case
  doctest StageEngine

  test "greets the world" do
    assert StageEngine.hello() == :world
  end
end
