defmodule Fractals.EscapeTime.Julia do
  import Complex

  import Fractals.EscapeTime.Helpers

  def pixels(grid_points, params) do
    Enum.map(grid_points, &escape_time(&1, params))
  end

  def escape_time(grid_point, params) do
    grid_point
    |> Stream.iterate(&iterator(&1,params.c))
    |> Stream.with_index
    |> Stream.drop_while(fn {z, i} ->
      !escaped?(z, params.cutoff_squared) &&
        !inside?(i, params.max_iterations)
    end)
    |> Stream.take(1)
    |> Enum.to_list
    |> List.first
  end

  def iterator(z, c) do
    z |> square |> add(c)
  end
end
