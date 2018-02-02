# http://teamon.eu/2016/measuring-visualizing-genstage-flow-with-gnuplot/
defmodule Progress do
  use GenServer

  @timeres :millisecond

  # Client API

  def start_link(opts) do
    scopes = Keyword.get(opts, :scopes, [])
    GenServer.start_link(__MODULE__, scopes, name: __MODULE__)
  end

  def stop do
    GenServer.stop(__MODULE__)
  end

  def incr(scope, n \\ 1, time \\ :os.system_time(@timeres)) do
    GenServer.cast(__MODULE__, {:incr, scope, n, time})
  end

  # Server API

  def init(scopes, start_time \\ :os.system_time(@timeres)) do
    files =
      Enum.map(scopes, fn scope ->
        {scope, File.open!("logs/progress-#{scope}.log", [:write])}
      end)

    counts = Enum.map(scopes, fn scope -> {scope, 0} end)
    Enum.each(files, fn {_, io} -> write(io, 0, start_time, start_time) end)

    {:ok, {start_time, files, counts}}
  end

  def handle_cast({:incr, scope, n, time}, {start_time, files, counts}) do
    {value, counts} = Keyword.get_and_update!(counts, scope, &{&1 + n, &1 + n})
    write(files[scope], value, time, start_time)

    {:noreply, {start_time, files, counts}}
  end

  defp write(file, value, time, start_time) do
    elapsed_time = time - start_time
    IO.write(file, "#{elapsed_time}\t#{value}\n")
  end
end
