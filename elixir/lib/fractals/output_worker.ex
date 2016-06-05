defmodule Fractals.OutputWorker do
  alias Fractals.Options

  # Client API

  def start_link(options) do
    pid = spawn_link(__MODULE__, :server, [0, options])
    Process.register(pid, __MODULE__)
    {:ok, pid}
  end

  def write(pid, chunk) do
    send pid, {:write, chunk}
  end

  # Server API

  def server(0, options) do
    write_header(options)
    server(1, options)
  end
  def server(number, %Options{chunk_count: count} = options) when number == count+1 do
    notify_next_pid(options, {:done, self})
    # YUCK: keeps server running so supervisor doesn't restart
    # IDEA: could use :transient strategy, but not simple replacement
    server(number + 1, options)
  end
  def server(chunk_number, options) do
    receive do
      {:write, {^chunk_number, data}} ->
        write_chunk(chunk_number, data, options)
        server(chunk_number + 1, options)
    end
  end

  def write_header(options) do
    PPM.p3_header(options.size.width, options.size.height)
    |> Enum.each(&(IO.puts(options.output_pid, &1)))
  end

  def write_chunk(chunk_number, data, options) do
    notify_next_pid(options, {:writing, chunk_number})
    Enum.each(data, &(IO.puts(options.output_pid, &1)))
  end

  def notify_next_pid(options, message) do
    send options.next_pid, message
  end
end
