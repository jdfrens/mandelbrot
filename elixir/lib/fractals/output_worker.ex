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
    done(options)
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

  # helpers

  def done(options) do
    notify_next_pid(options, {:done, self})
  end

  def write_header(%Options{size: size} = options) do
    PPM.p3_header(size.width, size.height)
    |> lines_to_file(options)
  end

  def write_chunk(chunk_number, data, options) do
    notify_next_pid(options, {:writing, chunk_number})
    lines_to_file(data, options)
  end

  def lines_to_file(lines, options) do
    Enum.each(lines, &(IO.puts(options.output_pid, &1)))
  end

  def notify_next_pid(options, message) do
    send options.next_pid, message
  end
end
