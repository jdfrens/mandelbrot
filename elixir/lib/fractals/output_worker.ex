defmodule Fractals.OutputWorker do
  alias Fractals.Params

  # Client API

  def start_link(params) do
    pid = spawn_link(__MODULE__, :server, [0, params])
    Process.register(pid, __MODULE__)
    {:ok, pid}
  end

  def write(pid, chunk) do
    send pid, {:write, chunk}
  end

  # Server API

  def server(0, params) do
    write_header(params)
    server(1, params)
  end
  def server(number, %Params{chunk_count: count} = params) when number == count+1 do
    done(params)
    # YUCK: keeps server running so supervisor doesn't restart
    # IDEA: could use :transient strategy, but not simple replacement
    server(number + 1, params)
  end
  def server(chunk_number, params) do
    receive do
      {:write, {^chunk_number, data}} ->
        write_chunk(chunk_number, data, params)
        server(chunk_number + 1, params)
    end
  end

  # helpers

  def done(params) do
    notify_next_pid(params, {:done, self})
  end

  def write_header(%Params{size: size} = params) do
    PPM.p3_header(size.width, size.height)
    |> lines_to_file(params)
  end

  def write_chunk(chunk_number, data, params) do
    notify_next_pid(params, {:writing, chunk_number})
    lines_to_file(data, params)
  end

  def lines_to_file(lines, params) do
    Enum.each(lines, &(IO.puts(params.output_pid, &1)))
  end

  def notify_next_pid(params, message) do
    send params.next_pid, message
  end
end
