defmodule Fractals.ConversionWorkerTest do
  use ExUnit.Case, async: true

  alias Fractals.ConversionWorker
  alias Fractals.Params

  setup do
    test_pid = self()
    convert = fn source, destination -> send(test_pid, {source, destination}) end
    broadcast = fn tag, params, opts -> send(test_pid, {:test_report, {tag, params, opts}}) end

    {:ok, pid} =
      ConversionWorker.start_link(
        convert: convert,
        broadcast: broadcast,
        name: :conversion_worker_under_test
      )

    [pid: pid]
  end

  describe "&convert/1" do
    test "reports done for a PPM file", %{pid: pid} do
      params = %Params{output_filename: "output.ppm"}
      ConversionWorker.convert(pid, params)

      assert_receive {:test_report, {:done, _pid, _params}}
    end

    test "calls converter and reports done for a PNG file", %{pid: pid} do
      params = %Params{output_filename: "output.png"}
      ConversionWorker.convert(pid, params)

      assert_receive {"output.ppm", "output.png"}
      assert_receive {:test_report, {:done, _pid, _params}}
    end
  end
end
