defmodule Fractals.ConversionWorkerTest do
  use ExUnit.Case, async: true

  alias Fractals.Params
  alias Fractals.ConversionWorker

  setup do
    test_pid = self()
    convert = fn source, destination -> send test_pid, {source, destination} end
    ConversionWorker.start_link(convert)

    params = %Params{source_pid: test_pid}

    [params: params]
  end

  describe "&convert/1" do
    test "reports done for a PPM file", %{params: params} do
      params = %Params{params | output_filename: "output.ppm"}
      ConversionWorker.convert(params)

      assert_receive {:done, _pid}
    end

    test "calls converter and reports done for a PNG file", %{params: params} do
      params = %Params{params | output_filename: "output.png"}
      ConversionWorker.convert(params)

      assert_receive {"output.ppm", "output.png"}
      assert_receive {:done, _pid}
    end
  end
end
