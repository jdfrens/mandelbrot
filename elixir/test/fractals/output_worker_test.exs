defmodule Fractals.OutputWorkerTest do
  use ExUnit.Case, aysnc: true

  alias Fractals.{Params, Size}
  alias Fractals.OutputWorker

  setup do
    {:ok, output_pid} = StringIO.open("")
    [output_pid: output_pid]
  end

  describe "when sending one chunk" do
    setup [:chunk_count_1, :subject]

    test "writing a chunk", %{subject: subject, output_pid: output_pid} do
      OutputWorker.write(subject, {1, ["a", "b", "c"]})
      assert_receive {:done, _self}, 500
      assert StringIO.contents(output_pid) == {"", "P3\n3\n1\n255\na\nb\nc\n"}
    end
  end

  describe "when sending multiple chunks" do
    setup [:chunk_count_3, :subject]

    test "writes multiple chunks", %{subject: subject, output_pid: output_pid} do
      OutputWorker.write(subject, {1, ["a"]})
      OutputWorker.write(subject, {2, ["m"]})
      OutputWorker.write(subject, {3, ["x"]})
      assert_receive {:done, _self}, 500
      assert StringIO.contents(output_pid) == {"", "P3\n3\n1\n255\na\nm\nx\n"}
    end

    test "writes multiple chunks received out of order", %{subject: subject, output_pid: output_pid} do
      OutputWorker.write(subject, {2, ["m"]})
      OutputWorker.write(subject, {3, ["x"]})
      OutputWorker.write(subject, {1, ["a"]})
      assert_receive {:done, _self}, 500
      assert StringIO.contents(output_pid) == {"", "P3\n3\n1\n255\na\nm\nx\n"}
    end
  end

  defp chunk_count_1(_context), do: [chunk_count: 1]
  defp chunk_count_3(_context), do: [chunk_count: 3]

  defp subject(context) do
    params = %Params{
      output_pid: context.output_pid,
      next_pid: self,
      size: %Size{width: 3, height: 1},
      chunk_count: context.chunk_count
    }
    {:ok, subject} = OutputWorker.start_link(params)

    [subject: subject]
  end
end
