defmodule Fractals.OutputWorkerTest do
  use ExUnit.Case, aysnc: true

  alias Fractals.OutputWorker
  alias Fractals.{Params, Size}

  setup do
    test_pid = self()
    next_stage = fn _params -> send(test_pid, :sent_to_next_stage) end
    {:ok, output_pid} = StringIO.open("")
    {:ok, subject} = OutputWorker.start_link({next_stage, :whatever})
    [output_pid: output_pid, subject: subject]
  end

  describe "when sending one chunk" do
    setup [:chunk_count_1, :params]

    test "writing a chunk", %{subject: subject, output_pid: output_pid, params: params} do
      OutputWorker.write(subject, %Chunk{number: 1, data: ["a", "b", "c"], params: params})
      assert_receive :sent_to_next_stage, 500
      assert StringIO.contents(output_pid) == {"", "P3\n3\n1\n255\na\nb\nc\n"}
    end
  end

  describe "when sending multiple chunks" do
    setup [:chunk_count_3, :params]

    test "writes multiple chunks", %{subject: subject, output_pid: output_pid, params: params} do
      OutputWorker.write(subject, %Chunk{number: 1, data: ["a"], params: params})
      OutputWorker.write(subject, %Chunk{number: 2, data: ["m"], params: params})
      OutputWorker.write(subject, %Chunk{number: 3, data: ["x"], params: params})
      assert_receive :sent_to_next_stage, 500
      assert StringIO.contents(output_pid) == {"", "P3\n3\n1\n255\na\nm\nx\n"}
    end

    test "writes multiple chunks received out of order", %{
      subject: subject,
      output_pid: output_pid,
      params: params
    } do
      OutputWorker.write(subject, %Chunk{number: 2, data: ["m"], params: params})
      OutputWorker.write(subject, %Chunk{number: 3, data: ["x"], params: params})
      OutputWorker.write(subject, %Chunk{number: 1, data: ["a"], params: params})
      assert_receive :sent_to_next_stage, 500
      assert StringIO.contents(output_pid) == {"", "P3\n3\n1\n255\na\nm\nx\n"}
    end
  end

  defp chunk_count_1(_context), do: [chunk_count: 1]
  defp chunk_count_3(_context), do: [chunk_count: 3]

  defp params(context) do
    params = %Params{
      output_pid: context.output_pid,
      size: %Size{width: 3, height: 1},
      chunk_count: context.chunk_count
    }

    [params: params]
  end
end
