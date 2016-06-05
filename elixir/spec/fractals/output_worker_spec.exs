defmodule Fractals.OutputWorkerSpec do
  use ESpec, aysnc: true

  alias Fractals.{Options, Size}
  alias Fractals.OutputWorker

  import ExUnit.Assertions, only: [flunk: 1]

  let :output_pid do
    {:ok, pid} = StringIO.open("")
    pid
  end
  let :options do
    %Options{
      size: size,
      output_pid: output_pid,
      next_pid: self,
      chunk_count: chunk_count
    }
  end
  let :subject do
    {:ok, pid} = OutputWorker.start_link(options)
    pid
  end

  finally do: Process.unregister(OutputWorker)

  context "when sending one chunk" do
    let :size, do: %Size{width: 3, height: 1}
    let :chunk_count, do: 1
    it "writes a chunk" do
      OutputWorker.write(subject, {1, ["a", "b", "c"]})
      expect_output(subject, output_pid, "P3\n3\n1\n255\na\nb\nc\n")
    end
  end

  context "when sending multiple chunks" do
    let :size, do: %Size{width: 3, height: 1}
    let :chunk_count, do: 3

    it "writes multiple chunks" do
      OutputWorker.write(subject, {1, ["a"]})
      OutputWorker.write(subject, {2, ["m"]})
      OutputWorker.write(subject, {3, ["x"]})
      expect_output(subject, output_pid, "P3\n3\n1\n255\na\nm\nx\n")
    end

    it "writes multiple chunks received out of order" do
      OutputWorker.write(subject, {2, ["m"]})
      OutputWorker.write(subject, {3, ["x"]})
      OutputWorker.write(subject, {1, ["a"]})
      expect_output(subject, output_pid, "P3\n3\n1\n255\na\nm\nx\n")
    end
  end

  defp expect_output(subject, output_pid, output) do
    receive do
      {:done, ^subject} ->
        expect(StringIO.contents(output_pid)) |> to(eq({"", output}))
    after
      1000 ->
        {"", actual} = StringIO.contents(output_pid)
        flunk "spec received no output-done message, expected: #{output}, got: #{actual}"
    end
  end
end
