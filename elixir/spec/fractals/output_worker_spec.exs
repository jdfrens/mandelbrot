defmodule Fractals.OutputWorkerSpec do
  use ESpec, aysnc: true

  let :io do
    {:ok, pid} = StringIO.open("")
    pid
  end
  let :args do
    %{io: io, next_pid: self, chunk_count: chunk_count}
  end
  let :subject do
    {:ok, pid} = Fractals.OutputWorker.start_link(args)
    pid
  end

  context "when sending one chunk" do
    let :chunk_count, do: 1
    it "writes a chunk" do
      Fractals.OutputWorker.write(subject, {0, :options, ["a", "b", "c"]})
      expect_output(subject, io, "a\nb\nc\n")
    end
  end

  context "when sending multiple chunks" do
    let :chunk_count, do: 3

    it "writes multiple chunks" do
      Fractals.OutputWorker.write(subject, {0, :options, ["a", "b", "c"]})
      Fractals.OutputWorker.write(subject, {1, :options, ["m", "n", "o"]})
      Fractals.OutputWorker.write(subject, {2, :options, ["x", "y", "z"]})
      expect_output(subject, io, "a\nb\nc\nm\nn\no\nx\ny\nz\n")
    end

    it "writes multiple chunks received out of order" do
      Fractals.OutputWorker.write(subject, {1, :options, ["m", "n", "o"]})
      Fractals.OutputWorker.write(subject, {2, :options, ["x", "y", "z"]})
      Fractals.OutputWorker.write(subject, {0, :options, ["a", "b", "c"]})
      expect_output(subject, io, "a\nb\nc\nm\nn\no\nx\ny\nz\n")
    end
  end

  defp expect_output(subject, io, output) do
    receive do
      {:done, ^subject} ->
        expect(StringIO.contents(io)) |> to(eq({"", output}))
    after
      1000 ->
        ExUnit.Assertions.flunk "spec received no output-done message"
    end
  end
end
