defmodule Fractals.OutputSpec do
  use ESpec

  let :io do
    {:ok, pid} = StringIO.open("")
    pid
  end
  let :subject do
    {:ok, pid} = Fractals.Output.start_link(%{io: io})
    pid
  end

  it "writes a chunk" do
    Fractals.Output.write(subject, {0, :options, ["a", "b", "c"]})
    :timer.sleep(100)

    expect(StringIO.contents(io))
    |> to(eq({"", "a\nb\nc\n"}))
  end

  it "writes multiple chunks" do
    Fractals.Output.write(subject, {0, :options, ["a", "b", "c"]})
    Fractals.Output.write(subject, {1, :options, ["x", "y", "z"]})
    :timer.sleep(100)

    expect(StringIO.contents(io))
    |> to(eq({"", "a\nb\nc\nx\ny\nz\n"}))
  end

  it "writes multiple chunks received out of order" do
    Fractals.Output.write(subject, {1, :options, ["x", "y", "z"]})
    Fractals.Output.write(subject, {0, :options, ["a", "b", "c"]})
    :timer.sleep(100)

    expect(StringIO.contents(io))
    |> to(eq({"", "a\nb\nc\nx\ny\nz\n"}))
  end
end
