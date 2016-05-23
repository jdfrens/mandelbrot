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

  it "works" do
    Fractals.Output.out(subject, "x")
    expect(StringIO.contents(io)) |> to(eq({"", "x"}))
  end
end
