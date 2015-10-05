defmodule Fractals.Color.RandomSpec do

  use ESpec, async: true

  alias Fractals.Color.Random, warn: false

  let :ppm_regex do
    ~r/^\s*\d{1,3}\s+\d{1,3}\s+\d{1,3}\s*$/
  end

  describe ".build_random" do
    let :random_color do
      Random.build_random
    end

    it "is black inside" do
      expect(random_color.({:inside, :whatever, :whatever}))
      .to eq(PPM.black)
    end
    it "has a consistent 0 value" do
      original = random_color.({:outside, :whatever, 0})
      expect(random_color.({:outside, :whatever, 0})).to eq(original)
    end
    it "has a consistent 255 value" do
      original = random_color.({:outside, :whatever, 255})
      expect(random_color.({:outside, :whatever, 255})).to eq(original)
    end
    it "has a different value at 0 than at 255" do
      expect(random_color.({:outside, :whatever, 0}))
      .to_not eq(random_color.({:outside, :whatever, 255}))
    end
  end

end
