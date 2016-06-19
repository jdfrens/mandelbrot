ESpec.configure fn(config) ->
  config.before fn ->
    {:shared, hello: :world}
  end

  config.finally fn(_shared) ->
    :ok
  end
end

defmodule BeComplexCloseToAssertion do
  use ESpec.Assertions.Interface

  import Complex

  defp match(subject, [value, delta]) do
    result = magnitude(subtract(subject, value)) <= delta
    {result, result}
  end

  defp success_message(subject, [value, delta], _result, positive) do
    to = if positive, do: "is", else: "is not"
    "`#{inspect subject}` #{to} close to `#{inspect value}` with delta `#{inspect delta}`."
  end

  defp error_message(subject, [value, delta], result, positive) do
    to = if positive, do: "to", else: "not to"
    but = if result, do: "it is", else: "it isn't"
    "Expected `#{inspect subject}` #{to} be close to `#{inspect value}` with delta `#{inspect delta}`, but #{but}."
  end
end

defmodule ComplexAssertions do
  def be_complex_close_to(value, delta) do
    {BeComplexCloseToAssertion, [value, delta]}
  end
end
