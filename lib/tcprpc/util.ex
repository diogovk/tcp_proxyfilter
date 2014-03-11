
defmodule Utils do
  def print_hex(<< bhead, rest :: binary>>) do
    IO.inspect(bhead)
    print_hex(rest)
  end

  def print_hex(_) do
    :ok
  end

  
end


