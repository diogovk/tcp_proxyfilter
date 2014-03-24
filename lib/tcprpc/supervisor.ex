defmodule Tcprpc.Supervisor do
  use Supervisor.Behaviour

  def start_link(arg1) do
    :supervisor.start_link({ :local, :listener_sup },  __MODULE__, [arg1])
  end

  def init([listargs]) do
    IO.inspect listargs
    children = [
      # Define workers and child supervisors to be supervised
      # worker(Tcprpc.Worker, [])
    ]
    TcpFilter.Server.start_link
    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
