defmodule Tcprpc.Supervisor do
  use Supervisor.Behaviour

  def start_link() do
    :supervisor.start_link({ :local, :listener_sup },  __MODULE__, [])
  end

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      # worker(Tcprpc.Worker, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
