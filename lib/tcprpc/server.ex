defmodule Tcprpc2.Server do
  use GenServer.Behaviour
  @server_host 'localhost'
  @server_port 5678
  @local_port 23

  defrecord State, port: nil, rsock: nil, lsock: nil, serv_sock: nil

  def start_link(port) do
    :gen_server.start_link({ :local, :tcprpc }, __MODULE__, port, [])
  end

  def start_link() do
    start_link @local_port
  end

  def stop() do
    :gen_server.cast(:tcprpc, :stop)
  end

  def init (port) do
    tcp_options = [
        :binary,
        { :active, true },
        { :reuseaddr, true },
        {:nodelay, true}
      ]
    { :ok, lsock } = :gen_tcp.listen(port, tcp_options)
    { :ok, serv_sock } = :gen_tcp.connect(@server_host, @server_port, tcp_options)
    { :ok, State.new(lsock: lsock, port: port, serv_sock: serv_sock), 0 }
  end


  def handle_cast(:stop , state) do
    { :noreply, state }
  end

  def handle_info({:tcp_closed, _}, state) do
    IO.puts "Client closed socket - stopping connection "
    {:stop, :normal, state}
  end

  def handle_info({ :tcp, socket, raw_data }, state ) do
    if state.serv_sock == socket do
      :gen_tcp.send(state.rsock, raw_data)
    else
      Utils.print_hex(raw_data)
      send_server(state.serv_sock, raw_data)
    end
    { :noreply, state }
  end

  def handle_info(:timeout, state = State[lsock: lsock]) do
    { :ok, rsock } = :gen_tcp.accept lsock
    { :noreply, state.rsock(rsock) }
  end

  def send_server(socket, <<27, 91, 49, 53, 126, rest :: binary>>) do
    :gen_tcp.send(socket, "|****f5_PC****|")
    send_server(socket, rest)
  end

  def send_server(socket, <<27, 79, 84, rest :: binary>>) do
    :gen_tcp.send(socket, "|****f5_COLET****|")
    send_server(socket, rest)
  end

  def send_server(socket, <<bhead, rest :: binary>>) do
    :gen_tcp.send(socket,<< bhead >>)
    send_server(socket, rest)
  end

  def send_server(_socket, _) do
    :ok
  end

end
Tcprpc2.Server.start_link
