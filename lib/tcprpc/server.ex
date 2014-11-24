defmodule State do
  defstruct port: nil, rsock: nil, lsock: nil, serv_sock: nil
end

defmodule TcpFilter.Server do
  #@server_host 'localhost'
  @server_host '10.4.0.12'
  #@server_port 5678
  @server_port 23
  @local_port 23

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
    tcp_options = tcp_options ++ [ { :ip, { 10, 4, 25, 36 } } ]
    { :ok, serv_sock } = :gen_tcp.connect(@server_host, @server_port, tcp_options)
    { :ok, %State{lsock: lsock, port: port, serv_sock: serv_sock}, 0 }
  end


  def handle_cast(:stop , state) do
    { :noreply, state }
  end

  def handle_info({:tcp_closed, _}, state) do
    IO.puts "Client closed socket - stopping connection "
    {:stop, :normal, state}
  end

  def handle_info({ :tcp, socket, raw_data }, state ) do
    #Utils.print_hex(raw_data)
    if state.serv_sock == socket do
      recv_server(state.rsock, raw_data)
    else
      Utils.print_hex(raw_data)
      send_server(state.serv_sock, raw_data)
    end
    { :noreply, state }
  end

  def handle_info(:timeout, state = %State{lsock: lsock}) do
    { :ok, rsock } = :gen_tcp.accept lsock
    { :noreply, %{ state | rsock: rsock } }
  end

  #ignore term color codes
  #  def recv_server(socket, <<0x1b, 0x5b, ?0, ?; , ?1, ?m, rest :: binary>>) do
  #    :gen_tcp.send(socket,<<0x1b, 0x5b, ?0, ?; , ?1, ?m>>)
  #    recv_server(socket, rest)
  #  end

  #ignore term color codes
  def recv_server(socket, <<0x1b, 0x5b, ?0 , ?; , _ , _, rest :: binary>>) do
    IO.inspect "ui"
    recv_server(socket, rest)
  end

  #def recv_server(socket, <<0x1b, 0x5b, ?3, ?; , ?1, ?3, rest :: binary>>) do
  #  recv_server(socket, rest)
  #end

  #ignore term color codes
  def recv_server(socket, <<0x1b, 0x5b, a, ?; , b, c, rest :: binary>>) do
    IO.inspect %{a: a, b: b, c: c}
    :gen_tcp.send(socket,<<0x1b, 0x5b, a, ?; , b, c>>)
    recv_server(socket, rest)
  end

  def recv_server(socket, <<bhead, rest :: binary>>) do
    :gen_tcp.send(socket,<< bhead >>)
    recv_server(socket, rest)
  end

  def recv_server(_socket, _) do
    :ok
  end

  def send_server(socket, <<0x1b, 91, 49, 53, 126, rest :: binary>>) do
    :gen_tcp.send(socket, "|****f5_PC****|")
    send_server(socket, rest)
  end

  #doc 'F5' Firmware 2.0/4.2/6.3
  def send_server(socket, <<0x1b, 79, 116, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 53, 126>>)
    send_server(socket, rest)
  end

  #doc 'F6' Firmware 2.0/4.2/6.3
  def send_server(socket, <<0x1b, 79, 117, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 55, 126>>)
    send_server(socket, rest)
  end

  #doc 'F7' Firmware 2.0/4.2/6.3
  def send_server(socket, <<0x1b, 79, 118, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 56, 126>>)
    send_server(socket, rest)
  end

  #doc 'F5' Firmware 3.9
  def send_server(socket, <<0x1b, 79, 84, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 53, 126>>)
    send_server(socket, rest)
  end

  #doc 'F6' Firmware 3.9
  def send_server(socket, <<0x1b, 79, 85, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 55, 126>>)
    send_server(socket, rest)
  end

  #doc 'F7' Firmware 3.9
  def send_server(socket, <<0x1b, 79, 86, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 56, 126>>)
    send_server(socket, rest)
  end

  #doc 'F8'
  def send_server(socket, <<0x1b, 79, 105, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 49, 57, 126>>)
    send_server(socket, rest)
  end

  #doc 'F9'
  def send_server(socket, <<0x1b, 79, 32, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 50, 48, 126>>)
    send_server(socket, rest)
  end

  #doc 'F10'
  def send_server(socket, <<0x1b, 79, 120, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 50, 49, 126>>)
    send_server(socket, rest)
  end

  #doc 'F11'
  def send_server(socket, <<0x1b, 79, 97, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 50, 51, 126>>)
    send_server(socket, rest)
  end

  #doc 'F12'
  def send_server(socket, <<0x1b, 79, 98, rest :: binary>>) do
    :gen_tcp.send(socket, <<0x1b, 91, 50, 52, 126>>)
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
TcpFilter.Server.start_link
