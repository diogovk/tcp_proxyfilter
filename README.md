tcp_proxyfilter
===============

Example of tcp proxy filter using elixir.
It waits for a connection in a port, and then connects to a server/port sending, connecting the two, acting as a proxy.

For your filters, you should implement your versions of the following:
```elixir
def send_server(socket, <<27, 91, 49, 53, 126, rest :: binary>>) do
  :gen_tcp.send(socket, "|****f5_PC****|")
  send_server(socket, rest)
end
```

In this example, it will send the string `|****f5_PC****|` instead of the binary `<<27, 91, 49, 53, 126>>`


I realise this should have tests, and a proper supervisor, but I have no time to continue this.
Currently the supervisor is only a stub
Because I don't have a supervisor, you have to manually restart iex each time the client or server disconnects.
Said that, it's a good example of TCP use in elixir.

To test I used BSD's netcat:
```bash
# Create a TCP server on port 5678
nc -k -l 5678
```
Start proxy:
```bash
iex server.ex
```
Make a tcp client connect through our proxy running on port 1055
```bash
nc localhost 1055
```

If you send the sequence for F5 it will print `|****f5_PC****|`.
Any other sequence will be sent back and forth unfiltered.


