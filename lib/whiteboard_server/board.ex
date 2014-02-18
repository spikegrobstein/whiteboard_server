defmodule WhiteboardServer.Board do
  use GenServer.Behavior

  def start_link( name ) do
    :gen_server.start_link(__MODULE__, { name, 0, [], [] }, [])
  end

  # the state of a board is a tuple containing:
  # {
  #   name,
  #   counter,
  #   clients,
  #   data
  # }

  def init( state ) do
    { :ok, state }
  end

  ## event stuff
  @doc """
    receive a new packet from a client

    route it to the correct private function
  """
  def handle_cast( { :ingest_packet, pid, packet }, { name, counter, clients, data } ) do
    { event, payload } = parse_packet( packet )

    case event do
      "user_list" ->
        # send the user list to the requester
        send_user_list( pid, clients )

      "draw" ->
        # they sent a draw event, let's process it
        IO.inspect(payload)
        { counter, data } = ingest_draw( counter, clients, data, pid, payload )

      "pen_up" ->
        # received when a client stops drawing
        # this is broadcast to all clients that the user
        # stopped so they can clear local pen values for that user
        # payload is ignored
        IO.puts "pen_up"
        broadcast_pen_up( clients, pid )

      "console" ->
        IO.inspect(payload)

      _ ->
        IO.puts "got something else: #{ event }"
        send_unknown_packet_error( pid, event, payload )
    end

    { :noreply, { name, counter, clients, data } }
  end

  ## client stuff:

  def handle_cast( { :add_user, { pid, nick } }, { name, counter, clients, data } ) do

  end

  def handle_cast( { :del_user, pid }, { name, counter, clients, data } ) do

  end

  ## implementation functions

  # send the client list to the given pid
  defp send_user_list( pid, clients ) do
    user_list = Enum.map( clients, fn({ pid, nick }) -> [ userId: inspect(pid), nick: nick ] end)
    send pid, make_packet( "user_list", user_list )
  end

  ## util

  # given an event and payload, wrap up something to send to the
  # cowboy websocket, with the data JSON-encoded
  defp ws_response( event, payload ) do
    new_packet = [ event: event, payload: payload ]

    { :ok, json_packet } = JSON.encode( new_packet )

    { :message, json_packet }
  end

  # given a packet of data from a client
  # parse it out and return a tuple containing
  # { event, payload }
  defp parse_packet( packet ) do
    { :ok, event } = HashDict.fetch( packet, "event" )
    { :ok, payload } = HashDict.fetch( packet, "payload" )

    { event, payload }
  end

end
