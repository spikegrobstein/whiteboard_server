defmodule WhiteboardServer.ClientStore do
  use GenServer.Behaviour

  def start_link( clients ) do
    :gen_server.start_link({:local, :client_store}, __MODULE__, clients, [])
  end

  def init( clients ) do
    {:ok, clients}
  end

  def handle_call( :list_clients, _from, clients ) do
    { :reply, clients, clients }
  end

  def handle_cast( { :add_client, { pid, nick } }, clients ) do
    { :noreply, add_client( clients, { pid, nick } ) }
  end

  def handle_cast( { :del_client, pid }, clients ) do
    { :noreply, del_client( clients, pid ) }
  end

  def handle_cast( { :broadcast, message }, clients ) do
    { :noreply, broadcast( clients, message ) }
  end

  def handle_cast( { :handle_packet, pid, packet }, clients ) do
    handle_packet( clients, pid, packet )
    { :noreply, clients }
  end

  @doc """
    recieves a packet, parses it and routes it accordingly
  """
  def handle_packet( clients, pid, packet ) do
    { event, payload } = parse_packet( packet )

    case event do
      "user_list" ->
        send_user_list( pid, clients )

      "draw" ->
        IO.inspect(payload)
        broadcast_draw( clients, pid, payload )

      "pen_up" ->
        # received when a client stops drawing
        # this is broadcast to all clients that the user
        # stopped so they can clear local pen values for that user
        # payload is ignored
        broadcast_pen_up( clients, pid )

      _ ->
        IO.puts "got something else: #{ event }"
        send_unknown_packet_error( pid, event, payload )
    end
  end

  defp parse_packet( packet ) do
    { :ok, event } = HashDict.fetch( packet, "event" )
    { :ok, payload } = HashDict.fetch( packet, "payload" )

    { event, payload }
  end

  # add a new client
  # new client is a tuple that contains
  # client atom
  # new_pid => the pid of the client
  # nick => a nickname
  # prevents duplicate pids
  defp add_client( clients, { new_pid, nick } ) do

    send_user_join( clients, new_pid, nick )

    if Enum.any?( clients, fn({ pid, _nick }) -> pid == new_pid end ) do
      clients
    else
      [{ new_pid, nick }|clients]
    end
  end

  # delete a client based on the pid
  defp del_client( clients, pid_to_delete ) do
    IO.puts "removing client: #{ inspect pid_to_delete }"

    new_clients = Enum.reject( clients, fn({ pid, _nick}) -> pid_to_delete == pid end )

    send_user_part( new_clients, pid_to_delete )

    new_clients
  end

  # iterate over all clients, sending the given message
  # returns the clients
  defp broadcast( clients, message ) do
    Enum.each( clients, fn({ pid, _nick }) ->
      { :ok, json } = JSON.encode(message)
      send( pid, { :message, json } )
    end )

    clients
  end

  defp broadcast_draw( clients, source_pid, payload ) do

    draw_payload = HashDict.put( payload, :userId, inspect(source_pid) )

    Enum.each( clients, fn({pid, _nick}) ->
      send pid, make_packet( "draw", draw_payload )
    end )
  end

  @doc """
    notify all connected clients that the user lifted their pen
    this is sent to everyone
  """
  defp broadcast_pen_up( clients, source_pid ) do
    Enum.each( clients, fn({pid, _nick}) ->
      send pid, make_packet( "pen_up", [ userId: inspect(source_pid) ] )
    end )
  end

  @doc """
    given an event (as a string) and data (as a tuple or hash)
    return a JSON packet that's suitable to be sent over the wire
  """
  defp make_packet( event, payload ) do
    new_packet = [ event: event, payload: payload ]

    { :ok, json_packet } = JSON.encode( new_packet )

    { :message, json_packet }
  end

  defp send_user_list( pid, clients ) do
    user_list = Enum.map( clients, fn({ pid, nick }) -> [ userId: inspect(pid), nick: nick ] end)
    send pid, make_packet( "user_list", user_list )
  end

  defp send_user_join( clients, new_pid, new_nick ) do
    payload = [ userId: inspect(new_pid), nick: new_nick ]

    Enum.each( clients, fn({ pid, _nick }) ->
      send pid, make_packet( "user_join", payload )
    end )
  end

  defp send_user_part( clients, old_pid ) do
    payload = [ userId: inspect(old_pid) ]

    Enum.each( clients, fn({ pid, _nick }) ->
      send pid, make_packet( "user_part", payload )
    end )
  end

  defp send_unknown_packet_error( pid, event, payload ) do
    send pid, make_packet( "unknown_packet", [ event: event, payload: payload ] )
  end

end
