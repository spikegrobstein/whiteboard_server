defmodule WhiteboardServer.Board do
  use GenServer.Behaviour

  def start_link( key ) do
    :gen_server.start_link(__MODULE__, key, [])
  end

  # the state of a board is a tuple containing:
  # {
  #   key,
  #   counter,
  #   clients,
  #   data
  # }

  def init( key ) do
    IO.puts "created board with #{ key } (#{ inspect self })"
    { :ok, { key, 0, [], [] } }
  end

  ## event stuff
  @doc """
    receive a new packet from a client

    route it to the correct private function
  """
  def handle_cast( { :ingest_packet, pid, { event, payload } }, { key, counter, clients, data } ) do
    case event do
      "draw" ->
        # they sent a draw event, let's process it
        # IO.inspect(payload)
        { counter, data } = ingest_draw( counter, clients, data, pid, event, payload )

      "pen_up" ->
        # received when a client stops drawing
        # this is broadcast to all clients that the user
        # stopped so they can clear local pen values for that user
        # payload is ignored
        { counter, data } = ingest_draw( counter, clients, data, pid, event, payload )

      "get_range" ->
        stream_data_to_client( data, payload, pid )

      "console" ->
        IO.inspect(payload)

      _ ->
        IO.puts "got something else: #{ event }"
        send_unknown_packet_error( pid, { event, payload } )
    end

    { :noreply, { key, counter, clients, data } }
  end

  ## client stuff:

  @doc """
    add the given user to the user list
    broadcast to everyone that this user joined
  """
  def handle_cast( { :add_user, { pid, user_id, nick } }, { key, counter, clients, data } ) do
    IO.puts "add user: #{ inspect pid } - #{ user_id } - #{ nick }"
    broadcast clients, { "user_join", {}, { inspect(pid), user_id, nick } }

    clients = add_client( clients, { pid, user_id, nick } )

    { :noreply, { key, counter, clients, data } }
  end

  @doc """
    the given user has left
    broadcast to everyone that they left
  """
  def handle_cast( { :del_user, pid }, { key, counter, clients, data } ) do
    broadcast clients, { "user_leave", {}, { inspect(pid) } }

    clients = del_client( clients, pid )

    { :noreply, { key, counter, clients, data } }
  end

  @doc """
    send back info about this whiteboard
  """
  def handle_call( :hello, _from, state ) do
    { key, counter, clients, _ } = state

    user_list = make_user_list( clients )

    { :reply, { key, counter, user_list }, state }
  end

  @doc """
    return the user list
  """
  def handle_call( :user_list, _from, state ) do
    { _, _, clients, _ } = state

    list = make_user_list( clients )

    { :reply, list, state }
  end

  ## implementation functions

  # add a client
  defp add_client( clients, { pid, user_id, nick } ) do
    [{ pid, user_id, nick }|clients]
  end

  # delete a client by pid
  defp del_client( clients, pid_to_delete ) do
    Enum.reject clients, fn({ pid, _id, _nick }) ->
      pid_to_delete == pid
    end
  end

  ## util

  # given the list of clients and well-formed packet
  # send all the clients the packet
  defp broadcast( clients, packet ) do
    Enum.each clients, fn({ pid, _id, _nick }) ->
      send pid, { :packet, packet }
    end
  end

  defp send_unknown_packet_error( pid, packet ) do
    send pid, { :packet, { "unknown_packet", packet } }
  end

  # process a draw event, broadcast to clients, return {counter, data}
  defp ingest_draw( counter, clients, data, pid, event, payload ) do
    IO.inspect { :ingest_draw, event, counter, payload }
    counter = counter + 1

    { _pid, user_id, _nick } = client_for_pid( clients, pid )

    payload = HashDict.put( payload, :userId, user_id )
    headers = HashDict.new( sequence: counter )

    # create the packet tuple
    packet = { event, headers, payload }

    broadcast clients, packet

    { counter, [packet|data] }
  end

  defp client_for_pid( clients, pid ) do
    Enum.find clients, fn(c) ->
      { client_pid, _user_id, _nick } = c
      client_pid == pid
    end
  end

  # given the client list, build a list of users for the cilent
  # this is a list of tuples containing { user_id, nick } without dupes.
  defp make_user_list( clients ) do
    clients
      |> Enum.map(fn(c)->
          { _pid, user_id, nick } = c
          { user_id, nick }
        end)
      |> Enum.uniq(fn({ user_id, _ })->
          user_id
        end)
  end

  # stream the data to the given pid
  # payload should be a HashDict with a from and to field.
  defp stream_data_to_client( data, payload, pid ) do
    from = HashDict.get( payload, "from" )
    to = HashDict.get( payload, "to" )

    # fetch the sequences in the range
    # reverse it
    # send to client
    data
    |> Enum.filter( fn({ _event, headers, _payload}) ->
        sequence = HashDict.get( headers, :sequence, 0 )
        sequence >= from && sequence <= to
      end )
    |> Enum.reverse
    |> Enum.each( fn( packet ) ->
        { event, _headers, _payload } = packet
        IO.puts "sending #{ event } to client..."
        send pid, { :packet, packet }
      end )
  end

end
