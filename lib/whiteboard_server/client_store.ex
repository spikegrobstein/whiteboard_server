require IEx

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

  def handle_call( :get_nick, from, clients ) do
    { :reply, client_for_pid(clients, from), clients }
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

  defp client_for_pid( clients, pid ) do
    result = Enum.find( clients, fn(x) ->
      { ^pid, nick } = x
    end)

    { pid, nick } = result
    nick
  end

  def handle_packet( clients, pid, packet ) do
    { event, payload } = parse_packet( packet )

    case event do
      "draw" ->
        IO.puts "got draw event"
      "user_list" ->
        IO.inspect clients
        send_user_list( pid, clients )
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

    # broadcast that a user joined.
    broadcast( clients, { :user_join, nick } )

    # send the client the user_list
    # send new_pid, make_packet( "user_list", clients )

    if Enum.any?( clients, fn({ pid, _nick }) -> pid == new_pid end ) do
      clients
    else
      [{ new_pid, nick }|clients]
    end
  end

  # delete a client based on the pid
  defp del_client( clients, pid_to_delete ) do
    IO.puts "removing client: #{ inspect pid_to_delete }"
    Enum.reject( clients, fn({ pid, _nick}) -> pid_to_delete == pid end )
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
    user_list = Enum.map( clients, fn({ _pid, nick }) -> nick end)
    send pid, make_packet( "user_list2", user_list )
  end

  defp send_unknown_packet_error( pid, event, payload ) do
    send pid, make_packet( "unknown_packet", [ event: event, payload: payload ] )
  end

end
