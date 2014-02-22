defmodule WhiteboardServer.Board do
  use GenServer.Behaviour

  def start_link( name ) do
    :gen_server.start_link(__MODULE__, name, [])
  end

  # the state of a board is a tuple containing:
  # {
  #   name,
  #   counter,
  #   clients,
  #   data
  # }

  def init( name ) do
    IO.puts "created board with #{ name } (#{ inspect self })"
    { :ok, { name, 0, [], [] } }
  end

  ## event stuff
  @doc """
    receive a new packet from a client

    route it to the correct private function
  """
  def handle_cast( { :ingest_packet, pid, { event, payload } }, { name, counter, clients, data } ) do
    case event do
      "draw" ->
        # they sent a draw event, let's process it
        # IO.inspect(payload)
        { counter, data } = ingest_draw( counter, clients, data, pid, payload )

      "pen_up" ->
        # received when a client stops drawing
        # this is broadcast to all clients that the user
        # stopped so they can clear local pen values for that user
        # payload is ignored
        IO.puts "pen_up"
        broadcast clients, "pen_up", [ userId: inspect(pid) ]

      "console" ->
        IO.inspect(payload)

      _ ->
        IO.puts "got something else: #{ event }"
        send_unknown_packet_error( pid, { event, payload } )
    end

    { :noreply, { name, counter, clients, data } }
  end

  ## client stuff:

  @doc """
    add the given user to the user list
    broadcast to everyone that this user joined
  """
  def handle_cast( { :add_user, { pid, nick } }, { name, counter, clients, data } ) do
    IO.puts "add user: #{ inspect pid } - #{ nick }"
    broadcast clients, "user_join", { pid, nick }

    clients = add_client( clients, { pid, nick } )

    { :noreply, { name, counter, clients, data } }
  end

  @doc """
    the given user has left
    broadcast to everyone that they left
  """
  def handle_cast( { :del_user, pid }, { name, counter, clients, data } ) do
    broadcast clients, "user_leave", { pid }

    clients = del_client( clients, pid )

    { :noreply, { name, counter, clients, data } }
  end

  @doc """
    return the user list
  """
  def handle_call( :user_list, _from, state ) do
    { _, _, clients, _ } = state

    { :reply, clients, state }
  end

  ## implementation functions

  defp add_client( clients, { pid, nick } ) do
    [{ pid, nick }|clients]
  end

  defp del_client( clients, pid_to_delete ) do
    Enum.reject clients, fn({ pid, _nick }) ->
      pid_to_delete == pid
    end
  end

  ## util

  defp broadcast( clients, event, payload ) do
    Enum.each clients, fn({ pid, _nick }) ->
      send pid, { :packet, { event, payload } }
    end
  end

  defp send_unknown_packet_error( pid, packet ) do
    send pid, { :packet, { "unknown_packet", packet } }
  end

  # process a draw event, broadcast to clients, return {counter, data}
  defp ingest_draw( counter, clients, data, pid, payload ) do
    IO.inspect { :ingest_draw, counter, payload }
    counter = counter + 1

    payload = HashDict.put(payload, :userId, inspect(pid))
    payload = HashDict.put(payload, :sequence, counter)

    broadcast clients, "draw", payload

    { counter, Enum.concat(data, payload) }
  end


end
