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

  # add a new client
  # new client is a tuple that contains
  # client atom
  # new_pid => the pid of the client
  # nick => a nickname
  # prevents duplicate pids
  defp add_client( clients, { new_pid, nick } ) do

    # broadcast that a user joined.
    broadcast( clients, { :user_join, nick } )

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
end
