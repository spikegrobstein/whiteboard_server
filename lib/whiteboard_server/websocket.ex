defmodule WhiteboardServer.Websocket do
  # Cowboy Websocket Handler resources:
  #
  #   * User guide: http://ninenines.eu/docs/en/cowboy/HEAD/guide/ws_handlers
  #   * Reference:  http://ninenines.eu/docs/en/cowboy/HEAD/manual/cowboy_websocket_handler
  #
  @behaviour :cowboy_websocket_handler
  require :cowboy_req
  require :gen_server

  def init({:tcp, :http}, _req, _opts), do: { :upgrade, :protocol, :cowboy_websocket }

  def websocket_init(_transport_name, req, _opts) do

    # read the user field from URL
    { user, _ } = :cowboy_req.qs_val("user", req, "anon")
    { board_key, _ } = :cowboy_req.qs_val("board_key", req)

    # find the whiteboard
    { _, whiteboard } = :gen_server.call( :board_store, { :create_or_get_by_key, board_key } )

    IO.puts "started whiteboard #{ inspect whiteboard }"

    # join board
    :gen_server.cast( whiteboard, { :add_user, { self, user } })

    { :ok, req, whiteboard }
  end

  def websocket_info({ :packet, packet }, req, whiteboard ) do
    { :reply, { :text, ws_response( packet ) }, req, whiteboard }
  end

  def websocket_info({ :message, message }, req, whiteboard ) do
    # IO.puts("websocket_info: #{ inspect message }")
    { :reply, { :text, message }, req, whiteboard }
  end

  def websocket_handle({ :text, message }, req, whiteboard) do
    { :ok, data } = JSON.decode( message )
    packet = parse_packet( data )

    route_packet( whiteboard, packet )

    { :ok, req, whiteboard }
  end

  def websocket_terminate(_reason, _req, whiteboard) do
    :gen_server.cast( whiteboard, { :del_user, self } )

    :ok
  end

  defp route_packet( whiteboard, { event, payload } ) do
    case event do
      "user_list" ->
        IO.puts "Received user_list request."

        # send the client the userlist
        user_list = :gen_server.call( whiteboard, :user_list )

        # stringify the pids
        user_list = Enum.map( user_list, fn({ pid, nick }) ->
          [ userId: inspect(pid), nick: nick ]
        end)

        send self, { :packet, { "user_list", {}, user_list } }

      "hello" ->
        IO.puts "Received hello."

        { key, counter, user_list } = :gen_server.call( whiteboard, :hello )

        user_list = Enum.map( user_list, fn({ pid, nick }) ->
          [ userId: inspect(pid), nick: nick ]
        end)

        send self, { :packet, { "hello", {}, [ key: key, userList: user_list, sequence: counter ] } }
      _ ->
        :gen_server.cast whiteboard, { :ingest_packet, self, { event, payload } }
    end
  end

  ## Util:

  # given an event and payload, wrap up something to send to the
  # cowboy websocket, with the data JSON-encoded
  defp ws_response( { event, headers, payload } ) do
    new_packet = [ event: event, headers: headers, payload: payload ]

    { :ok, json_packet } = JSON.encode( new_packet )

    json_packet
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


