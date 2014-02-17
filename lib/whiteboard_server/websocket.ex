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
    { board_name, _ } = :cowboy_req.qs_val("board_name", req)

    # find the whiteboard
    whiteboard = :gen_server.call( :board_store, { :create_or_get_by_name, board_name } )

    # join board
    :gen_server.cast( whiteboard.clients, { :add_client, { self, user } })

    { :ok, req, whiteboard }
  end

  def websocket_info({ :message, message }, req, whiteboard ) do
    # IO.puts("websocket_info: #{ inspect message }")
    { :reply, { :text, message }, req, whiteboard }
  end

  def websocket_handle({ :text, message }, req, whiteboard) do
    { :ok, data } = JSON.decode( message )

    :gen_server.cast( whiteboard.clients, { :handle_packet, self, data } )

    # :gen_server.cast( :client_store, { :broadcast, data } )

    { :ok, req, whiteboard }
  end

  def websocket_terminate(_reason, _req, whiteboard) do
    :gen_server.cast( whiteboard.clients, { :del_client, self } )

    :ok
  end

end


