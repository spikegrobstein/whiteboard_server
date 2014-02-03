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

    { user, _ } = :cowboy_req.qs_val("user", req, "anon")

    :gen_server.cast( :client_store, { :add_client, { self, user } })

    { :ok, req, :no_state }
  end

  def websocket_info({ :message, message }, req, state) do
    # IO.puts("websocket_info: #{ inspect message }")
    { :reply, { :text, message }, req, state }
  end

  def websocket_handle({ :text, message }, req, state) do
    { :ok, data } = JSON.decode( message )

    # TODO: include the nick or something, rather than an inspected pid
    data = HashDict.put_new( data, :user, inspect(self) )

    :gen_server.cast( :client_store, { :broadcast, data } )
    # IO.puts "Got: #{ inspect data }"

    { :reply, { :text, message }, req, state }
  end

  def websocket_terminate(_reason, _req, _state) do
    :gen_server.cast( :client_store, { :del_client, self } )

    :ok
  end

end


