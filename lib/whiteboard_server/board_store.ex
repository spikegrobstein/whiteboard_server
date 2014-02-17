defmodule WhiteboardServer.BoardStore do
  use GenServer.Behaviour

  defrecord Board, name: "", client_store: nil

  def start_link( whiteboards ) do
    :gen_server.start_link({:local, :board_store}, __MODULE__, whiteboards, [])
  end

  def init( whiteboards ) do
    { :ok, whiteboards }
  end

  def handle_call( { :create, name }, _from, whiteboards) do
    [new|whiteboards] = create( whiteboards, name )

    { :reply, new, [new|whiteboards] }
  end

  def handle_call( { :get_by_name, name }, _from, whiteboards ) do
    { :reply, get_by_name( whiteboards, name ), whiteboards }
  end

  def handle_call( { :create_or_get_by_name, name }, _from, whiteboards ) do
    [new|whiteboards] = create_or_get_by_name( whiteboards, name )
    { :reply, new, [new|whiteboards] }
  end

  def handle_call( :list, _from, whiteboards ) do
    { :reply, whiteboards, whiteboards }
  end

  def handle_cast( { :destroy, name }, whiteboards ) do
    { :noreply, destroy( whiteboards, name ) }
  end

  # create
  # get_by_name
  # destroy

  # creates new whiteboard with given name
  # returns list of whiteboards and whiteboard.
  defp create( whiteboards, name ) do
    { :ok, client_store } = :gen_server.start_link(WhiteboardServer.ClientStore, [], [])
    [ Board.new(name: name, client_store: client_store ) | whiteboards ]
  end

  defp get_by_name( whiteboards, name ) do
    Enum.find whiteboards, fn(x) ->
      x.name == name
    end
  end

  defp create_or_get_by_name( whiteboards, name ) do
    whiteboard = get_by_name( whiteboards, name )

    if whiteboard == nil do
      [whiteboard|tail] = create( whiteboards, name )
    end

    [whiteboard|whiteboards]
  end

  defp destroy( whiteboards, name ) do
    Enum.reject whiteboards, fn(x) ->
      x.name == name
    end
  end
end
