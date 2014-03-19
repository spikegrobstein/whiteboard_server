defmodule WhiteboardServer.BoardStore do
  use GenServer.Behaviour

  def start_link( whiteboards ) do
    :gen_server.start_link({:local, :board_store}, __MODULE__, whiteboards, [])
  end

  def init( whiteboards ) do
    { :ok, whiteboards }
  end

  def handle_call( { :create, key }, _from, whiteboards) do
    [new|whiteboards] = create( whiteboards, key )

    { :reply, new, [new|whiteboards] }
  end

  def handle_call( { :get_by_key, key }, _from, whiteboards ) do
    { :reply, get_by_key( whiteboards, key ), whiteboards }
  end

  def handle_call( { :create_or_get_by_key, key }, _from, whiteboards ) do
    [new|whiteboards] = create_or_get_by_key( whiteboards, key )
    { :reply, new, [new|whiteboards] }
  end

  def handle_call( :list, _from, whiteboards ) do
    { :reply, whiteboards, whiteboards }
  end

  def handle_cast( { :destroy, key }, whiteboards ) do
    { :noreply, destroy( whiteboards, key ) }
  end

  # create
  # get_by_key
  # destroy

  # creates new whiteboard with given key
  # returns list of whiteboards and whiteboard.
  defp create( whiteboards, key ) do
    { :ok, board } = :gen_server.start_link(WhiteboardServer.Board, key, [])
    [ { key, board } | whiteboards ]
  end

  defp get_by_key( whiteboards, target_key ) do
    Enum.find whiteboards, fn({ key, _board }) ->
      target_key == key
    end
  end

  defp create_or_get_by_key( whiteboards, key ) do
    whiteboard = get_by_key( whiteboards, key )

    if whiteboard == nil do
      [whiteboard|_tail] = create( whiteboards, key )
    end

    [whiteboard|whiteboards]
  end

  defp destroy( whiteboards, target_key ) do
    Enum.reject whiteboards, fn({ key, _, _, _ }) ->
      target_key == key
    end
  end
end
