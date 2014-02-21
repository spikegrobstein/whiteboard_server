defmodule WhiteboardServer.BoardStore do
  use GenServer.Behaviour

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
    { :ok, board } = :gen_server.start_link(WhiteboardServer.Board, name, [])
    [ { name, board } | whiteboards ]
  end

  defp get_by_name( whiteboards, target_name ) do
    Enum.find whiteboards, fn({ name, _board }) ->
      target_name == name
    end
  end

  defp create_or_get_by_name( whiteboards, name ) do
    whiteboard = get_by_name( whiteboards, name )

    if whiteboard == nil do
      [whiteboard|_tail] = create( whiteboards, name )
    end

    [whiteboard|whiteboards]
  end

  defp destroy( whiteboards, target_name ) do
    Enum.reject whiteboards, fn({ name, _, _, _ }) ->
      target_name == name
    end
  end
end
