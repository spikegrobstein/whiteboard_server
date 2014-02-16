defmodule WhiteboardServer.Supervisor do
  use Supervisor.Behaviour

  def start_link( client_list ) do
    :supervisor.start_link(__MODULE__, client_list )
  end

  def init( client_list ) do
    children = [
      worker(WhiteboardServer.BoardStore, [client_list]),
      supervisor(WhiteboardServer.Dynamo, [])
    ]

    supervise children, strategy: :one_for_one
  end
end
