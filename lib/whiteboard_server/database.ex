defmodule WhiteboardServer.Database do
  use GenServer.Behaviour

  def start_link( config ) do
    :gen_server.start_link({:local, :database}, __MODULE__, config, [])
  end

  def init( config ) do
    { :ok, client } = :pgsql.connect(
      'localhost',
      'spike',
      'spike',
      [{ :database, 'whiteboard_dev' }]
    )

    { :ok, client }
  end

  def handle_call( { :equery, sql, params }, _from, client ) do
    params = process_params( params )

    IO.puts "Running query: #{ sql } => #{ inspect params }"

    {
      :reply,
      :pgsql.equery( client, sql, params ),
      client
    }
  end

  # convert double-quoted strings to single quoted for sequel query.
  defp process_params( params ) do
    Enum.map params, fn(p) ->
      if is_bitstring(p) do
        to_char_list(p)
      else
        p
      end
    end
  end

end
