defmodule WhiteboardServer.Users do
  use GenServer.Behaviour

  def start_link( config ) do
    :gen_server.start_link({:local, :users}, __MODULE__, config, [])
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

  def handle_call( { :create, user }, _from, client ) do
    { :reply, create_user(client, user), client }
  end

  def handle_call( { :find, user_id }, _from, client ) do
    { :reply, find_user(client, user_id), client }
  end

  def handle_call( { :find_by_email, email }, _from, client ) do
    { :reply, nil, client }
  end

  def handle_call( { :authenticate, { email, password } }, _from, client ) do
    { :reply, authenticate(client, email, password), client }
  end

  def handle_call( { :boards_for_user, user_id }, _from, client ) do
    { :reply, nil, client }
  end

  # given a user map, create the user
  # returns { :ok, user_id } on success or { :error } on error
  defp create_user( client, user ) do
    sql = '''
      insert into users (created_at, email, password_hash, password_salt)
      values
      ( now(), $1, md5( $2 || $3 ), $4 )
      returning id
    '''

    IO.puts "password: #{ inspect HashDict.get(user, :password) }"

    result = :pgsql.equery(
      client,
      sql,
      [
        to_char_list( HashDict.get(user, :email) ),
        to_char_list( HashDict.get(user, :password) ),
        'salt',
        'salt'
      ]
    )

    IO.puts "got back user: #{ inspect result }"

    case result do
      { :error, e } ->
        IO.puts "Got error: #{ inspect e }"
        { :error }
      { :ok, _count, _columns, rows } ->
        IO.puts "created user. yay!"
        { user_id } = hd(rows)
        { :ok, user_id }
    end
  end

  defp find_user( client, user_id ) do

    IO.puts "Finding user: #{ inspect user_id }"

    sql = '''
      select email, first, last from users where id = $1
    '''

    result = :pgsql.equery(
      client,
      sql,
      [user_id]
    )

    IO.puts "User found: #{ inspect result }"

    case result do
      { :error, _ } ->
        { :error }
      { :ok, _columns, [{ email, first, last }]  } ->
        { :ok, [email: email, first: first, last: last] }
    end
  end

  defp authenticate( client, email, password ) do
    sql = '''
      select id from users where email = $1 and password_hash = md5( $2 || password_salt )
    '''

    result = :pgsql.equery(
      client,
      sql,
      [
        to_char_list(email),
        to_char_list(password)
      ]
    )

    IO.puts "User found: #{ inspect result }"

    case result do
      { :error, _ } ->
        { :error }
      { :ok, _columns, [] } ->
        { :error }
      { :ok, _columns, [{user_id}] } ->
        { :ok, user_id }
    end

  end

end
