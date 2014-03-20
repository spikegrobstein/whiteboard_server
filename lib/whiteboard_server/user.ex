defmodule WhiteboardServer.User do

  def create( user_dict ) do
    sql = '''
      insert into users
      (
        created_at,
        first,
        last,
        email,
        password_hash,
        password_salt
      )
      values
      (
        now(),
        $1,
        $2,
        $3,
        md5( $4 || $5 ),
        $6
      )
      returning id
    '''

    result = query(
      sql,
      [
        HashDict.get(user_dict, :first, ""),
        HashDict.get(user_dict, :last, ""),
        HashDict.get(user_dict, :email, ""),
        HashDict.get(user_dict, :password, ""),
        "salt",
        "salt"
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

  def find( nil ) do
    { :error }
  end

  def find( user_id ) do
    sql = '''
      select id, email, first, last from users where id = $1
    '''

    result = query( sql, [ user_id ] )

    case result do
      { :error, _ } ->
        { :error }
      { :ok, _columns, [{ id, email, first, last }]  } ->
        { :ok, [id: id, email: email, first: first, last: last] }
    end
  end

  def find_by_email( email ) do
    sql = '''
      select email, first, last from users where email = $1
    '''

    result = query( sql, [ email ] )

    case result do
      { :error, _ } ->
        { :error }
      { :ok, _columns, [{ id, email, first, last }]  } ->
        { :ok, [id: id, email: email, first: first, last: last] }
    end
  end

  def whiteboards( user_id ) do
    sql = '''
      select name, key from whiteboards where user_id = $1 and active
    '''

    result = query( sql, [ user_id ] )

    case result do
      { :error, error } ->
        IO.puts "Error fetching whiteboards: #{ inspect error }"
        { :error }
      { :ok, _columns, whiteboard_list } ->
        Enum.map whiteboard_list, fn({name, key}) ->
          [ name: name, key: key ]
        end
    end
  end

  def create_whiteboard( user_id, name ) do
    sql = '''
      insert into whiteboards
      (
        created_at,
        user_id,
        name,
        key
      )
      values
      (
        now(),
        $1,
        $2,
        md5( $3 || $4 || now() )
      )
      returning key
    '''

    result = query( sql, [ user_id, name, user_id, name ] )

    case result do
      { :error, error } ->
        IO.puts "Error creating whiteboard: #{ inspect error }"
        { :error }
      { :ok, 1, _columns, [ {key} ] } ->
        :gen_server.call( :board_store, { :create, key } )
        { :ok, key }
    end

  end

  def authenticate( email, password ) do
    sql = '''
      select id from users where email = $1 and password_hash = md5( $2 || password_salt )
    '''

    result = query( sql, [ email, password ] )

    case result do
      { :error, _ } ->
        { :error }
      { :ok, _columns, [] } ->
        { :not_found }
      { :ok, _columns, [{user_id}] } ->
        { :ok, user_id }
    end

  end

  defp query( sql, params ) do
    :gen_server.call( :database, { :equery, sql, params } )
  end


end
