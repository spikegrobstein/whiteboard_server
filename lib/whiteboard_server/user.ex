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

  def find( user_id ) when is_integer(user_id) do
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

  def find( user_id ) when is_bitstring(user_id) do
    { id, _ } = Integer.parse(user_id)

    find id
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
      select
        whiteboards.name,
        whiteboard_invites.key
      from whiteboard_invites
      join whiteboards on ( whiteboard_invites.board_id = whiteboards.id )
      where
        whiteboard_invites.user_id = $1
        and whiteboards.active
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

  # create a whiteboard for the given user
  # then create an invite
  # return the board ID and the invite key.
  def create_whiteboard( user_id, whiteboard_name ) do
    sql = '''
      with new_whiteboard as (
        insert into whiteboards
        (
          created_at,
          user_id,
          name
        )
        values
        (
          now(),
          $1,
          $2
        )
      )
      insert into whiteboard_invites
      (
        created_at, key, user_id, board_id
      )
      values
      (
       now(),
       md5( $3 || $4 || now() ),
       $5,
       ( select id from new_whiteboard )
      )
    '''

    result = query( sql, [ user_id, whiteboard_name, user_id, whiteboard_name, user_id ] )

    IO.puts "Ran query: #{ inspect result }"
    case result do
      { :error, error } ->
        IO.puts "Error creating whiteboard: #{ inspect error }"
        { :error }
      { :ok, 1, _columns, [ {board_id, key} ] } ->
        IO.puts "Created new whiteboard: #{ board_id } / #{ key }"
        :gen_server.call( :board_store, { :create, board_id } )
        { :ok, board_id, key }
    end

  end

  # create a whiteboard invite for the given board_id for the given user
  # return the invite key
  def create_whiteboard_invite( board_id, user_id ) do
    sql = '''
      insert into whiteboard_invites
      (
        created_at, key, user_id, board_id
      )
      values
      (
        now(),
        md5( $1 || $2 || now() ),
        $3,
        $4
      )
      returning key
    '''

    result = query( sql, [ user_id, board_id, user_id, board_id ] )

    case result do
      { :error, error } ->
        IO.puts "Error creating invite: #{ inspect error }"
        { :error }
      { :ok, 1, _columns, [ { key } ] } ->
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
