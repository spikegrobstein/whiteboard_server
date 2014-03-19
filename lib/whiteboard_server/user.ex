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
      select email, first, last from users where id = $1
    '''

    result = query( sql, [ user_id ] )

    case result do
      { :error, _ } ->
        { :error }
      { :ok, _columns, [{ email, first, last }]  } ->
        { :ok, [email: email, first: first, last: last] }
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
      { :ok, _columns, [{ email, first, last }]  } ->
        { :ok, [email: email, first: first, last: last] }
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
