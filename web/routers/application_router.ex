defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    conn = conn.assign(:layout, "public")

    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn = conn.fetch([:cookies, :params, :session])

    # load the flash
    conn = conn.assign(:flash, get_session(conn, :flash))
    conn = delete_session(conn, :flash)

    conn
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/" do
    if logged_in?(conn) do
      redirect conn, to: "/home"
    else
      render conn, "index.html"
    end
  end

  get "/sign-up" do
    render conn, "sign-up.html"
  end

  post "/sign-up" do
    user = HashDict.new
    user = user
      |> HashDict.put(:email, conn.params["email"])
      |> HashDict.put(:password, conn.params["password"])
      |> HashDict.put(:first, conn.params["first-name"])
      |> HashDict.put(:last, conn.params["last-name"])

    case :gen_server.call( :users, { :create, user } ) do
      { :error } ->
        conn = put_session(conn, :flash, "Error when inserting user.")
        redirect conn, to: "/sign-up"
      { :ok, user_id } ->
        conn = put_session(conn, :user_id, user_id )
        IO.puts "created new user: #{ user_id }"
        redirect conn, to: "/home"
    end

  end

  get "/login" do
    render conn, "login.html"
  end

  post "/login" do
    email = conn.params["email"]
    password = conn.params["password"]
  
    case :gen_server.call( :users, { :authenticate, { email, password } } ) do
      { :error } ->
        conn = put_session(conn, :flash, "Incorrect login credentials.")
        redirect conn, to: "/login"
      { :ok, user_id } ->
        conn = put_session(conn, :user_id, user_id)
        redirect conn, to: "/home"
    end

  end

  get "/logout" do
    conn = delete_session( conn, :user_id )

    redirect conn, to: "/"
  end

  get "/home" do
    user_id = get_session(conn, :user_id)
    result = :gen_server.call( :users, { :find, user_id } )

    case result do
      { :error } ->
        conn = put_session( conn, :flash, "You need to be logged in to do that")
        redirect conn, to: "/sign-up"
      { :ok, user } ->
        conn = conn.assign( :user, user )
        render conn, "home.html"
    end
  end


  post "/whiteboards" do

    board_name = conn.params["board-name"]
    nick = conn.params["user"]

    whiteboard = :gen_server.call( :board_store, { :get_by_name, board_name } )

    IO.inspect whiteboard

    IO.puts "new user at #{ board_name } named #{ nick }"

    redirect conn, to: "/static/index.html?user=#{ nick }&board_name=#{ board_name }"
  end

  post "/whiteboards/:name" do
    :gen_server.call( :board_store, { :create, conn.params["name"] } )

    conn.resp 201, "created."
  end

  get "/whiteboards" do
    whiteboards = :gen_server.call( :board_store, :list )

    list = whiteboards
              |> Enum.map( fn(x) -> x.name end )
              |> Enum.join("\n")

    conn.resp 200, list
  end

  defp authenticate_user(conn) do
    session = get_session(conn)

    IO.inspect session
  end

  defp logged_in?(conn) do
    ! nil? get_session(conn, :user_id)
  end
end
