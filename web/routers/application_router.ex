defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn.fetch([:cookies, :params, :session])
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/" do
    authenticate_user conn

    render conn, "index.html"
  end

  get "/sign-up" do
    IO.puts "getting new user"

    render conn, "sign-up.html"
  end

  post "/sign-up" do
    IO.puts "signing up #{ conn.params["email"] }"

    redirect conn, to: "/welcome"
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

end
