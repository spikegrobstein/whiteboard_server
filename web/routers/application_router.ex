defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn.fetch([:cookies, :params])
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/" do
    conn = conn.assign(:title, "Welcome to Dynamo!")
    render conn, "index.html"
  end

  post "/whiteboards" do

    whiteboard_name = conn.params["whiteboard-name"]
    nick = conn.params["user"]

    IO.puts "new user at #{ whiteboard_name } named #{ nick }"


    conn.resp 200, "connected."
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

end
