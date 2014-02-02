config :dynamo,
  # In production, modules are compiled up-front.
  compile_on_demand: false,
  reload_modules: false

config :server,
  port: 8888,
  acceptors: 100,
  max_connections: 10000,
  dispatch: [{ :_, [
    {"/websocket", WhiteboardServer.Websocket, [] },
    {:_, Dynamo.Cowboy.Handler, __MODULE__ }
  ] }]

# config :ssl,
#  port: 8889,
#  keyfile: "/var/www/key.pem",
#  certfile: "/var/www/cert.pem"
