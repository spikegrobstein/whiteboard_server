defmodule WhiteboardServer.Mixfile do
  use Mix.Project

  def project do
    [ app: :whiteboard_server,
      version: "0.0.1",
      build_per_environment: true,
      dynamos: [WhiteboardServer.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:cowboy, :dynamo],
      mod: { WhiteboardServer, [] } ]
  end

  defp deps do
    [ { :cowboy, github: "extend/cowboy" },
      { :derp, github: 'meh/derp' },
      { :json,   github: "cblage/elixir-json", tag: "v0.2.7" },
      { :dynamo, github: "elixir-lang/dynamo", tag: 'elixir-0.12.2' } ]
  end
end
