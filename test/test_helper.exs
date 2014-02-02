Dynamo.under_test(WhiteboardServer.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule WhiteboardServer.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end
