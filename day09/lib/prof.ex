defmodule Mix.Tasks.Profile do
  use Mix.Task
  import ExProf.Macro

  @impl Mix.Task
  def run(_args) do
    profile do: run_game()
  end

  def run_game do
    Day09.run_game(100, 10000)
  end
end
