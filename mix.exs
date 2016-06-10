defmodule Nlocks.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :nlocks,
     version: @version,
     description: "Native spinlocks for Erlang",
     compilers: [:make, :erlang, :app],
     aliases: aliases,
     package: package]
  end

  defp aliases do
    [clean: ["clean", "clean.make"]]
  end

  defp package do
    [files: ~w(cpp_src priv src rebar.config README.md LICENSE),
     contributors: ["Guilherme Andrade"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/g-andrade/nlocks"}]
  end
end

# based on https://spin.atomicobject.com/2015/03/16/elixir-native-interoperability-ports-vs-nifs/
defmodule Mix.Tasks.Compile.Make do
  @shortdoc "Compiles helper in cpp_src"
  def run(_) do
    {result, _error_code} = System.cmd("make", ['-C', 'cpp_src/'], stderr_to_stdout: true)
    Mix.shell.info result
    :ok
  end
end

defmodule Mix.Tasks.Clean.Make do
  @shortdoc "Cleans helper in c_src"
  def run(_) do
    {result, _error_code} = System.cmd("make", ['-C', 'cpp_src/', 'clean'], stderr_to_stdout: true)
    Mix.shell.info result
    :ok
  end
end
