defmodule sterms.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :sterms,
     version: @version,
     description: "Native spinlocks for Erlang"
     package: package]
  end

  defp package do
    [files: ~w(src rebar.config README.md LICENSE),
     contributors: ["Guilherme Andrade"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/g-andrade/nlocks"}]
  end
end
