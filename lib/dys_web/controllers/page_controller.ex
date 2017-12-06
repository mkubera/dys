defmodule DysWeb.PageController do
  use DysWeb, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
