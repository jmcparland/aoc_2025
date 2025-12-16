defmodule D11.Node do
  use GenServer

  # Client API

  def start_link(name, children) do
    GenServer.start_link(__MODULE__, {name, children}, name: {:global, name})
  end

  def send_message(name, msg) do
    GenServer.cast({:global, name}, {:message, msg})
  end

  def report(name) do
    GenServer.call({:global, name}, :report)
  end

  def p2_trigger do
    D11.Node.send_message("svr", %{dac: false, fft: false})
  end

  # Server Callbacks

  @impl true
  def init({name, children}) do
    {:ok, %{name: name, children: children, count: 0, p2_count: 0}}
  end

  @impl true
  def handle_call(:report, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_cast({:message, %{dac: dac, fft: fft} = msg}, state) do
    new_msg =
      case state.name do
        "dac" -> %{msg | dac: true}
        "fft" -> %{msg | fft: true}
        _ -> msg
      end

    new_state =
      if state.name == "out" and dac and fft do
        IO.puts("Total: #{state.count + 1}, P2 Valid: #{state.p2_count + 1}")
        %{state | count: state.count + 1, p2_count: state.p2_count + 1}
      else
        %{state | count: state.count + 1}
      end

    Enum.each(state.children, fn child_name ->
      GenServer.cast({:global, child_name}, {:message, new_msg})
    end)

    {:noreply, new_state}
  end

  @impl true
  def handle_cast({:message, msg}, state) do
    if state.name == "out", do: IO.puts("Node #{state.name} received: #{inspect(msg)}")

    new_msg = [state.name | msg]

    Enum.each(state.children, fn child_name ->
      GenServer.cast({:global, child_name}, {:message, new_msg})
    end)

    # for part 2, the "out" node also counts messages containing both "dac" and "fft"
    p2_count =
      if state.name == "out" and
           Enum.any?(new_msg, &(&1 == "dac")) and
           Enum.any?(new_msg, &(&1 == "fft")) do
        state.p2_count + 1
      else
        state.p2_count
      end

    {:noreply, %{state | count: state.count + 1, p2_count: p2_count}}
  end
end

defmodule D11 do
  def read_input(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim/1)
    |> Stream.reject(&(&1 == ""))
    |> Enum.reduce(%{}, fn line, acc ->
      [node, children_str] = String.split(line, ": ", parts: 2)
      children = String.split(children_str, " ")
      Map.put(acc, node, children)
    end)
  end

  def start_network(node_map) do
    Enum.each(node_map, fn {name_str, children} ->
      D11.Node.start_link(name_str, children)
    end)

    D11.Node.start_link("out", [])
  end

  def restart_network(file_path) do
    # Stop all global processes
    :global.registered_names()
    |> Enum.each(fn name ->
      case :global.whereis_name(name) do
        pid when is_pid(pid) -> Process.exit(pid, :kill)
        _ -> :ok
      end
    end)

    # Wait a bit for cleanup
    Process.sleep(500)

    # Restart
    node_map = read_input(file_path)
    start_network(node_map)
  end

  def report do
    :global.registered_names()
    |> Map.new(fn name -> {name, D11.Node.report(name)} end)
  end
end
