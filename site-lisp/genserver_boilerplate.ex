  use GenServer

  def start_link(opts \\ []) do
    opts = Map.new(opts)
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl GenServer
  def init(opts) do
    {:ok, opts}
  end

  @impl GenServer
  def handle_call(:c, _from, state) do
    {:reply, :ok, state}
  end

  @impl GenServer
  def handle_cast(:c, state) do
    {:noreply, state}
  end

  @impl GenServer
  def handle_info(:c, state) do
    {:noreply, state}
  end
