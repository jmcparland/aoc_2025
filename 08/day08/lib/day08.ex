defmodule Day08 do
  @test_data "../test"
  @input_data "../input"

  def read_input(path) do
    File.read!(path)
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)
      |> List.to_tuple()
    end)
    |> Enum.sort()
  end

  def euclidean_distance({x1, y1, z1}, {x2, y2, z2}) do
    :math.sqrt(:math.pow(x1 - x2, 2) + :math.pow(y1 - y2, 2) + :math.pow(z1 - z2, 2))
  end

  def group_distances(data_list) do
    [rep | points] = data_list

    Enum.map(points, fn point ->
      {euclidean_distance(rep, point), {rep, point}}
    end)
  end

  def process_all(data_list) do
    do_process_all(data_list, [])
    |> Enum.sort()
  end

  defp do_process_all([], acc), do: acc

  defp do_process_all([rep | points], acc) do
    distances =
      Enum.map(points, fn point ->
        {euclidean_distance(rep, point), {rep, point}}
      end)

    do_process_all(points, distances ++ acc)
  end

  def to_clusters(distances) do
    # Build clusters with a point->cluster_id mapping
    {clusters, _point_to_cluster} =
      Enum.reduce(distances, {%{}, %{}}, fn {_distance, {point1, point2}},
                                            {clusters, point_to_cluster} ->
        cluster_id1 = Map.get(point_to_cluster, point1)
        cluster_id2 = Map.get(point_to_cluster, point2)

        cond do
          # Both points in different clusters - merge them
          cluster_id1 && cluster_id2 && cluster_id1 != cluster_id2 ->
            members1 = Map.fetch!(clusters, cluster_id1)
            members2 = Map.fetch!(clusters, cluster_id2)
            merged = MapSet.union(members1, members2)

            # Update all points from cluster2 to point to cluster1
            new_point_to_cluster =
              Enum.reduce(members2, point_to_cluster, fn point, acc ->
                Map.put(acc, point, cluster_id1)
              end)

            new_clusters =
              clusters
              |> Map.delete(cluster_id2)
              |> Map.put(cluster_id1, merged)

            {new_clusters, new_point_to_cluster}

          # point1 already in a cluster, add point2
          cluster_id1 ->
            members = Map.fetch!(clusters, cluster_id1)
            new_members = MapSet.put(members, point2)
            new_clusters = Map.put(clusters, cluster_id1, new_members)
            new_point_to_cluster = Map.put(point_to_cluster, point2, cluster_id1)
            {new_clusters, new_point_to_cluster}

          # point2 already in a cluster, add point1
          cluster_id2 ->
            members = Map.fetch!(clusters, cluster_id2)
            new_members = MapSet.put(members, point1)
            new_clusters = Map.put(clusters, cluster_id2, new_members)
            new_point_to_cluster = Map.put(point_to_cluster, point1, cluster_id2)
            {new_clusters, new_point_to_cluster}

          # Neither point in a cluster - create new cluster
          true ->
            new_cluster = MapSet.new([point1, point2])
            new_clusters = Map.put(clusters, point1, new_cluster)

            new_point_to_cluster =
              point_to_cluster
              |> Map.put(point1, point1)
              |> Map.put(point2, point1)

            {new_clusters, new_point_to_cluster}
        end
      end)

    clusters
  end

  def cluster_lengths(cluster_map) do
    cluster_map
    |> Map.values()
    |> Enum.map(&MapSet.size/1)
    |> Enum.sort(:desc)
  end

  def part1() do
    read_input(@input_data)
    |> process_all()
    |> Enum.take(1000)
    |> to_clusters()
    |> cluster_lengths()
    |> Enum.take(3)
    |> Enum.product()
  end

  # iex(4)> :timer.tc(Day08, :part1, [])
  # {64849921, _}
  # iex(5)>

  # iex(95)> :timer.tc(Day08, :part1, [])
  # {464274, _}

  def add_until(remaining_points, point_set, consumed_point_list) do
    cond do
      MapSet.size(point_set) == 1000 ->
        consumed_point_list

      remaining_points == [] ->
        {consumed_point_list, point_set}

      true ->
        [point | remaining_points] = remaining_points
        consumed_point_list = [point | consumed_point_list]
        {_distance, {point1, point2}} = point
        point_set = MapSet.put(point_set, point1) |> MapSet.put(point2)
        add_until(remaining_points, point_set, consumed_point_list)
    end
  end

  def part2() do
    {_distance, {{x1, _y1, _z1}, {x2, _y2, _z2}}} =
      read_input(@input_data)
      |> process_all()
      |> add_until(MapSet.new(), [])
      |> hd()

    x1 * x2
  end

  # iex(46)> :timer.tc(Day08, :part2, [])
  # {89500901, _}
  # iex(47)>

  # iex(92)> :timer.tc(Day08, :part2, [])
  # {418547, _}
  # iex(93)>
end
