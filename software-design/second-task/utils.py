import time

HOUR_SECONDS = 3600


def get_hour_intervals(hours: int) -> list[tuple[int, int]]:
    end = int(time.time())
    start = end - hours * HOUR_SECONDS

    interval_start_points = list(range(start, end + HOUR_SECONDS, HOUR_SECONDS))

    interval_end_points = interval_start_points[1:]

    return list(zip(interval_start_points, interval_end_points))
