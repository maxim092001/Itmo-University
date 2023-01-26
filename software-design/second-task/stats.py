from client import *
import utils
import time


class StatisticsService:
    def __init__(self) -> None:
        self.client = VkClient()

    def get_posts(self, query: str, hours: int):

        if not 1 <= hours <= 24:
            raise ValueError("Hours {} has to be between 1 and 24".format(hours))

        query = "#{}".format(query.strip())
        payload = {"q": query, "count": 0}
        intervals = utils.get_hour_intervals(hours=hours)

        result = []

        for start_time, end_time in intervals:
            time.sleep(1)

            payload["start_time"] = start_time
            payload["end_time"] = end_time

            response = self.client.search_feed(payload)
            result.append(response["total_count"])

        return result
