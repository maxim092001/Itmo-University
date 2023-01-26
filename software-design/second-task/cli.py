from stats import *
import os
import json

if __name__ == "__main__":
    with open("./configs/client_config.json") as config:
        config = json.loads(config.read())

        os.environ["VK_CLIENT_URL"] = config["url"]
        os.environ["VK_CLIENT_ACCESS_TOKEN"] = config["api_key"]
        os.environ["VK_CLIENT_API_VERSION"] = config["api_version"]

    stats_service = StatisticsService()

    while True:
        print("Enter the query or :q to exit")
        query = input()

        if query == ":q":
            break
        print("Enter integer number of hours between 1 and 24")

        hours = int(input())

        if not 1 <= hours <= 24:
            print("Invalid number of hours: {}".format(hours))
            continue

        distr = stats_service.get_posts(query, hours)
        print(distr)
